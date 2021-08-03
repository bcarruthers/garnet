namespace Garnet.Composition

open System
open System.Buffers
open System.Collections.Generic
open System.Diagnostics
open System.Text
open Garnet
open Garnet.Comparisons
open Garnet.Formatting
open Garnet.Metrics

type internal EventHandler<'a> = ReadOnlyMemory<'a> -> unit

type IPublisher =
    abstract member PublishAll<'a> : ReadOnlyMemory<'a> * ReadOnlyMemory<EventHandler<'a>> -> unit

type internal IChannel =
    abstract member Clear : unit -> unit
    abstract member Commit : unit -> unit
    abstract member Publish : unit -> bool
    abstract member SetPublisher : IPublisher voption -> unit

module internal Publisher =
    let formatBatch (messages : ReadOnlySpan<_>) =
        let sb = StringBuilder()
        let count = min 20 messages.Length
        for i = 0 to count - 1 do 
            let msg = messages.[i]
            sb.AppendLine().Append(sprintf "%A" msg) |> ignore
        let remaining = messages.Length - count
        if remaining > 0 then
            sb.AppendLine().Append(sprintf "(+%d)" remaining) |> ignore
        sb.ToString()

    let publishAll<'a>(batch : ReadOnlyMemory<'a>) (handlers : ReadOnlySpan<_>) =
        for i = 0 to handlers.Length - 1 do
            let handler = handlers.[i]
            try
                handler batch
            with
            | ex -> 
                let str = 
                    sprintf "Error in handler %d on %s batch (%d):%s" 
                        i (typeof<'a> |> Format.typeToString) batch.Length (formatBatch batch.Span)
                exn(str, ex) |> raise                

type Channel<'a>() =
    let unsubscribed = List<EventHandler<'a>>()
    let handlers = ResizableBuffer<EventHandler<'a>>(8)
    let stack = ResizableBuffer<'a>(8)
    let mutable publisher : IPublisher voption = ValueNone
    let mutable events = ResizableBuffer<'a>(8)
    let mutable pending = ResizableBuffer<'a>(8)
    let mutable total = 0
    member c.Clear() =
        stack.Clear()
        pending.Clear()
    member c.SetPublisher(newPublisher) =
        publisher <- newPublisher
    member c.PublishAll(batch : ReadOnlyMemory<'a>) =
        match publisher with 
        | ValueNone -> Publisher.publishAll batch handlers.WrittenSpan
        | ValueSome publisher -> publisher.PublishAll(batch, handlers.WrittenMemory)
    /// Dispatches event immediately/synchronously
    member c.Publish(event) =
        stack.WriteValue(event)
        try
            let mem = stack.WrittenMemory.Slice(stack.WrittenCount - 1, 1)
            c.PublishAll(mem)
        finally
            stack.RemoveLast()
    member c.Send(event) =
        pending.WriteValue(event)
        total <- total + 1
    member c.SendAll(events : ReadOnlyMemory<_>) =
        pending.Write(events.Span)
        total <- total + events.Length
    member c.Advance(count) =
        pending.Advance(count)
        total <- total + count
    member c.GetMemory(sizeHint) =
        pending.GetMemory(sizeHint)
    member c.GetSpan(sizeHint) =
        pending.GetSpan(sizeHint)
    interface IBufferWriter<'a> with
        member c.Advance(count) = c.Advance(count)
        member c.GetMemory(sizeHint) = c.GetMemory(sizeHint)
        member c.GetSpan(sizeHint) = c.GetSpan(sizeHint)
    member c.OnAll(handler : EventHandler<_>) =
        handlers.WriteValue(handler)
        Disposable.Create(fun () -> unsubscribed.Add(handler))
    /// Calls handler behaviors and prunes subscriptions after
    member c.Publish() =
        if events.WrittenCount = 0 then false
        else
            c.PublishAll(events.WrittenMemory)
            true
    /// Commit pending events to publish list and resets
    member c.Commit() =
        if unsubscribed.Count > 0 then
            let mutable i = 0
            while i < handlers.WrittenCount do
                // only remove one occurrence
                if unsubscribed.Remove handlers.[i] then
                    // note ordering changes, but subscribers of the same
                    // event type should not have any ordering dependencies
                    handlers.[i] <- handlers.[handlers.WrittenCount - 1]
                    handlers.RemoveLast()
                else i <- i + 1
            unsubscribed.Clear()
        // clear prior events and swap in pending to current
        events.Clear()
        if pending.WrittenCount > 0 then
            let temp = pending
            pending <- events
            events <- temp
    interface IChannel with
        member c.Clear() = c.Clear()
        member c.Publish() = c.Publish()
        member c.Commit() = c.Commit()
        member c.SetPublisher(p) = c.SetPublisher(p)
    override c.ToString() =            
        sprintf "%s: %dH %dP %dE %dT %dSE" (typeof<'a> |> Format.typeToString) 
            handlers.WrittenCount pending.WrittenCount events.WrittenCount total stack.WrittenCount

type IChannels =
    abstract member GetChannel<'a> : unit -> Channel<'a>

/// Supports reentrancy
type Channels() =
    let channels = List<IChannel>()
    let mutable lookup = Array.zeroCreate<IChannel>(8)
    let mutable publisher : IPublisher voption = ValueNone
    member c.Count = channels.Count
    member c.Clear() =
        for channel in channels do
            channel.Clear()
    member c.Commit() =
        for channel in channels do
            channel.Commit()
    member c.SetPublisher(newPublisher) =
        publisher <- newPublisher
        for channel in channels do
            channel.SetPublisher newPublisher
    /// Returns true if any events were handled
    member c.Publish() =
        // to handle reentrancy, avoid foreach and iterate up to current count
        let mutable published = false
        let count = channels.Count
        for i = 0 to count - 1 do
            published <- channels.[i].Publish() || published
        published
    member c.GetChannel<'a>() =
        let id = MessageTypeId<'a>.Id
        if id >= lookup.Length then
            Buffer.resizeArray (id + 1) &lookup
        let channel = lookup.[id]
        if isNotNull channel then channel :?> Channel<'a>
        else            
            let channel = Channel<'a>()
            channel.SetPublisher publisher
            lookup.[id] <- channel :> IChannel
            channels.Add(channel)
            channel
    interface IChannels with
        member c.GetChannel<'a>() = c.GetChannel<'a>()
    override c.ToString() =
        let sb = StringBuilder()
        sb.Append("Channels") |> ignore
        let groups = 
            channels
            |> Seq.map (fun ch -> ch.GetType().GetGenericArguments().[0], ch)
            |> Seq.groupBy (fun (t, _) -> t.Namespace)
            |> Seq.sortBy (fun (key, _) -> key)
        for ns, group in groups do
            let name = if String.IsNullOrEmpty(ns) then "[None]" else ns
            sb.AppendLine().Append("  " + name) |> ignore
            let channels = 
                group 
                |> Seq.sortBy (fun (t, _) -> t.Name)
                |> Seq.map snd
            for channel in channels do
                sb.AppendLine().Append("    " + channel.ToString()) |> ignore
        sb.ToString()
                
[<AutoOpen>]
module Channels =
    type IChannels with    
        member c.GetSender<'a>() =
            c.GetChannel<'a>().Send

        member c.Send(msg) =
            c.GetChannel<'a>().Send(msg)

        member c.OnAll<'a> handler =
            c.GetChannel<'a>().OnAll(handler)

        member c.Publish<'a>(event : 'a) =
            c.GetChannel<'a>().Publish(event)

        member c.OnAll<'a>() =
            c.GetChannel<'a>().OnAll

        member c.On<'a> handle =
            c.GetChannel<'a>().OnAll(
                fun batch -> 
                    let span = batch.Span
                    for i = 0 to span.Length - 1 do
                        handle span.[i])

        /// Buffers incoming events in a second set of channels and subscribes
        /// handler to the buffered channel. THis is useful for holding events
        /// until a commit event is received or an appropriate update event occurs.
        member c.BufferOnAll<'a>(buffer : IChannels, handler) =
            let channel = buffer.GetChannel<'a>()
            Disposable.Create [
                // when first channels receive events, write to buffer
                // events will need to be published by a separate mechanism
                c.OnAll<'a> channel.SendAll                    
                // subscribe handler to buffer rather than original channels
                channel.OnAll(handler)
                ]
    
        /// Buffers incoming events in a second set of channels and subscribes
        /// handler to the buffered channel. THis is useful for holding events
        /// until a commit event is received or an appropriate update event occurs.
        member c.BufferOn<'a>(buffer : IChannels, handle) =
            c.BufferOnAll<'a>(buffer, fun mem ->
                for e in mem.Span do
                    handle e)

    type Channel<'a> with    
        member c.Wait(msg) =
            c.Send(msg)
            Wait.defer

    type IChannels with    
        member c.Wait(msg) =
            c.GetChannel<'a>().Wait(msg)

type internal NullPublisher() =
    static let mutable instance = NullPublisher() :> IPublisher
    static member Instance = instance
    interface IPublisher with
        member c.PublishAll(_, _) = ()

type PrintPublisherOptions = {
    EnableLog : bool
    LogLabel : string
    MessageSizeLimit : int
    MinDurationMicroseconds : int
    SendLog : string -> unit
    SendTiming : Timing -> unit
    CanSendLog : Type -> bool
    CanSendTiming : Type -> bool
    BasePublisher : IPublisher
    Formatter : IFormatter
    }

/// Prints published events
type internal PrintPublisher(options) =
    let sb = StringBuilder()
    let mutable count = 0
    interface IPublisher with        
        member c.PublishAll<'a>(batch : ReadOnlyMemory<'a>, handlers) =
            let start = Stopwatch.GetTimestamp()
            let mutable completed = false
            try
                options.BasePublisher.PublishAll(batch, handlers)
                completed <- true
            finally
                let typeInfo = Format.CachedTypeInfo<'a>.Info
                let canLog =
                    options.EnableLog &&
                    options.CanSendLog typeof<'a> && 
                    options.Formatter.CanFormat<'a>()
                let canTime =
                    options.CanSendTiming typeof<'a>
                // stop timer
                let stop = Stopwatch.GetTimestamp()
                // send timing
                if canTime then
                    options.SendTiming {
                        Name = typeInfo.typeName
                        Start = start
                        Stop = stop
                        Count = batch.Length 
                        }
                // send log message
                if canLog then
                    let duration = stop - start
                    let usec = duration * 1000L * 1000L / Stopwatch.Frequency |> int
                    if not completed || usec >= options.MinDurationMicroseconds then
                        sb.Append(
                            sprintf "[%s] %d: %dx %s to %d handlers in %dus%s"
                                options.LogLabel count batch.Length 
                                (typeof<'a> |> Format.typeToString)
                                handlers.Length usec
                                (if completed then "" else " failed")
                            ) |> ignore
                        // print messages
                        if not typeInfo.isEmpty then
                            Format.formatMessagesTo sb options.Formatter.Format batch.Span options.MessageSizeLimit
                        sb.AppendLine() |> ignore
                        options.SendLog (sb.ToString())
                        sb.Clear() |> ignore
                count <- count + 1
    
type Publisher() =
    static let mutable instance = Publisher() :> IPublisher
    static member Default = instance
    static member Null = NullPublisher.Instance
    static member Print options = PrintPublisher(options) :> IPublisher
    interface IPublisher with
        member c.PublishAll<'a>(batch : ReadOnlyMemory<'a>, handlers : ReadOnlyMemory<_>) =
            Publisher.publishAll batch handlers.Span

module PrintPublisherOptions =
    let enabled = {
        EnableLog = true
        LogLabel = ""
        MessageSizeLimit = 10
        MinDurationMicroseconds = 0
        SendLog = printfn "%s"
        SendTiming = ignore
        CanSendLog = fun _ -> true
        CanSendTiming = fun _ -> true
        BasePublisher = Publisher.Default
        Formatter = Formatter()
        }
