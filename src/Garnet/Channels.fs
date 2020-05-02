namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Text
open Garnet
open Garnet.Comparisons
open Garnet.Formatting
open Garnet.Metrics

type Start = struct end    
type Stop = struct end    

/// Event published when commit occurs    
type Commit = struct end

[<Struct>]
type Update = {
    currentTime : int64
    deltaTime : int64
    }

type internal EventHandler<'a> = Memory<'a> -> unit

type IPublisher =
    abstract member PublishAll<'a> : Memory<'a> * Memory<EventHandler<'a>> -> unit

type internal IChannel =
    abstract member Clear : unit -> unit
    abstract member Commit : unit -> unit
    abstract member Publish : unit -> bool
    abstract member SetPublisher : IPublisher voption -> unit

module internal Publisher =
    let formatBatch (messages : Span<_>) =
        let sb = System.Text.StringBuilder()
        let count = min 20 messages.Length
        for i = 0 to count - 1 do 
            let msg = messages.[i]
            sb.AppendLine().Append(sprintf "%A" msg) |> ignore
        let remaining = messages.Length - count
        if remaining > 0 then
            sb.AppendLine().Append(sprintf "(+%d)" remaining) |> ignore
        sb.ToString()

    let publishAll<'a>(batch : Memory<'a>) (handlers : Span<_>) =
        for i = 0 to handlers.Length - 1 do
            let handler = handlers.[i]
            try
                handler batch
            with
            | ex -> 
                let str = 
                    sprintf "Error in handler %d on %s batch (%d):%s" 
                        i (typeof<'a> |> typeToString) batch.Length (formatBatch batch.Span)
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
    member c.SetPublisher p =
        publisher <- p
    member c.PublishAll(batch : Memory<'a>) =
        match publisher with 
        | ValueNone -> Publisher.publishAll batch handlers.Buffer.Span
        | ValueSome publisher -> publisher.PublishAll(batch, handlers.Buffer)
    /// Dispatches event immediately/synchronously
    member c.Publish event =
        stack.Add event
        try
            let mem = Memory(stack.Array, stack.Count - 1, 1)
            c.PublishAll mem
        finally
            stack.RemoveLast()
    member c.Send(event) =
        pending.Add(event)
        total <- total + 1
    member c.SendAll(events : Memory<_>) =
        pending.AddAll(events.Span)
        total <- total + events.Length
    member c.OnAll(handler : EventHandler<_>) =
        handlers.Add(handler)
        Disposable.init (fun () -> unsubscribed.Add(handler))
    /// Calls handler behaviors and prunes subscriptions after
    member c.Publish() =
        if events.Count = 0 then false
        else
            c.PublishAll events.Buffer
            true
    /// Commit pending events to publish list and resets
    member c.Commit() =
        if unsubscribed.Count > 0 then
            let mutable i = 0
            while i < handlers.Count do
                // only remove one occurrence
                if unsubscribed.Remove handlers.[i] then
                    // note ordering changes, but subscribers of the same
                    // event type should not have any ordering dependencies
                    handlers.[i] <- handlers.[handlers.Count - 1]
                    handlers.RemoveLast()
                else i <- i + 1
            unsubscribed.Clear()
        // clear prior events and swap in pending to current
        events.Clear()
        let temp = pending
        pending <- events
        events <- temp
    interface IChannel with
        member c.Clear() = c.Clear()
        member c.Publish() = c.Publish()
        member c.Commit() = c.Commit()
        member c.SetPublisher p = c.SetPublisher p
    override c.ToString() =            
        sprintf "%s: %dH %dP %dE %dT %dSE" (typeof<'a> |> typeToString) 
            handlers.Count pending.Count events.Count total stack.Count

type IChannels =
    abstract member GetChannel<'a> : unit -> Channel<'a>

/// Supports reentrancy
type Channels() =
    let channels = List<IChannel>()
    let mutable lookup = Array.zeroCreate<IChannel>(8)
    let mutable publisher : IPublisher voption = ValueNone
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
            c.GetChannel<'a>().Send msg

        member c.OnAll<'a>(handler) =
            c.GetChannel<'a>().OnAll(handler)

        member c.Publish<'a>(event : 'a) =
            c.GetChannel<'a>().Publish event

        member c.OnAll<'a>() =
            c.GetChannel<'a>().OnAll

        member c.On<'a>(handler) =
            c.GetChannel<'a>().OnAll(
                fun batch -> 
                    let span = batch.Span
                    for i = 0 to span.Length - 1 do
                        handler span.[i])

    type Channel<'a> with    
        member c.Wait(msg) =
            c.Send(msg)
            Wait.defer

    type IChannels with    
        member c.Wait(msg) =
            c.GetChannel<'a>().Wait msg

type internal NullPublisher() =
    static let mutable instance = NullPublisher() :> IPublisher
    static member Instance = instance
    interface IPublisher with
        member c.PublishAll(batch, handlers) = ()

type PrintPublisherOptions = {
    enableLog : bool
    logLabel : string
    messageSizeLimit : int
    minDurationUsec : int
    sendLog : string -> unit
    sendTiming : Timing -> unit
    canSendLog : Type -> bool
    canSendTiming : Type -> bool
    basePublisher : IPublisher
    formatter : IFormatter
    }

/// Prints published events
type internal PrintPublisher(options) =
    let sb = StringBuilder()
    let mutable count = 0
    interface IPublisher with        
        member c.PublishAll<'a>(batch : Memory<'a>, handlers) =
            let start = Timing.getTimestamp()
            let mutable completed = false
            try
                options.basePublisher.PublishAll(batch, handlers)
                completed <- true
            finally
                let typeInfo = CachedTypeInfo<'a>.Info
                let canLog =
                    options.enableLog &&
                    options.canSendLog typeof<'a> && 
                    options.formatter.CanFormat<'a>()
                let canTime =
                    options.canSendTiming typeof<'a>
                // stop timer
                let stop = Timing.getTimestamp()
                // send timing
                if canTime then
                    options.sendTiming {
                        name = typeInfo.typeName
                        start = start
                        stop = stop
                        count = batch.Length 
                        }
                // send log message
                if canLog then
                    let duration = stop - start
                    let usec = duration * 1000L / Timing.ticksPerMs |> int
                    if not completed || usec >= options.minDurationUsec then
                        sb.Append(
                            sprintf "[%s] %d: %dx %s to %d handlers in %dus%s"
                                options.logLabel count batch.Length 
                                (typeof<'a> |> typeToString)
                                handlers.Length usec
                                (if completed then "" else " failed")
                            ) |> ignore
                        // print messages
                        if not typeInfo.isEmpty then
                            formatMessagesTo sb options.formatter.Format batch.Span options.messageSizeLimit
                        sb.AppendLine() |> ignore
                        options.sendLog (sb.ToString())
                        sb.Clear() |> ignore
                count <- count + 1
    
type Publisher() =
    static let mutable instance = Publisher() :> IPublisher
    static member Default = instance
    static member Null = NullPublisher.Instance
    static member Print options = PrintPublisher(options) :> IPublisher
    interface IPublisher with
        member c.PublishAll<'a>(batch : Memory<'a>, handlers : Memory<_>) =
            Publisher.publishAll batch handlers.Span

module PrintPublisherOptions =
    let enabled = {
        enableLog = true
        logLabel = ""
        messageSizeLimit = 10
        minDurationUsec = 0
        sendLog = printfn "%s"
        sendTiming = ignore
        canSendLog = fun t -> not (t.Equals(typeof<Commit>))
        canSendTiming = fun t -> not (t.Equals(typeof<Commit>))
        basePublisher = Publisher.Default
        formatter = Formatter()
        }
