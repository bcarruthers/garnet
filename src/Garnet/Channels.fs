namespace Garnet.Ecs

open System
open System.Collections.Generic
open System.Text
open Garnet.Comparisons
open Garnet.Formatting
open Garnet.Metrics
open Garnet.Collections
    
type EventHandler<'a> = List<'a> -> unit

type IEventHandler =
    abstract member Handle<'a> : List<'a> -> unit

type Subscription<'a>(handler : EventHandler<'a>) =
    let mutable isDisposed = false
    let handler = handler
    member c.IsUnsubscribed = isDisposed
    member c.Handle batch = handler batch
    interface IDisposable with
        member c.Dispose() = isDisposed <- true

type internal Subscription(handler : IEventHandler) =
    let mutable isDisposed = false
    let handler = handler
    member c.IsUnsubscribed = isDisposed
    member c.Handle batch = handler.Handle batch
    interface IDisposable with
        member c.Dispose() = isDisposed <- true
    
type IDispatcher =
    abstract member Dispatch<'a> : List<'a> -> List<Subscription<'a>> -> unit

type internal IChannel =
    abstract member Clear : unit -> unit
    abstract member Commit : unit -> unit
    abstract member Publish : unit -> bool
            
type internal NullDispatcher() =
    interface IDispatcher with
        member c.Dispatch batch handlers = ()

type Dispatcher() =
    let formatBatch (messages : List<_>) =
        let sb = System.Text.StringBuilder()
        let count = min 20 messages.Count
        for i = 0 to count - 1 do 
            let msg = messages.[i]
            sb.AppendLine().Append(sprintf "%A" msg) |> ignore
        let remaining = messages.Count - count
        if remaining > 0 then
            sb.AppendLine().Append(sprintf "(+%d)" remaining) |> ignore
        sb.ToString()

    static member Default = Dispatcher() :> IDispatcher
    static member Null = NullDispatcher() :> IDispatcher

    interface IDispatcher with
        member c.Dispatch<'a> (batch : List<'a>) handlers =
            let count = handlers.Count
            for i = 0 to count - 1 do
                let handler = handlers.[i]
                try
                    handler.Handle batch
                with
                | ex -> 
                    let str = 
                        sprintf "Error in handler %d on %s batch (%d):%s" 
                            i (typeof<'a> |> typeToString) batch.Count (formatBatch batch)
                    exn(str, ex) |> raise                

type PrintDispatcherOptions = {
    isPrintEnabled : bool
    printLabel : string
    maxPrintMessages : int
    minDurationUsec : int
    sendTiming : Timing -> unit
    sendLogMessage : string -> unit
    }

module PrintDispatcherOptions =
    let defaultOptions = {
        isPrintEnabled = false
        printLabel = ""
        maxPrintMessages = 10
        minDurationUsec = 0
        sendTiming = ignore
        sendLogMessage = printfn "%s"
        }

/// Prints dispatched events
type PrintDispatcher(dispatcher : IDispatcher, formatter : IFormatter) =
    let sb = StringBuilder()
    let ticksPerSec = Timing.ticksPerMs / int64 1000
    let enabledTypes = HashSet<Type>()
    let mutable count = 0
    let mutable options = PrintDispatcherOptions.defaultOptions
    new() = PrintDispatcher(Dispatcher(), Formatter())
    member c.Options = options
    member c.SetOptions newOptions =
        options <- newOptions
    member c.Enable t =
        enabledTypes.Add t |> ignore
    member c.Disable t =
        enabledTypes.Remove t |> ignore
    interface IDispatcher with        
        member c.Dispatch<'a> (batch : List<'a>) handlers =
            let options = options
            let start = Timing.getTimestamp()
            let handlerCount = handlers.Count
            let mutable completed = false
            try
                dispatcher.Dispatch batch handlers
                completed <- true
            finally
                let stop = Timing.getTimestamp()
                let typeInfo = CachedTypeInfo<'a>.Info
                // send timing to accumulator
                if typeInfo.canSendTimings then
                    options.sendTiming {
                        name = typeInfo.typeName
                        start = start
                        stop = stop
                        count = batch.Count 
                        }
                // print immediate timing
                let canPrint = 
                    (options.isPrintEnabled || enabledTypes.Contains typeof<'a>) && 
                    formatter.CanFormat<'a>() && 
                    typeInfo.canPrint
                if canPrint then
                    let duration = stop - start
                    let usec = duration * 1000000L / ticksPerSec |> int
                    if not completed || usec >= options.minDurationUsec then
                        sb.Append(
                            sprintf "[%s] %d: %s %dmsg %dh %dus%s"
                                options.printLabel count (typeof<'a> |> typeToString)
                                batch.Count handlerCount usec
                                (if completed then "" else " FAILED")
                            ) |> ignore
                        // print messages
                        if not typeInfo.isEmpty then //if typeInfo.canPrint then
                            formatMessagesTo sb formatter.Format batch options.maxPrintMessages
                        sb.AppendLine() |> ignore
                        options.sendLogMessage (sb.ToString())
                        sb.Clear() |> ignore
                count <- count + 1

type Channel<'a>(dispatcher : IDispatcher) =
    let isUnsubscribed = Predicate(fun (sub : Subscription<'a>) -> sub.IsUnsubscribed)
    let handlers = List<Subscription<'a>>()
    let singleEventPool = List<List<'a>>()
    let mutable events = List<'a>()
    let mutable pending = List<'a>()
    let mutable total = 0
    member c.Clear() =
        events.Clear()
        pending.Clear()
        total <- 0
    /// Dispatches event immediately/synchronously
    member c.Handle(event) =
        // create a batch consisting of single item
        let batch = 
            if singleEventPool.Count = 0 then List<'a>(1) 
            else 
                let i = singleEventPool.Count - 1
                let list = singleEventPool.[i]
                singleEventPool.RemoveAt(i)
                list
        batch.Add(event)
        // run on all handlers
        dispatcher.Dispatch batch handlers
        // return batch to pool
        batch.Clear()
        singleEventPool.Add(batch)
    member c.Send(event) =
        pending.Add(event)
        total <- total + 1
    member c.SendAll(events : List<_>) =
        for event in events do
            pending.Add(event)
        total <- total + events.Count
    member c.Subscribe(handler) =
        let sub = new Subscription<_>(handler)
        handlers.Add(sub)
        sub :> IDisposable
    /// Calls handler behaviors and prunes subscriptions after
    member c.Publish() =
        if events.Count = 0 then false
        else
            dispatcher.Dispatch events handlers
            true
    interface IChannel with
        member c.Clear() = c.Clear()
        /// Calls handler behaviors and prunes subscriptions after
        member c.Publish() = c.Publish()
        /// Commit pending events to publish list and resets
        member c.Commit() =
            handlers.RemoveAll(isUnsubscribed) |> ignore
            events.Clear()            
            let temp = pending
            pending <- events
            events <- temp
    override c.ToString() =            
        sprintf "%s: %dH %dP %dE %dT %dSE" (typeof<'a> |> typeToString) 
            handlers.Count pending.Count events.Count total singleEventPool.Count

type internal EventHandlerCollection() =
    let isUnsubscribed = Predicate(fun (sub : Subscription) -> sub.IsUnsubscribed)
    let handlers = List<Subscription>()
    member c.Subscribe(handler : IEventHandler) =
        let sub = new Subscription(handler)
        handlers.Add(sub)
        sub :> IDisposable
    member c.Commit() =
        handlers.RemoveAll(isUnsubscribed) |> ignore
    interface IEventHandler with
        member c.Handle(batch) =
            for h in handlers do
                h.Handle(batch)

type internal PrintEventHandler(print) =
    interface IEventHandler with               
        member c.Handle batch =
            for x in batch do
                print (x.ToString())

type IChannels =
    abstract member GetChannel<'a> : unit -> Channel<'a>

/// Supports reentrancy
type Channels() =
    // lookup needed for reentrancy since it has a list we can iterate over
    let mutable dispatcher = Dispatcher.Default
    let lookup = IndexedLookup<Type, IChannel>()
    member c.Clear() =
        for i = 0 to lookup.Count - 1 do
            lookup.[i].Clear()
    member c.Commit() =
        for i = 0 to lookup.Count - 1 do
            lookup.[i].Commit()
    member c.SetDispatcher(newDispatcher) =
        dispatcher <- newDispatcher
    /// Returns true if any events were handled
    member c.Publish() =
        let mutable published = false
        for i = 0 to lookup.Count - 1 do
            published <- lookup.[i].Publish() || published
        published
    member c.GetChannel<'a>() =
        let t = typeof<'a>
        let i =
            match lookup.TryGetIndex(t) with
            | false, _ -> lookup.Add(t, Channel<'a>(c))
            | true, i -> i
        lookup.[i] :?> Channel<'a>
    interface IDispatcher with
        member c.Dispatch batch handlers =
            dispatcher.Dispatch batch handlers
    interface IChannels with
        member c.GetChannel<'a>() = c.GetChannel<'a>()
    override c.ToString() =
        let sb = StringBuilder()
        sb.Append("Channels") |> ignore
        let groups = 
            lookup.Entries 
            |> Seq.groupBy (fun kvp -> kvp.Key.Namespace)
            |> Seq.sortBy (fun (key, _) -> key)
        for ns, group in groups do
            let name = if String.IsNullOrEmpty(ns) then "[None]" else ns
            sb.AppendLine().Append("  " + name) |> ignore
            let channels = 
                group 
                |> Seq.sortBy (fun kvp -> kvp.Key.Name)
                |> Seq.map (fun kvp -> kvp.Value)
            for index in channels do
                let channel = lookup.[index]
                sb.AppendLine().Append("    " + channel.ToString()) |> ignore
        sb.ToString()
        
[<AutoOpen>]
module Channels =
    type IChannels with    
        member c.GetSender<'a>() =
            c.GetChannel<'a>().Send

        member c.Send(msg) =
            c.GetChannel<'a>().Send msg

        member c.Subscribe<'a>(handler) =
            c.GetChannel<'a>().Subscribe(handler)

        member c.Handle<'a>(event : 'a) =
            c.GetChannel<'a>().Handle event

        member c.On<'a>() =
            c.GetChannel<'a>().Subscribe

        member c.On<'a>(handler) =
            c.GetChannel<'a>().Subscribe(
                fun batch -> 
                    for i = 0 to batch.Count - 1 do
                        handler (batch.[i]))

    type Channel<'a> with    
        member c.Wait(msg) =
            c.Send(msg)
            Wait.defer

    type IChannels with    
        member c.Wait(msg) =
            c.GetChannel<'a>().Wait msg
