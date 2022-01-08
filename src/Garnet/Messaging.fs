namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Buffers
open System.Threading
open Garnet.Composition.Comparisons

/// Identifies an actor
[<Struct>]
type ActorId =
    val Value : int
    new(value) = { Value = value }
    override e.ToString() = "0x" + e.Value.ToString("x")
    member e.IsDefined = e.Value <> 0
    member e.IsUndefined = e.Value = 0
    static member inline Undefined = ActorId 0

[<Struct>]
type Message<'a> = {
    Buffer : 'a[]
    Count : int
    SourceId : ActorId
    DestinationId : ActorId
    Pool : ArrayPool<'a>
    }

type IOutbox =
    abstract member SendAll<'a> : Message<'a> * ActorId -> unit
     
[<AutoOpen>]
module Mailbox =
    type IBufferWriter<'a> with
        member c.WriteValue(x) =
            c.GetSpan(1).[0] <- x
            c.Advance(1)

    type IOutbox with
        member c.Send<'a>(destId : ActorId, payload : 'a) =
            c.Send(destId, ActorId.Undefined, destId, payload)

        member c.Send<'a>(destId : ActorId, sourceId : ActorId, payload : 'a) =
            c.Send(destId, sourceId, destId, payload)

        member c.Send<'a>(destId : ActorId, sourceId : ActorId, deliveryId : ActorId, payload : 'a) =
            let arr = ArrayPool.Shared.Rent(1)
            arr.[0] <- payload
            c.SendAll({
                Buffer = arr
                Count = 1
                SourceId = sourceId
                DestinationId = destId
                Pool = ArrayPool.Shared
                }, deliveryId)

        member c.SendAll<'a>(destId : ActorId, payload : ReadOnlySpan<'a>) =
            c.SendAll(destId, ActorId.Undefined, destId, payload)

        member c.SendAll<'a>(destId : ActorId, sourceId : ActorId, payload : ReadOnlySpan<'a>) =
            c.SendAll(destId, sourceId, destId, payload)

        member c.SendAll<'a>(destId : ActorId, sourceId : ActorId, deliveryId : ActorId, payload : ReadOnlySpan<'a>) =
            let arr = ArrayPool.Shared.Rent(payload.Length)
            payload.CopyTo(arr.AsSpan())
            c.SendAll({
                Buffer = arr
                Count = payload.Length
                SourceId = sourceId
                DestinationId = destId
                Pool = ArrayPool.Shared
                }, deliveryId)

type NullOutbox() =
    static let mutable instance = NullOutbox() :> IOutbox
    static member Instance = instance
    interface IOutbox with
        member c.SendAll<'a>(message : Message<'a>, _) =
            if message.Buffer.Length > 0 then
                message.Pool.Return(message.Buffer, true)

/// Similar to ArrayBufferWriter, but uses ArrayPool for allocations
type MessageWriter<'a>() =
    let mutable arr = Array.Empty<'a>()
    let mutable pos = 0
    member val Outbox = NullOutbox.Instance with get, set
    member val SourceId = ActorId.Undefined with get, set
    member val DestinationId = ActorId.Undefined with get, set
    member val DeliveryId = ActorId.Undefined with get, set
    member c.WrittenCount = pos
    member c.WrittenSpan = ReadOnlySpan(arr, 0, pos)
    member c.WrittenMemory = ReadOnlyMemory(arr, 0, pos)
    member c.Allocate(minSize) =
        // Note min allocation in case count is zero
        let required = pos + max minSize 16
        if required > arr.Length then
            let newMem = ArrayPool.Shared.Rent(required)
            arr.CopyTo(newMem, 0)
            ArrayPool.Shared.Return(arr, true)
            arr <- newMem        
    member c.WriteValue(x) =
        if pos >= arr.Length then
            c.Allocate(1)
        arr.[pos] <- x
        pos <- pos + 1
    member c.Advance(count) =
        pos <- pos + count
    member c.GetMemory(minSize) =
        c.Allocate(minSize)
        Memory(arr, pos, arr.Length - pos)
    member c.GetSpan(minSize) =
        c.GetMemory(minSize).Span
    member c.Clear() =
        if arr.Length > 0 then
            ArrayPool.Shared.Return(arr, true)
            arr <- Array.Empty<'a>()
        pos <- 0
        c.Outbox <- NullOutbox.Instance
        c.SourceId <- ActorId.Undefined
        c.DestinationId <- ActorId.Undefined
        c.DeliveryId <- ActorId.Undefined
    member c.Send() =
        if pos > 0 then
            c.Outbox.SendAll({
                Buffer = arr
                Count = pos
                SourceId = c.SourceId
                DestinationId = c.DestinationId
                Pool = ArrayPool.Shared
                }, if c.DeliveryId.IsDefined then c.DeliveryId else c.DestinationId)
            arr <- Array.Empty<'a>()
        c.Clear()
    member c.Dispose() = c.Send()
    interface IDisposable with
        member c.Dispose() = c.Send()
    interface IBufferWriter<'a> with
        member c.Advance(count) = c.Advance(count)
        member c.GetMemory(minSize) = c.GetMemory(minSize)
        member c.GetSpan(minSize) = c.GetSpan(minSize)

type IInbox =
    abstract member Receive<'a> : IOutbox * Message<'a> -> unit
    
type internal MessageTypeId() =
    static let mutable id  = 0
    static member GetNext() = Interlocked.Increment(&id)

type internal MessageTypeId<'a>() =
    static let mutable id = MessageTypeId.GetNext()
    static member Id = id

type Mailbox() =
    let mutable lookup = Array.zeroCreate<obj>(8)
    let mutable outbox = NullOutbox() :> IOutbox
    let mutable sourceId = ActorId.Undefined
    let mutable destId = ActorId.Undefined
    member c.SourceId = sourceId
    member c.DestinationId = destId
    member c.OnAll<'a>(action : ReadOnlyMemory<'a> -> unit) =
        let id = MessageTypeId<'a>.Id
        Buffer.resizeArray (id + 1) &lookup
        let combined =
            let h = lookup.[id]
            if isNotNull h then
                let existing = h :?> ReadOnlyMemory<'a> -> unit
                fun e ->
                    existing e
                    action e
            else action
        lookup.[id] <- combined :> obj
    member c.On<'a>(handle : 'a -> unit) =
        c.OnAll<'a>(fun buffer ->
            let span = buffer.Span
            for i = 0 to span.Length - 1 do
                handle span.[i])
    member c.TryReceive<'a>(currentOutbox : IOutbox, e : Message<'a>) =
        let id = MessageTypeId<'a>.Id
        if id < lookup.Length then
            let h = lookup.[id]
            if isNotNull h then
                outbox <- currentOutbox
                sourceId <- e.SourceId
                destId <- e.DestinationId
                try
                    let handle = h :?> ReadOnlyMemory<'a> -> unit
                    handle (ReadOnlyMemory(e.Buffer, 0, e.Count))
                    true
                finally
                    outbox <- NullOutbox.Instance
                    sourceId <- ActorId.Undefined
                    destId <- ActorId.Undefined
            else false
        else false
    member c.Send<'a>(message, deliveryId) =
        outbox.Send<'a>(message, deliveryId)
    interface IInbox with
        member c.Receive(outbox, e) =
            c.TryReceive(outbox, e) |> ignore
    interface IOutbox with
        member c.SendAll<'a>(message, deliveryId) =
            outbox.SendAll<'a>(message, deliveryId)
    override c.ToString() =
        outbox.ToString()
    static member Create(register) =
        let inbox = Mailbox()
        register inbox
        inbox

type Mailbox with
    member c.Respond(msg) =
        c.Send(c.SourceId, msg)

type NullInbox() =
    static let mutable instance = NullInbox() :> IInbox
    static member Instance = instance
    interface IInbox with
        member c.Receive(_, _) = ()
    
type InboxCollection(handlers : IInbox[]) =
    new(handlers : IInbox seq) =
        InboxCollection(handlers |> Seq.toArray)
    interface IInbox with
        member c.Receive<'a>(outbox, e) =
            for handler in handlers do
                handler.Receive<'a>(outbox, e)
    override c.ToString() =
         Format.formatList "Inboxes" handlers.Length (String.Join("\n", handlers))

[<Struct>]
type Actor(inbox : IInbox, dispatcherId : int, dispose : unit -> unit) =
    static let mutable instance = Actor(NullInbox.Instance)
    static member Null = instance
    new(inbox, dispose) = Actor(inbox, 0, dispose)
    new(inbox, dispatcherId) = Actor(inbox, dispatcherId, ignore)
    new(inbox) = Actor(inbox, 0)
    member _.Inbox = inbox
    member _.DispatcherId = dispatcherId
    member _.Dispose() = dispose()
    member _.WithDispatcher(newId) =
        Actor(inbox, newId, dispose)

type IActorFactory =
    abstract member TryCreate : ActorId -> ValueOption<Actor>

type internal ActorFactoryCollection() =
    let factories = List<IActorFactory>()
    member c.Add(factory) =
        factories.Add(factory)
    member c.TryCreate(createId : ActorId) =
        let mutable result = ValueNone
        let mutable i = factories.Count - 1
        while result.IsNone && i >= 0 do
            result <- factories.[i].TryCreate(createId)
            i <- i - 1
        result
    interface IActorFactory with
        member c.TryCreate(createId) =
            c.TryCreate(createId)

type ActorFactory(tryCreate) =
    static let mutable instance = 
        ActorFactory(fun _ -> ValueSome Actor.Null) 
        :> IActorFactory
    static member Null = instance
    member c.TryCreate(createId : ActorId) =
        tryCreate createId
    interface IActorFactory with
        member c.TryCreate(createId) =
            c.TryCreate(createId)
    static member Create(tryCreate) =
        ActorFactory(tryCreate) :> IActorFactory
    static member Create(factories) =
        let c = ActorFactoryCollection()
        for factory in factories do
            c.Add(factory)
        c :> IActorFactory
    /// Creates for any actor ID
    static member Create(create : ActorId -> Actor) =
        ActorFactory(fun (createId : ActorId) ->
            create createId |> ValueSome)
    /// Creates conditionally for actor ID
    static member Create(predicate : ActorId -> bool, create : ActorId -> Actor) =
        ActorFactory(fun (createId : ActorId) ->
            if predicate createId then create createId |> ValueSome
            else ValueNone) :> IActorFactory
    /// Creates for a specific actor ID
    static member Create(actorId : ActorId, create : ActorId -> Actor) =
        ActorFactory.Create((=)actorId, create)
    /// Creates conditionally for actor ID
    static member Create(predicate : ActorId -> bool, register : ActorId -> Mailbox -> unit) =
        let create (createId : ActorId) =
            Actor(Mailbox.Create(register createId))
        ActorFactory.Create(predicate, create)
    /// Creates for any actor ID
    static member Create(register : ActorId -> Mailbox -> unit) =
        let predicate (_ : ActorId) = true
        ActorFactory.Create(predicate, register)
    /// Creates for a specific actor ID
    static member Create(actorId : ActorId, register : ActorId -> Mailbox -> unit) =
        ActorFactory.Create((=)actorId, register)

[<AutoOpen>]
module ActorFactory =
    type IActorFactory with
        member c.Wrap(map) =
            let tryCreate (createId : ActorId) =
                match c.TryCreate(createId) with
                | ValueSome actor -> ValueSome (map createId actor)
                | ValueNone -> ValueNone
            ActorFactory.Create(tryCreate)

        member c.WithDispatcher(dispatcherId) =
            c.Wrap(fun _ actor -> actor.WithDispatcher(dispatcherId))

        member c.Create(actorId) =
            match c.TryCreate(actorId) with
            | ValueSome actor -> actor
            | ValueNone -> Actor.Null

type internal DisposableCollection<'a when 'a :> IDisposable>(items : 'a[]) =
    interface IDisposable with
        member c.Dispose() =
            for item in items do 
                item.Dispose()

type Disposable(dispose) =
    static let mutable instance = new Disposable(ignore) :> IDisposable
    static member Null = instance
    interface IDisposable with
        member c.Dispose() = dispose()
    static member Create(dispose) =
        new Disposable(dispose) :> IDisposable
    static member Create(items : IDisposable[]) =
        new DisposableCollection<_>(items) :> IDisposable
    static member Create(items : IDisposable seq) =
        Disposable.Create(items |> Seq.toArray)

[<Struct>]
type Addresses = {
    SourceId : ActorId
    DestinationId : ActorId
    } with
    static member inline Undefined = {        
        SourceId = ActorId.Undefined
        DestinationId = ActorId.Undefined
        }

[<Struct>]
type internal MessageContext = {
    Addresses : Addresses
    Outbox : IOutbox
    }
             
module internal MessageContext = 
    let empty = {
        Addresses = Addresses.Undefined
        Outbox = NullOutbox.Instance
        }
    
/// Need sender member indirection because container registrations
/// need permanent reference while incoming messages have varying sender
type internal Outbox() =
    let mutable sendCount = 0
    let mutable pushCount = 0
    let mutable popCount = 0
    let mutable current = MessageContext.empty
    let outboxStack = Stack<_>()
    let popOutbox() = 
        popCount <- popCount + 1
        current <- outboxStack.Pop()
    let scope = new Disposable(popOutbox)
    member c.Current = current
    /// Set temporary outbox for a scope such as handling incoming message
    member c.Push(outbox, message : Message<_>) = 
        let context = {
            Outbox = outbox
            Addresses = {
                SourceId = message.SourceId
                DestinationId = message.DestinationId
                }
            }
        outboxStack.Push(current)
        current <- context
        pushCount <- pushCount + 1
        scope
    member c.SendAll(message, deliveryId) =             
        current.Outbox.SendAll(message, deliveryId)
        sendCount <- sendCount + 1
    interface IOutbox with
        member c.SendAll(message, deliveryId) =
            c.SendAll(message, deliveryId)            
    override c.ToString() =
        sprintf "Outbox: %d outboxes, %d sent, %d/%d push/pop" outboxStack.Count sendCount pushCount popCount
        