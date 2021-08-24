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
    static member inline Undefined = ActorId 0

[<Struct>]
type Destination = {
    DestinationId : ActorId
    RecipientId : ActorId
    }

type IMessageWriter =
    inherit IDisposable
    /// Assigns the source actor ID, which is typically the actor
    /// sending the message
    abstract member SetSource : ActorId -> unit
    /// Adds a recipient actor ID
    abstract member AddDestination : Destination -> unit
         
/// Provides methods for constructing a batch of messages, sending
/// them upon disposal
type IMessageWriter<'a> =
    inherit IMessageWriter
    inherit IBufferWriter<'a>

type IOutbox =
    abstract member BeginSend<'a> : unit -> IMessageWriter<'a>

type MessageWriter<'a>(dispose : Action<_>) =
    let mutable pos = 0
    let mutable arr = ArrayPool<'a>.Shared.Rent(0)
    let mutable sourceId = ActorId.Undefined
    let recipients = List<Destination>()
    new() = new MessageWriter<'a>(Action<_>(ignore))
    member c.Recipients = recipients
    member c.SourceId = sourceId
    member c.Memory = ReadOnlyMemory(arr, 0, pos)
    member c.Allocate minSize =
        let required = pos + minSize
        if required > arr.Length then
            let newMem = ArrayPool.Shared.Rent(required)
            arr.CopyTo(newMem, 0)
            ArrayPool.Shared.Return(arr, true)
            arr <- newMem        
    member c.SetSource(id) =
        sourceId <- id
    member c.AddDestination(id) =
        recipients.Add(id)
    member c.Advance count =
        pos <- pos + count
    member c.GetMemory minSize =
        c.Allocate minSize
        Memory(arr)
    member c.GetSpan minSize =
        c.Allocate minSize
        Span(arr)
    member c.CopyTo(writer : IMessageWriter<'a>) =
        writer.SetSource sourceId
        for dest in recipients do
            writer.AddDestination(dest)
        let span = c.Memory.Span
        span.CopyTo(writer.GetSpan(span.Length))
        writer.Advance(span.Length)
    member c.Dispose() = 
        sourceId <- ActorId.Undefined
        recipients.Clear()
        ArrayPool.Shared.Return(arr, true)
        arr <- ArrayPool<'a>.Shared.Rent(0)
        pos <- 0
        dispose.Invoke c
    interface IMessageWriter<'a> with
        member c.SetSource id = c.SetSource id
        member c.AddDestination id = c.AddDestination id
        member c.Advance count = c.Advance count
        member c.GetMemory minSize = c.GetMemory minSize
        member c.GetSpan minSize = c.GetSpan minSize
        member c.Dispose() = c.Dispose()
        
[<Struct>]
type Envelope<'a> = {
    Outbox : IOutbox
    SourceId : ActorId
    DestinationId : ActorId
    Message : 'a
    } with
    override c.ToString() =
        sprintf "%d->%d: %A" c.SourceId.Value c.DestinationId.Value c.Message

[<AutoOpen>]
module Mailbox =
    type IBufferWriter<'a> with
        member c.WriteValue(x) =
            c.GetSpan(1).[0] <- x
            c.Advance(1)

    type IMessageWriter with
        member c.AddDestination(destId) =
            c.AddDestination {
                DestinationId = destId
                RecipientId = destId
                }

        member c.AddDestination(destId, recipientId) =
            c.AddDestination {
                DestinationId = destId
                RecipientId = recipientId
                }

        member c.AddDestinations(recipients : ActorId[]) =
            for id in recipients do
                c.AddDestination(id)

        member c.AddDestinations(recipients : ReadOnlySpan<ActorId>) =
            for id in recipients do
                c.AddDestination(id)

    type IMessageWriter<'a> with
        member c.WriteAll(msgs : ReadOnlySpan<'a>) =
            let dest = c.GetSpan(msgs.Length)
            for i = 0 to msgs.Length - 1 do
                dest.[i] <- msgs.[i]
            c.Advance(msgs.Length)

    type IOutbox with
        member c.BeginSend<'a>(destId : ActorId) =
            let batch = c.BeginSend<'a>()
            batch.AddDestination(destId)
            batch

        member c.Send<'a>(destId, msg) =
            use batch = c.BeginSend<'a>(destId)
            batch.WriteValue(msg)

        member c.Send<'a>(destId, msg, sourceId) =
            use batch = c.BeginSend<'a>(destId)
            batch.SetSource(sourceId)
            batch.WriteValue(msg)

        member c.SendAll<'a>(destId, msgs : ReadOnlySpan<'a>) =
            use batch = c.BeginSend<'a>(destId)
            batch.WriteAll(msgs)

        member c.SendAll<'a>(destId, msgs : ReadOnlySpan<'a>, sourceId) =
            use batch = c.BeginSend<'a>(destId)
            batch.SetSource(sourceId)
            batch.WriteAll(msgs)

        member c.SendToAll<'a>(destIds : ReadOnlySpan<ActorId>, msg) =
            use batch = c.BeginSend<'a>()
            batch.WriteValue(msg)
            batch.AddDestinations(destIds)

        member c.SendToAll<'a>(destIds : ReadOnlySpan<ActorId>, msg, sourceId) =
            use batch = c.BeginSend<'a>()
            batch.SetSource(sourceId)
            batch.WriteValue(msg)
            batch.AddDestinations(destIds)

        member c.SendToAll<'a>(destIds : ActorId[], msg) =
            c.SendToAll<'a>(ReadOnlyMemory(destIds).Span, msg)

        member c.SendToAll<'a>(destIds : ActorId[], msg, sourceId) =
            c.SendToAll<'a>(ReadOnlyMemory(destIds).Span, msg, sourceId)

        member c.SendAll<'a>(e : Envelope<ReadOnlyMemory<'a>>) =
            use msg = c.BeginSend()
            msg.SetSource(e.SourceId)
            msg.AddDestination(e.DestinationId)
            let src = e.Message.Span
            let span = msg.GetSpan(e.Message.Length)
            src.CopyTo(span)
            msg.Advance(e.Message.Length)

type NullOutbox() =
    static let mutable instance = NullOutbox() :> IOutbox
    static member Instance = instance
    interface IOutbox with
        member c.BeginSend<'a>() =
            new MessageWriter<'a>() :> IMessageWriter<'a>
            
type IInbox =
    abstract member Receive<'a> : Envelope<ReadOnlyMemory<'a>> -> unit
    
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
    member c.TryReceive<'a> (e : Envelope<ReadOnlyMemory<'a>>) =
        let id = MessageTypeId<'a>.Id
        if id < lookup.Length then
            let h = lookup.[id]
            if isNotNull h then
                outbox <- e.Outbox
                sourceId <- e.SourceId
                destId <- e.DestinationId
                try
                    let handle = h :?> ReadOnlyMemory<'a> -> unit
                    handle e.Message
                    true
                finally
                    outbox <- NullOutbox.Instance
                    sourceId <- ActorId.Undefined
                    destId <- ActorId.Undefined
            else false
        else false
    member c.BeginSend<'a>() =
        outbox.BeginSend<'a>()
    interface IInbox with
        member c.Receive e =
            c.TryReceive e |> ignore
    interface IOutbox with
        member c.BeginSend<'a>() =
            outbox.BeginSend<'a>()
    override c.ToString() =
        outbox.ToString()
    static member Create(register) =
        let inbox = Mailbox()
        register inbox
        inbox

type Mailbox with
    member c.BeginRespond() =
        c.BeginSend(c.SourceId)

    member c.Respond(msg) =
        c.Send(c.SourceId, msg)

type NullInbox() =
    static let mutable instance = NullInbox() :> IInbox
    static member Instance = instance
    interface IInbox with
        member c.Receive _ = ()
    
type private InboxCollection(handlers : IInbox[]) =
    interface IInbox with
        member c.Receive<'a> e =
            for handler in handlers do
                handler.Receive<'a> e
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
    
    let fromEnvelope (c : Envelope<_>) = {
        Outbox = c.Outbox
        Addresses = {
            SourceId = c.SourceId
            DestinationId = c.DestinationId
            }
        }

/// Need sender member indirection because container registrations
/// need permanent reference while incoming messages have varying sender
type internal Outbox() =
    let mutable batchCount = 0
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
    member c.Push mail = 
        let outbox = MessageContext.fromEnvelope mail
        outboxStack.Push current
        current <- outbox
        pushCount <- pushCount + 1
        scope
    /// Create an outgoing message batch which is sent on batch disposal
    member c.BeginSend() =             
        // get current outbox
        let batch = current.Outbox.BeginSend()
        batchCount <- batchCount + 1
        batch
    interface IOutbox with
        member c.BeginSend() = c.BeginSend()            
    override c.ToString() =
        sprintf "Outbox: %d outboxes, %d batches, %d/%d push/pop" outboxStack.Count batchCount pushCount popCount
        