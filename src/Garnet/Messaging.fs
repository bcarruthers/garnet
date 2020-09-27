namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Buffers
open System.Threading
open Garnet
open Garnet.Comparisons
open Garnet.Formatting

/// Identifies an actor
[<Struct>]
type ActorId =
    val value : int
    new(value) = { value = value }
    override e.ToString() = "0x" + e.value.ToString("x")
    
module ActorId =
    let undefined = ActorId 0
    let isAny (id : ActorId) = true

type IMessageWriter =
    inherit IDisposable
    /// Assigns the source actor ID, which is typically the actor
    /// sending the message
    abstract member SetSource : ActorId -> unit
    /// Adds a recipient actor ID
    abstract member AddRecipient : ActorId -> unit
         
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
    let mutable sourceId = ActorId.undefined
    let recipients = List<ActorId>()
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
    member c.SetSource id =
        sourceId <-sourceId
    member c.AddRecipient id =
        recipients.Add id
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
        for id in recipients do
            writer.AddRecipient id
        let span = c.Memory.Span
        span.CopyTo(writer.GetSpan(span.Length))
        writer.Advance(span.Length)
    member c.Dispose() = 
        sourceId <- ActorId.undefined
        recipients.Clear()
        ArrayPool.Shared.Return(arr, true)
        arr <- ArrayPool<'a>.Shared.Rent(0)
        pos <- 0
        dispose.Invoke c
    interface IMessageWriter<'a> with
        member c.SetSource id = c.SetSource id
        member c.AddRecipient id = c.AddRecipient id
        member c.Advance count = c.Advance count
        member c.GetMemory minSize = c.GetMemory minSize
        member c.GetSpan minSize = c.GetSpan minSize
        member c.Dispose() = c.Dispose()

[<AutoOpen>]
module Mailbox =
    type IBufferWriter<'a> with
        member c.WriteValue(x) =
            c.GetSpan(1).[0] <- x
            c.Advance(1)

    type IMessageWriter<'a> with
        member c.WriteAll(msgs : ReadOnlySpan<'a>) =
            let dest = c.GetSpan(msgs.Length)
            for i = 0 to msgs.Length - 1 do
                dest.[i] <- msgs.[i]
            c.Advance(msgs.Length)

        member c.AddRecipients(recipients : ReadOnlySpan<ActorId>) =
            for id in recipients do
                c.AddRecipient(id)

    type IOutbox with
        member c.BeginSend<'a>(destId) =
            let batch = c.BeginSend<'a>()
            batch.AddRecipient destId
            batch
        member c.Send<'a>(destId, msg) =
            use batch = c.BeginSend<'a>(destId)
            batch.WriteValue msg
        member c.Send<'a>(destId, msg, sourceId) =
            use batch = c.BeginSend<'a>(destId)
            batch.SetSource sourceId
            batch.WriteValue msg
        member c.SendAll<'a>(destId, msgs : ReadOnlySpan<'a>) =
            use batch = c.BeginSend<'a>(destId)
            batch.WriteAll msgs
        member c.SendAll<'a>(destId, msgs : ReadOnlySpan<'a>, sourceId) =
            use batch = c.BeginSend<'a>(destId)
            batch.SetSource sourceId
            batch.WriteAll msgs
        member c.SendToAll<'a>(destIds : ReadOnlySpan<ActorId>, msg) =
            use batch = c.BeginSend<'a>()
            batch.WriteValue msg
            batch.AddRecipients(destIds)
        member c.SendToAll<'a>(destIds : ReadOnlySpan<ActorId>, msg, sourceId) =
            use batch = c.BeginSend<'a>()
            batch.SetSource(sourceId)
            batch.WriteValue msg
            batch.AddRecipients(destIds)

type NullOutbox() =
    static let mutable instance = NullOutbox() :> IOutbox
    static member Instance = instance
    interface IOutbox with
        member c.BeginSend<'a>() =
            new MessageWriter<'a>() :> IMessageWriter<'a>
        
[<Struct>]
type Envelope<'a> = {
    outbox : IOutbox
    sourceId : ActorId
    destinationId : ActorId
    message : 'a
    } with
    override c.ToString() =
        sprintf "%d->%d: %A" c.sourceId.value c.destinationId.value c.message
            
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
    let mutable sourceId = ActorId.undefined
    let mutable destId = ActorId.undefined
    member c.SourceId = sourceId
    member c.DestinationId = destId
    member c.OnAll<'a>(action : ReadOnlyMemory<'a> -> unit) =
        let id = MessageTypeId<'a>.Id
        Buffer.resizeArray (id + 1) &lookup
        let combined =
            let h = lookup.[id]
            if isNotNull h then
                let existing = h :?> (ReadOnlyMemory<'a> -> unit)
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
                outbox <- e.outbox
                sourceId <- e.sourceId
                destId <- e.destinationId
                try
                    let handle = h :?> (ReadOnlyMemory<'a> -> unit)
                    handle e.message
                    true
                finally
                    outbox <- NullOutbox.Instance
                    sourceId <- ActorId.undefined
                    destId <- ActorId.undefined
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

type Mailbox with
    member c.BeginRespond() =
        c.BeginSend(c.SourceId)
    member c.Respond(msg) =
        c.Send(c.SourceId, msg)
        
type NullInbox() =
    static let mutable instance = NullInbox() :> IInbox
    static member Instance = instance
    interface IInbox with
        member c.Receive e = ()
    
type private InboxCollection(handlers : IInbox[]) =
    interface IInbox with
        member c.Receive<'a> e =
            for handler in handlers do
                handler.Receive<'a> e
    override c.ToString() =
         formatList "Inboxes" handlers.Length (String.Join("\n", handlers))
     
/// Defines how an actor is executed or run
type Execution =
    /// Null endpoint which ignores all messages
    | None = 0
    /// Routes incoming messages to another actor ID
    | Route = 1
    /// Actor which can be run on a background thread
    | Default = 2
    /// Actor which must be run on the main thread
    | Main = 3

[<Struct>]
type Actor = {
    routedId : ActorId
    execution : Execution
    inbox : IInbox
    dispose : unit -> unit
    }

module Actor =
    let init exec inbox dispose = {
        routedId = ActorId.undefined
        execution = exec
        inbox = inbox
        dispose = dispose
        }

    let none = init Execution.None (NullInbox()) ignore

    let route routedId = { 
        none with 
            routedId = routedId 
            execution = Execution.Route
            }

    let disposable inbox dispose =
        init Execution.Default inbox dispose

    let inbox inbox = disposable inbox ignore

    let handler register = 
        let inbox = Mailbox()
        register inbox
        disposable inbox ignore

    let execMain a =
        { a with execution = Execution.Main }

    let combine actors =
        let actors = actors |> Seq.toArray
        if actors.Length = 0 then none
        else
            let inboxes = actors |> Array.map (fun d -> d.inbox)
            let exec = 
                actors 
                |> Seq.map (fun a -> a.execution) 
                |> Seq.reduce max
            let inbox = InboxCollection(inboxes) :> IInbox
            let dispose = fun () -> for d in actors do d.dispose() 
            init exec inbox dispose

type internal ActorFactoryCollection() =
    let factories = List<_>()
    member c.Add(desc : ActorId -> Actor) =
        factories.Add(desc)
    member c.Create actorId =
        // first priority is execution type, then order where last wins
        let mutable result = Actor.none
        let mutable i = factories.Count - 1
        while int result.execution <> int Execution.Default && i >= 0 do
            let actor = factories.[i] (ActorId actorId)
            if int actor.execution > int result.execution then
                result <- actor
            i <- i - 1
        result

module ActorFactory =
    let route map =
        fun (id : ActorId) -> Actor.route (map id)

    let filter canCreate create =
        fun id -> if canCreate id then create id else Actor.none

    let any create =
        filter (fun id -> true) create

    let init actorId create =
        filter ((=)actorId) (fun id -> create())

    let filterHandler canCreate register =
        filter canCreate (fun id -> Actor.handler (register id))

    let handler actorId register =
        init actorId (fun () -> Actor.handler register)

    let map (f : Actor -> Actor) create =
        fun (id : ActorId) -> create id |> f

    let combine factories =
        let collection = ActorFactoryCollection()
        for f in factories do collection.Add f
        fun (id : ActorId) -> collection.Create id.value

module Disposable =
    type private Disposable(dispose) =
        interface IDisposable with
            member c.Dispose() = dispose()

    type private DisposableList(items : seq<IDisposable>) =
        let list = List<_>(items)
        new() = new DisposableList(Seq.empty)
        member c.Add item =
            list.Add item
        member c.Remove item =
            list.Remove item
        interface IDisposable with
            member c.Dispose() =
                for item in list do 
                    item.Dispose()

    let init dispose = 
        new Disposable(dispose) :> IDisposable

    let list items =
        new DisposableList(items |> Seq.map (fun x -> x :> IDisposable)) 
        :> IDisposable

    let empty =
        init ignore

    let combine systems =
        fun c -> systems |> List.map (fun f -> f c) |> list

[<Struct>]
type Addresses = {
    sourceId : ActorId
    destinationId : ActorId
    }

module Addresses = 
    let undefined = {        
        sourceId = ActorId.undefined
        destinationId = ActorId.undefined
        }

[<Struct>]
type internal MessageContext = {
    addresses : Addresses
    outbox : IOutbox
    }
             
module internal MessageContext = 
    let empty = {
        addresses = Addresses.undefined
        outbox = NullOutbox.Instance
        }
    
    let fromEnvelope (c : Envelope<_>) = {
        outbox = c.outbox
        addresses = {
            sourceId = c.sourceId
            destinationId = c.destinationId
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
    let scope = Disposable.init popOutbox
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
        let batch = current.outbox.BeginSend()
        batchCount <- batchCount + 1
        batch
    interface IOutbox with
        member c.BeginSend() = c.BeginSend()            
    override c.ToString() =
        sprintf "Outbox: %d outboxes, %d batches, %d/%d push/pop" outboxStack.Count batchCount pushCount popCount
        