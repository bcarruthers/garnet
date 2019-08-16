namespace Garnet.Composition

open System
open System.Collections.Generic
open Garnet
open Garnet.Comparisons
open Garnet.Formatting

/// Identifies an actor
[<Struct>]
type ActorId =
    val value : int
    new(id) = { value = id }
    override e.ToString() = e.value.ToString()
    
module ActorId =
    let undefined = ActorId 0
    let isAny (id : ActorId) = true
         
/// Provides methods for constructing a batch of messages, sending
/// them upon disposal
type IMessageWriter<'a> =
    inherit IDisposable
    /// Assigns the source actor ID, which is typically the actor
    /// sending the message
    abstract member SetSource : ActorId -> unit
    /// Adds a recipient actor ID
    abstract member AddRecipient : ActorId -> unit
    /// Writes a single value to the current batch
    abstract member Write : 'a -> unit
    /// Writes a buffer to the current batch
    abstract member WriteAll : Buffer<'a> -> unit

type internal NullMessageWriter<'a>() =
    static let mutable instance = new NullMessageWriter<'a>() :> IMessageWriter<'a>
    static member Instance = instance
    interface IMessageWriter<'a> with
        member c.SetSource id = ()
        member c.AddRecipient id = ()
        member c.Write msg = ()
        member c.WriteAll msg = ()
        member c.Dispose() = ()                                          

type IOutbox =
    abstract member BeginSend<'a> : unit -> IMessageWriter<'a>

[<AutoOpen>]
module Mailbox =
    type IOutbox with
        member c.BeginSend<'a>(destId) =
            let batch = c.BeginSend<'a>()
            batch.AddRecipient destId
            batch
        member c.Send<'a>(destId, msg) =
            use batch = c.BeginSend<'a>(destId)
            batch.Write msg
        member c.Send<'a>(destId, msg, sourceId) =
            use batch = c.BeginSend<'a>(destId)
            batch.SetSource sourceId
            batch.Write msg
        member c.SendAll<'a>(destId, msgs : Buffer<'a>) =
            use batch = c.BeginSend<'a>(destId)
            batch.WriteAll msgs
        member c.SendAll<'a>(destId, msgs : Buffer<'a>, sourceId) =
            use batch = c.BeginSend<'a>(destId)
            batch.SetSource sourceId
            batch.WriteAll msgs

type NullOutbox() =
    static let mutable instance = NullOutbox() :> IOutbox
    static member Instance = instance
    interface IOutbox with
        member c.BeginSend<'a>() =
            NullMessageWriter<'a>.Instance

[<Struct>]
type internal MessageContext = {
    sourceId : ActorId
    destinationId : ActorId
    outbox : IOutbox
    }
        
[<Struct>]
type Mail<'a> = {
    outbox : IOutbox
    sourceId : ActorId
    destinationId : ActorId
    message : 'a
    } with
    override c.ToString() =
        sprintf "%d->%d: %A" c.sourceId.value c.destinationId.value c.message
                
type Mail<'a> with
    member c.BeginSend() =
        c.outbox.BeginSend()
    member c.BeginSend(destId) =
        c.outbox.BeginSend(destId)
    member c.BeginRespond() =
        c.BeginSend(c.sourceId)
    member c.Send(destId, msg) =
        use batch = c.BeginSend destId
        batch.Write msg
    member c.Respond(msg) =
        c.Send(c.sourceId, msg)

module private MessageContext = 
    let empty = {
        sourceId = ActorId.undefined
        destinationId = ActorId.undefined
        outbox = NullOutbox.Instance
        }
    
    let fromMail (c : Mail<_>) = {
        outbox = c.outbox
        sourceId = c.sourceId
        destinationId = c.destinationId
        }
            
type IInbox =
    abstract member Receive<'a> : Mail<Buffer<'a>> -> unit

type Mailbox() =
    let dict = Dictionary<Type, obj>()
    let mutable outbox = NullOutbox.Instance
    let mutable sourceId = ActorId.undefined
    let mutable destId = ActorId.undefined
    member c.SourceId = sourceId
    member c.DestinationId = destId
    member c.OnAll<'a>(action : Buffer<'a> -> unit) =
        let t = typeof<'a>
        let combined =
            match dict.TryGetValue t with
            | false, _ -> action
            | true, existing -> 
                let existing = existing :?> (Buffer<'a> -> unit)
                fun e -> 
                    existing e
                    action e        
        dict.[t] <- combined
    member c.On<'a>(handle : 'a -> unit) =
        c.OnAll<'a>(fun mail ->
            for i = 0 to mail.Count - 1 do
                handle mail.[i])
    member c.TryHandle<'a> e =
        match dict.TryGetValue(typeof<'a>) with
        | true, x -> 
            let handle = x :?> (Buffer<'a> -> unit)
            handle e
            true
        | false, _ -> false
    member c.Handle<'a> e =
        c.TryHandle<'a> e |> ignore
    interface IInbox with
        member c.Receive e =
            outbox <- e.outbox
            sourceId <- e.sourceId
            destId <- e.destinationId
            try
                c.Handle e.message
            finally
                outbox <- NullOutbox.Instance
                sourceId <- ActorId.undefined
                destId <- ActorId.undefined
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

module Mail =
    let private nullOutbox = NullOutbox()

    let empty msg = {
        outbox = nullOutbox
        sourceId = ActorId.undefined
        destinationId = ActorId.undefined
        message = msg
        }

    let map f mail = {
        outbox = mail.outbox
        sourceId = mail.sourceId
        destinationId = mail.destinationId
        message = f mail.message
        }

    let withMessage newMsg mail = {
        outbox = mail.outbox
        sourceId = mail.sourceId
        destinationId = mail.destinationId
        message = newMsg
        }
    
type private NullInbox() =
    interface IInbox with
        member c.Receive e = ()
    interface IDisposable with
        member c.Dispose() = ()
        
module private NullInbox =
    let handler = new NullInbox() :> IInbox

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

    let none = init Execution.None NullInbox.handler ignore

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
        let outbox = MessageContext.fromMail mail
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
        