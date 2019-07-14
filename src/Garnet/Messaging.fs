namespace Garnet.Actors

open System
open System.Collections.Generic
open Garnet.Comparisons
          
[<Struct>]
type ActorId =
    val value : int
    new(id) = { value = id }
    override e.ToString() = e.value.ToString()
    
module ActorId =
    let undefined = ActorId 0
    let isAny (id : ActorId) = true
    
type IMessageWriter<'a> =
    inherit IDisposable
    abstract member SetChannel : int -> unit
    abstract member SetSource : ActorId -> unit
    abstract member AddRecipient : ActorId -> unit
    abstract member AddMessage : 'a -> unit

type IOutbox =
    abstract member BeginSend<'a> : unit -> IMessageWriter<'a>

[<Struct>]
type Envelope<'a> = {
    outbox : IOutbox
    sourceId : ActorId
    destinationId : ActorId
    channelId : int
    message : 'a
    } with
    override c.ToString() =
        sprintf "%d->%d: %A" c.sourceId.value c.destinationId.value c.message

type IInbox =
    abstract member Receive<'a> : Envelope<List<'a>> -> unit

type Inbox() =
    let dict = Dictionary<Type, obj>()
    member c.OnAll<'a>(action : Envelope<List<'a>> -> unit) =
        let t = typeof<'a>
        let combined =
            match dict.TryGetValue t with
            | false, _ -> action
            | true, existing -> 
                let existing = existing :?> (Envelope<List<'a>> -> unit)
                fun e -> 
                    existing e
                    action e        
        dict.[t] <- combined
    member c.TryReceive<'a> e =
        match dict.TryGetValue(typeof<'a>) with
        | true, x -> 
            let handle = x :?> (Envelope<List<'a>> -> unit)
            handle e
            true
        | false, _ -> false
    interface IInbox with
        member c.Receive e =
            c.TryReceive e |> ignore
            
[<Struct>]
type Disposable =
    val onDispose : unit -> unit
    new(onDispose) = { onDispose = onDispose }
    interface IDisposable with
        member c.Dispose() = c.onDispose()

type private NullMessageWriter<'a>() =
    interface IMessageWriter<'a> with
        member c.SetChannel id = ()
        member c.SetSource id = ()
        member c.AddRecipient id = ()
        member c.AddMessage msg = ()
        member c.Dispose() = ()
        
[<AutoOpen>]
module private Batches =
    let nullBatch<'a> = new NullMessageWriter<'a>() :> IMessageWriter<'a>

type NullOutbox() =
    interface IOutbox with
        member c.BeginSend() = nullBatch<'a>
        
module Envelope =
    let private nullOutbox = NullOutbox()

    let empty msg = {
        outbox = nullOutbox
        sourceId = ActorId.undefined
        destinationId = ActorId.undefined
        channelId = 0
        message = msg
        }

type IDisposableInbox =
    inherit IDisposable
    inherit IInbox
    
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
                
type Execution =
    | None = 0
    | Route = 1
    | Default = 2
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
        let inbox = Inbox()
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
    member c.Create(id : ActorId) =
        // first priority is execution type, then order where last wins
        let mutable result = Actor.none
        let mutable i = factories.Count - 1
        while int result.execution <> int Execution.Default && i >= 0 do
            let actor = factories.[i] id
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
        collection.Create

[<AutoOpen>]
module Inbox =
    type Inbox with
        member c.On<'a>(handler) =
            c.OnAll<'a> <| fun e -> 
                for i = 0 to e.message.Count - 1 do
                    handler (e.message.[i])

        member c.OnInbound<'a>(action) =
            c.OnAll<'a> <| fun e -> 
                for msg in e.message do
                    action { 
                        sourceId = e.sourceId
                        channelId = e.channelId
                        destinationId = e.destinationId
                        outbox = e.outbox
                        message = msg }

    type IOutbox with
        member c.BeginSend<'a>(destId) =
            let batch = c.BeginSend<'a>()
            batch.AddRecipient destId
            batch
        member c.Send<'a>(destId, msg) =
            c.Send<'a>(destId, msg, ActorId.undefined)
        member c.Send<'a>(destId, msg, sourceId) =
            c.Send<'a>(destId, msg, sourceId, 0)
        member c.Send<'a>(destId, msg, sourceId, channelId) =
            use batch = c.BeginSend<'a>(destId)
            batch.SetSource sourceId
            batch.SetChannel channelId
            batch.AddMessage msg
        member c.SendAll<'a>(destId, msgs : List<'a>) =
            c.SendAll<'a>(destId, msgs, ActorId.undefined)
        member c.SendAll<'a>(destId, msgs : List<'a>, sourceId) =
            c.SendAll<'a>(destId, msgs, sourceId, 0)
        member c.SendAll<'a>(destId, msgs : List<'a>, sourceId, channelId) =
            use batch = c.BeginSend<'a>(destId)
            batch.SetSource sourceId
            batch.SetChannel channelId
            for msg in msgs do
                batch.AddMessage msg
    
/// Need sender member indirection because container registrations
/// need permanent reference while incoming messages have varying sender
type internal Outbox() =
    let nullOutbox = NullOutbox() :> IOutbox
    let mutable batchCount = 0
    let mutable pushCount = 0
    let mutable popCount = 0
    let outboxStack = 
        let s = Stack<_>()
        s.Push(nullOutbox)
        s
    let popOutbox() = 
        popCount <- popCount + 1
        outboxStack.Pop() |> ignore
    let scope = new Disposable(popOutbox)
    /// Set temporary outbox for a scope such as handling incoming message
    member c.PushOutbox outbox = 
        outboxStack.Push outbox
        pushCount <- pushCount + 1
        scope
    /// Create an outgoing message batch which is sent on batch disposal
    member c.BeginSend() =             
        // get current outbox
        let batch = outboxStack.Peek().BeginSend()
        batchCount <- batchCount + 1
        batch
    interface IOutbox with
        member c.BeginSend() = c.BeginSend()            
    override c.ToString() =
        sprintf "Outbox: %d outboxes, %d batches, %d/%d push/pop" outboxStack.Count batchCount pushCount popCount
        