namespace Garnet.Actors

open System
open System.Collections.Generic
open Garnet.Comparisons
          
[<Struct>]
type ActorId =
    val value : int
    new(id) = { value = id }
    override e.ToString() = e.value.ToString()
        
[<Struct>]
type Addresses = {
    sourceId : ActorId
    destinationId : ActorId
    }
    
module ActorId =
    let undefined = ActorId 0
    let isAny (id : ActorId) = true
    
module Addresses =
    let undefined = { 
        sourceId = ActorId.undefined
        destinationId = ActorId.undefined 
        }
    
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
    member c.addresses =
        { sourceId = c.sourceId
          destinationId = c.destinationId }
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
    member c.TryHandle<'a> e =
        match dict.TryGetValue(typeof<'a>) with
        | true, x -> 
            let handle = x :?> (Envelope<List<'a>> -> unit)
            handle e
            true
        | false, _ -> false
    interface IInbox with
        member c.Receive e =
            c.TryHandle e |> ignore
            
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

    let forwardMessageFrom (e : Envelope<List<_>>) targetId channelId sourceId =
        use batch = e.outbox.BeginSend()
        batch.SetChannel channelId
        batch.SetSource sourceId
        batch.AddRecipient targetId
        for msg in e.message do
            batch.AddMessage msg

    let forwardMessage (e : Envelope<List<_>>) targetId channelId =
        forwardMessageFrom e targetId channelId e.sourceId

type ActorDefinition = {
    handler : IInbox
    dispose : unit -> unit
    }

type ActorFactory = {
    isBackground : bool
    canCreate : ActorId -> bool
    create : ActorId -> ActorDefinition
    }

type ActorRule =
    | ActorRedirect of (ActorId -> ActorId)
    | ActorFactory of ActorFactory

type private NullInbox() =
    interface IInbox with
        member c.Receive e = ()
    interface IDisposable with
        member c.Dispose() = ()
        
module private NullInbox =
    let handler = new NullInbox() :> IInbox

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
    
module ActorRule =
    let empty = {
        isBackground = false
        canCreate = fun id -> false
        create = fun id -> { 
            handler = NullInbox.handler
            dispose = ignore 
            }
        }

type private InboxCollection(handlers : IInbox[]) =
    interface IInbox with
        member c.Receive<'a> e =
            for handler in handlers do
                handler.Receive<'a> e
            
module ActorDefinition =
    let init handler = { 
        handler = handler
        dispose = ignore 
        }

    let disposable<'a when 'a :> IDisposable and 'a :> IInbox> (handler : 'a) = 
        { 
            handler = handler
            dispose = handler.Dispose 
        }

    let combine definitions =
        let definitions = definitions |> Seq.toArray
        let handlers = definitions |> Array.map (fun d -> d.handler)
        { 
            handler = InboxCollection(handlers) :> IInbox
            dispose = fun () -> for d in definitions do d.dispose() 
        }

    let handlerId register id =
        let h = Inbox()
        h.OnInbound<Inbox -> unit> <| fun e -> 
            e.message h
        register id h
        { 
            handler = h :> IInbox
            dispose = ignore 
        }

    let handler register =
        handlerId (fun _ handler -> register handler) ()
          
module ActorFactory =
    let main canCreate create = ActorFactory {
        isBackground = false
        canCreate = canCreate
        create = create
        }

    let initRange canCreate create = ActorFactory {
        isBackground = true
        canCreate = canCreate
        create = create
        }

    let init actorId create = ActorFactory {
        isBackground = true
        canCreate = ((=)actorId)
        create = fun id -> create()
        }

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
        