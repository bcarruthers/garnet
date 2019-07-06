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
    abstract member StartBatch<'a> : unit -> IMessageWriter<'a>

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

module Envelope =
    let forwardMessageFrom (e : Envelope<List<_>>) targetId channelId sourceId =
        use batch = e.outbox.StartBatch()
        batch.SetChannel channelId
        batch.SetSource sourceId
        batch.AddRecipient targetId
        for msg in e.message do
            batch.AddMessage msg

    let forwardMessage (e : Envelope<List<_>>) targetId channelId =
        forwardMessageFrom e targetId channelId e.sourceId

type IMessageHandler =
    abstract member Handle<'a> : Envelope<List<'a>> -> unit

type MessageHandler() =
    let dict = Dictionary<Type, obj>()
    member c.Subscribe<'a>(action : Envelope<List<'a>> -> unit) =
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
    interface IMessageHandler with
        member c.Handle e =
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
        member c.StartBatch() = nullBatch<'a>

type ActorDefinition = {
    handler : IMessageHandler
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

type private NullMessageHandler() =
    interface IMessageHandler with
        member c.Handle e = ()
    interface IDisposable with
        member c.Dispose() = ()
        
module private NullMessageHandler =
    let handler = new NullMessageHandler() :> IMessageHandler

[<AutoOpen>]
module MessageHandler =
    type MessageHandler with
        member c.On<'a>(action) =
            c.Subscribe<'a> <| fun e -> 
                for msg in e.message do
                    action { 
                        sourceId = e.sourceId
                        channelId = e.channelId
                        destinationId = e.destinationId
                        outbox = e.outbox
                        message = msg }
    
module ActorRule =
    let empty = {
        isBackground = false
        canCreate = fun id -> false
        create = fun id -> { 
            handler = NullMessageHandler.handler
            dispose = ignore 
            }
        }

type private MessageHandlerCollection(handlers : IMessageHandler[]) =
    interface IMessageHandler with
        member c.Handle<'a> e =
            for handler in handlers do
                handler.Handle<'a> e
            
module ActorDefinition =
    let init handler = { 
        handler = handler
        dispose = ignore 
        }

    let disposable<'a when 'a :> IDisposable and 'a :> IMessageHandler> (handler : 'a) = 
        { 
            handler = handler
            dispose = handler.Dispose 
        }

    let combine definitions =
        let definitions = definitions |> Seq.toArray
        let handlers = definitions |> Array.map (fun d -> d.handler)
        { 
            handler = MessageHandlerCollection(handlers) :> IMessageHandler
            dispose = fun () -> for d in definitions do d.dispose() 
        }

    let handlerId register id =
        let h = MessageHandler()
        h.On<MessageHandler -> unit> <| fun e -> 
            e.message h
        register id h
        { 
            handler = h :> IMessageHandler
            dispose = ignore 
        }

    let handler register =
        handlerId (fun _ handler -> register handler) ()
          
module ActorFactory =
    let init isBackground canCreate create = ActorFactory {
        isBackground = isBackground
        canCreate = canCreate
        create = create
        }

/// Need sender member indirection because container registrations
/// need permanent reference while incoming messages have varying sender
type MessageSender() =
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
    member c.StartBatch() =             
        // get current outbox
        let batch = outboxStack.Peek().StartBatch()
        batchCount <- batchCount + 1
        batch
    interface IOutbox with
        member c.StartBatch() = c.StartBatch()            
    override c.ToString() =
        sprintf "Sender: %d outboxes, %d batches, %d/%d push/pop" outboxStack.Count batchCount pushCount popCount

/// Maps incoming messages to include address info if needed
type MessageReceiver() =
    let mappings = Dictionary<Type, obj>()
    member c.Register<'a> (map : 'a -> Addresses -> 'a) = 
        mappings.[typeof<'a>] <- map
        new Disposable(ignore) :> IDisposable
    member c.Map<'a> (msg : 'a) addresses =
        match mappings.TryGetValue(typeof<'a>) with
        | true, map -> (map :?> 'a -> Addresses -> 'a) msg addresses
        | false, _ -> msg
    override c.ToString() =
        sprintf "Receiver: %d mappings" mappings.Count
