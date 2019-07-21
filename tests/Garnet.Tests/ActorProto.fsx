open System
open System.Collections.Generic
open System.Threading

type Run = struct end
type Ping = struct end
type Pong = struct end

// Inbox and outbox with IMessage
// No pooling for messages, so GC
module Proto1 =
    type IReceiver =
        abstract member Receive<'a> : 'a -> unit

    type IMessage =
        inherit IDisposable
        abstract member Receive : IReceiver -> unit

    type Message<'a>(payload : 'a) =
        interface IMessage with
            member c.Receive(receiver) =
                receiver.Receive(payload)
            member c.Dispose() =
                ()

    module Message =
        let init payload = new Message<_>(payload) :> IMessage

    type Mail<'a> = {
        message : 'a
        send : int -> IMessage -> unit 
        }

    type Actor() =
        let handlers = Dictionary<Type, obj>()
        let incoming = Queue<IMessage>()
        let outgoing = Queue<struct(int * IMessage)>()        
        //let disposals = Queue<IMessage>()
        let send destId msg = 
            outgoing.Enqueue(struct(destId, msg))
        member c.On<'a>(handle) =
            handlers.Add(typeof<'a>, Action<Mail<'a>>(handle))
        member c.Enqueue msg =
            incoming.Enqueue msg
        member c.Process() =
            let count = incoming.Count
            while incoming.Count > 0 do
                let msg = incoming.Dequeue()
                msg.Receive(c)
            count > 0
        member c.Distribute (actors : Dictionary<_, Actor>) =
            let count = outgoing.Count
            while outgoing.Count > 0 do
                let struct(destId, msg) = outgoing.Dequeue()
                match actors.TryGetValue(destId) with
                | true, actor -> actor.Enqueue msg
                | false, _ -> ()
            count > 0           
        interface IReceiver with
            member c.Receive<'a>(msg : 'a) =
                match handlers.TryGetValue(typeof<'a>) with
                | true, handle -> (handle :?> Action<_>).Invoke { message = msg; send = send }
                | false, _ -> ()

    type ActorSet() =
        let actors = Dictionary<int, Actor>()
        member c.Register(id, actor) =
            actors.Add(id, actor)
        member c.Process() =
            let mutable handled = false
            for a in actors.Values do
                handled <- a.Process() || handled
            handled
        member c.Distribute() =
            let mutable handled = false
            for a in actors.Values do
                handled <- a.Distribute(actors) || handled
            handled
        member c.RunOnce() =
            let processed = c.Process()
            let distrib = c.Distribute()
            processed || distrib
        member c.Send(destId, msg) =
            match actors.TryGetValue(destId) with
            | true, actor -> actor.Enqueue msg
            | false, _ -> ()            
        member c.Run() =
            while c.RunOnce() do ()

    let runPingPong onPing onPong iterations =
        let mutable count = 0
        let a = ActorSet()
        a.Register(1, 
            let h = Actor()
            h.On<Run> <| fun e ->
                e.send 2 <| Message.init (Ping())
            h.On<Pong> <| fun e ->
                count <- count + 1
                if count < iterations then
                    onPing(e)
                    e.send 2 <| Message.init (Ping())
            h)
        a.Register(2,
            let h = Actor()
            h.On<Ping> <| fun e -> 
                onPong e
                e.send 1 <| Message.init (Pong())
            h)
        a.Send(1, Message.init (Run()))
        a.Run()
    
    let test() =
        runPingPong ignore ignore 5000000
        //runPingPong (printfn "Ping %A") (printfn "Pong %A") 10

// Flat list of handlers
module Proto2 =
    type ISender =
        abstract Send<'a> : int * 'a -> unit

    [<Struct>]
    type Mail<'a> = {
        message : 'a
        system : ISender 
        }

    type Handler<'a>(handle) =
        let inbox = Queue<'a>()
        member c.Enqueue msg =
            inbox.Enqueue msg
        member c.Process send =
            let count = inbox.Count
            while inbox.Count > 0 do
                let msg = inbox.Dequeue()
                handle { message = msg; system = send }
            count

    type ActorSet() =
        let handlers = Dictionary<struct(int * Type), obj>()
        let processors = List<_>()
        member c.On<'a>(id, handle : Mail<'a> -> unit) =
            let handler = Handler<'a>(handle)
            handlers.Add(struct(id, typeof<'a>), handler :> obj)
            processors.Add handler.Process
        member c.Send<'a>(destId, msg : 'a) =
            match handlers.TryGetValue(struct(destId, typeof<'a>)) with
            | true, handler -> (handler :?> Handler<'a>).Enqueue msg
            | false, _ -> ()
        member c.RunOnce() =
            let mutable count = 0
            for proc in processors do
                count <- count + proc c
            count
        member c.Run() =
            while c.RunOnce() > 0 do ()
        interface ISender with
            member c.Send(destId, msg) = c.Send(destId, msg)

    let runPingPong onPing onPong iterations =
        let mutable count = 0
        let a = ActorSet()
        a.On<Run>(1, fun e -> e.system.Send(2, Ping()))
        a.On<Pong>(1, fun e ->
            count <- count + 1
            if count < iterations then
                onPing(e)
                e.system.Send(2, Ping()))
        a.On<Ping>(2, fun e -> 
            onPong e
            e.system.Send(1, Pong()))
        a.Send(1, Run())
        a.Run()
    
    let test() =
        runPingPong ignore ignore 5000000
        //runPingPong (printfn "Ping %A") (printfn "Pong %A") 10

// Inbox only, no GC
// Slightly faster than flat handler list
module Proto3 =
    type ISender =
        abstract Send<'a> : int * 'a -> unit

    type IProcessor =
        abstract Process : ISender -> unit

    [<Struct>]
    type Mail<'a> = {
        message : 'a
        system : ISender 
        }

    type Handler<'a>(handle) =
        let inbox = Queue<'a>()
        member c.Enqueue msg =
            inbox.Enqueue msg
        interface IProcessor with
            member c.Process send =
                let msg = inbox.Dequeue()
                handle { message = msg; system = send }

    type Actor() =
        let queue = Queue<_>()
        let handlers = Dictionary<Type, IProcessor>()
        member c.On<'a>(handle : Mail<'a> -> unit) =
            let handler = Handler<'a>(handle)
            handlers.Add(typeof<'a>, handler :> IProcessor)
        member c.Enqueue<'a> (msg : 'a) =
            match handlers.TryGetValue(typeof<'a>) with
            | true, h -> 
                let handler = h :?> Handler<'a>
                handler.Enqueue msg
                queue.Enqueue (handler :> IProcessor)
            | false, _ -> ()
        member c.Process (send : ISender) =
            let count = queue.Count
            while queue.Count > 0 do
                let proc = queue.Dequeue()
                proc.Process send
            count            

    type ActorSet() =
        let actors = Dictionary<int, Actor>()
        member c.Register(id, actor : Actor) =
            actors.Add(id, actor)
        member c.Send<'a>(destId, msg : 'a) =
            match actors.TryGetValue(destId) with
            | true, actor -> actor.Enqueue msg
            | false, _ -> ()
        member c.RunOnce() =
            let mutable count = 0
            for actor in actors.Values do
                count <- count + actor.Process c
            count
        member c.Run() =
            while c.RunOnce() > 0 do ()
        interface ISender with
            member c.Send(destId, msg) = c.Send(destId, msg)

    let runPingPong onPing onPong iterations =
        let mutable count = 0
        let a = ActorSet()
        a.Register(1, 
            let h = Actor()
            h.On<Run> <| fun e ->
                e.system.Send(2, Ping())
            h.On<Pong> <| fun e ->
                count <- count + 1
                if count < iterations then
                    onPing(e)
                    e.system.Send(2, Ping())
            h)
        a.Register(2,
            let h = Actor()
            h.On<Ping> <| fun e -> 
                onPong e
                e.system.Send(1, Pong())
            h)
        a.Send(1, Run())
        a.Run()
    
    let test() =
        runPingPong ignore ignore 5000000
        //runPingPong (printfn "Ping %A") (printfn "Pong %A") 10

// Impact of uncontested locks on perf
module Proto4 =
    [<Struct>]
    type Addresses = {
        sourceId : int
        destinationId : int
        }

    module Addresses =
        let init s r = {
            sourceId = s
            destinationId = r
            }

    type IBatch<'a> =
        inherit IDisposable
        abstract Add : 'a -> unit 

    type IOutbox =
        abstract BeginSend<'a> : Addresses -> IBatch<'a>

    [<AutoOpen>]
    module Outbox =
        type IOutbox with
            member c.Send<'a>(addresses, msg : 'a) =
                use batch = c.BeginSend<'a> addresses
                batch.Add msg

    [<Struct>]
    type Mail<'a> = {
        actorId : int
        addresses : Addresses
        message : 'a
        system : IOutbox 
        //dispose : 'a -> unit
        }
        
    type Mail<'a> with
        member c.BeginSend(destId) =
            c.system.BeginSend(Addresses.init c.addresses.destinationId destId)
        member c.BeginRespond() =
            c.BeginSend(c.addresses.sourceId)
        member c.Send(destId, msg) =
            use batch = c.BeginSend destId
            batch.Add msg
        member c.Respond(msg) =
            c.Send(c.addresses.sourceId, msg)

    type IHandler =
        inherit IOutbox
        abstract Process : int * IOutbox -> bool

    type ISubscribable =
        abstract OnAll<'a> : (Mail<List<'a>> -> unit) -> unit

    [<AutoOpen>]
    module Subscribable =
        type ISubscribable with
            member c.On<'a>(handle : Mail<'a> -> unit) =
                c.OnAll<'a>(fun mail ->
                    for msg in mail.message do
                        handle {
                            message = msg
                            actorId = mail.actorId
                            addresses = mail.addresses
                            system = mail.system
                            })

    type private ITypeHandler =
        abstract Process : int * Addresses * IOutbox -> unit
        
    [<Struct>]
    type private QueuedBatch = {
        addresses : Addresses
        typeHandler : ITypeHandler
        }

    type NullBatch<'a>() =
        static member Batch = new NullBatch<'a>() :> IBatch<'a>
        interface IBatch<'a> with
            member c.Add msg = ()
            member c.Dispose() = ()
        
    type private Handler<'a>(queueSync : obj, queue : Queue<_>, handle) =
        let inbox = Queue<List<'a>>()
        let pool = Stack<List<'a>>()

        let mutable addresses = Addresses.init 0 0
        let mutable batch : List<'a> = null
        member c.BeginSend(addr) =
            Monitor.Enter queueSync
            addresses <- addr
            batch <- if pool.Count > 0 then pool.Pop() else List<'a>()
            c :> IBatch<'a>
        interface IBatch<'a> with
            member c.Add msg =
                batch.Add msg
            member c.Dispose() =
                inbox.Enqueue batch
                queue.Enqueue {
                    addresses = addresses
                    typeHandler = c
                    }
                batch <- null
                addresses <- Addresses.init 0 0
                Monitor.Exit queueSync

        interface ITypeHandler with
            member c.Process(actorId, addresses, send) =
                let batch = inbox.Dequeue()
                Monitor.Exit queueSync
                handle { 
                    actorId = actorId
                    addresses = addresses
                    message = batch
                    system = send 
                    //dispose = ignore
                    }
                batch.Clear()
                // recycle batch
                Monitor.Enter queueSync
                pool.Push batch
                Monitor.Exit queueSync

    type private Handler() =
        let handlers = Dictionary<Type, ITypeHandler>()
        let queue = Queue<QueuedBatch>()
        let queueSync = obj()           
        interface ISubscribable with
            member c.OnAll<'a>(handle : Mail<List<'a>> -> unit) =
                let handler = new Handler<'a>(queueSync, queue, handle)
                handlers.Add(typeof<'a>, handler :> ITypeHandler)
        interface IHandler with
            member c.BeginSend<'a>(addresses) =
                match handlers.TryGetValue(typeof<'a>) with
                | true, h ->
                    let handler = h :?> Handler<'a>
                    handler.BeginSend(addresses)                
                | false, _ ->
                    NullBatch<'a>.Batch
            member c.Process(actorId, send : IOutbox) =
                // lock for duration of dequeue, which ends
                // in handler process method
                Monitor.Enter queueSync
                if queue.Count > 0 then
                    let item = queue.Dequeue()
                    item.typeHandler.Process(actorId, item.addresses, send)
                    true
                else
                    Monitor.Exit queueSync
                    false
                    
    // throughput limits how many messages we can process at a time before
    // going to another actor
    type Actor(actorId, logId, throughput, handler : IHandler) =
        let procSync = obj()
        member c.BeginSend addresses =
            handler.BeginSend addresses
        member c.Process (sender : IOutbox) =
            let mutable count = 0
            try
                Monitor.Enter procSync
                try
                    while count < throughput && handler.Process(actorId, sender) do
                        count <- count + 1
                finally
                    Monitor.Exit procSync
            with ex ->
                sender.Send(Addresses.init actorId logId, ex)                
            count > 0

    type ActorSet() =
        let actors = Dictionary<int, Actor>()
        let logId = 0
        let mutable mapId = id
        member c.Register(newMapId) =
            mapId <- newMapId << mapId
        member c.Register(actorId, throughput, register) =
            let builder = Handler()
            register (builder :> ISubscribable)
            actors.Add(actorId, Actor(actorId, logId, throughput, builder))
        member c.BeginSend<'a> addresses =
            let mappedId = mapId addresses.destinationId
            match actors.TryGetValue(mappedId) with
            | true, actor -> actor.BeginSend(addresses)
            | false, _ -> NullBatch<'a>.Batch
        member c.RunOnce() =
            let mutable handled = false
            for actor in actors.Values do
                handled <- actor.Process c || handled
            handled
        member c.Run() =
            while c.RunOnce() do ()
        interface IOutbox with
            member c.BeginSend<'a> addresses =
                c.BeginSend<'a> addresses
            
    type Run = struct end

    [<Struct>]
    type Ping8 = {
        x0 : int
        x1 : int
        }

    [<Struct>]
    type Pong8 = {
        y0 : int
        y1 : int
        }

    let runPingPong onPing onPong initialCount throughput iterations =
        let ping = { x0 = 1; x1 = 1 }
        let pong = { y0 = 1; y1 = 1 }
        let mutable count = 0
        let mutable total = 0
        let a = ActorSet()
        a.Register(1, throughput, fun h ->
            h.On<Run> <| fun e ->
                for i = 1 to initialCount do
                    e.Send(2, ping)
            h.On<Pong8> <| fun e ->
                total <- total + 1
                onPing(e)
                e.Respond(ping))
        a.Register(2, throughput, fun h ->
            h.On<Ping8> <| fun e -> 
                count <- count + 1
                if count <= iterations then
                    total <- total + 1
                    onPong e
                    e.Respond(pong))
        a.Send(Addresses.init 0 1, Run())
        a.Run()
        total

    let runPingPongBatch onPing onPong initialCount batchSize throughput iterations =
        let ping = { x0 = 1; x1 = 1 }
        let pong = { y0 = 1; y1 = 1 }
        let mutable count = 0
        let mutable total = 0
        let a = ActorSet()
        a.Register(1, throughput, fun h ->
            h.On<Run> <| fun e ->
                for i = 1 to initialCount do
                    use b = e.BeginSend(2)
                    for i = 1 to batchSize do
                        b.Add ping
            h.OnAll<Pong8> <| fun e ->
                use b = e.BeginRespond()
                for msg in e.message do
                    total <- total + 1
                    onPing msg
                    b.Add ping)
        a.Register(2, throughput, fun h ->
            h.OnAll<Ping8> <| fun e -> 
                count <- count + 1
                if count <= iterations then
                    use b = e.BeginRespond()
                    for msg in e.message do
                        total <- total + 1
                        onPong msg
                        b.Add pong)
        a.Send(Addresses.init 0 1, Run())
        a.Run()
        total
    
    let test() =
        printfn "%d" <| runPingPongBatch ignore ignore 100 100 100 500000
        printfn "%d" <|runPingPong ignore ignore 1 1 3000000
        printfn "%d" <|runPingPong ignore ignore 100 10 3000000
        printfn "%d" <|runPingPong (printfn "Ping %A") (printfn "Pong %A") 1 1 10

#time
Proto4.test()
#time
