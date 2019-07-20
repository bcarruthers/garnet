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
    type ISender =
        abstract Send<'a> : int * 'a -> unit

    type IProcessor =
        abstract Process : obj * ISender -> unit

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
            member c.Process(sync, send) =
                let msg = inbox.Dequeue()
                Monitor.Exit sync
                handle { message = msg; system = send }

    type Actor(handlers : Dictionary<Type, IProcessor>) =
        let queue = Queue<_>()
        let queueSync = obj()
        let procSync = obj()
        member c.Enqueue<'a> (msg : 'a) =
            match handlers.TryGetValue(typeof<'a>) with
            | true, h -> 
                let handler = h :?> Handler<'a>
                Monitor.Enter queueSync
                handler.Enqueue msg
                queue.Enqueue (handler :> IProcessor)
                Monitor.Exit queueSync
            | false, _ -> ()
        member private c.ProcessOne (send : ISender) =
            // lock for duration of dequeue, which ends
            // in handler process method
            Monitor.Enter queueSync
            if queue.Count > 0 then
                let proc = queue.Dequeue()
                proc.Process(queueSync, send)
                true
            else
                Monitor.Exit queueSync
                false
        member c.Process (sender : ISender) =
            let mutable count = 0
            try
                Monitor.Enter procSync
                try
                    while c.ProcessOne sender do
                        count <- count + 1
                finally
                    Monitor.Exit procSync
            with ex ->
                sender.Send(0, ex)                
            count > 0

    type ActorSet(mapId, actors : Dictionary<int, Actor>) =
        member c.Send<'a>(destId, msg : 'a) =
            let mappedId = mapId destId
            match actors.TryGetValue(mappedId) with
            | true, actor -> actor.Enqueue msg
            | false, _ -> ()
        member c.RunOnce() =
            let mutable handled = false
            for actor in actors.Values do
                handled <- actor.Process c || handled
            handled
        member c.Run() =
            while c.RunOnce() do ()
        interface ISender with
            member c.Send(destId, msg) = c.Send(destId, msg)

    type ActorBuilder() =
        let mutable handlers = Dictionary<Type, IProcessor>()
        member c.On<'a>(handle : Mail<'a> -> unit) =
            let handler = Handler<'a>(handle)
            handlers.Add(typeof<'a>, handler :> IProcessor)
        member c.Build() =
            let a = Actor(handlers)
            handlers <- Dictionary<_,_>()
            a

    type ActorSetBuilder() =
        let mutable actors = Dictionary<int, Actor>()
        let mutable mapId = id
        member c.Register(newMapId) =
            mapId <- newMapId << mapId
        member c.Register(id, actor : Actor) =
            actors.Add(id, actor)
        member c.Build() =
            let a = ActorSet(mapId, actors)
            actors <- Dictionary<_,_>()
            a

    let runPingPong onPing onPong iterations =
        let mutable count = 0
        let a = ActorSetBuilder()
        a.Register(1, 
            let h = ActorBuilder()
            h.On<Run> <| fun e ->
                e.system.Send(2, Ping())
            h.On<Pong> <| fun e ->
                count <- count + 1
                if count < iterations then
                    onPing(e)
                    e.system.Send(2, Ping())
            h.Build())
        a.Register(2,
            let h = ActorBuilder()
            h.On<Ping> <| fun e -> 
                onPong e
                e.system.Send(1, Pong())
            h.Build())
        let a = a.Build()
        a.Send(1, Run())
        a.Run()
    
    let test() =
        runPingPong ignore ignore 5000000
        //runPingPong (printfn "Ping %A") (printfn "Pong %A") 10

#time
Proto4.test()
#time
