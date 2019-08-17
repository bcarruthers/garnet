namespace Garnet.Composition

open System
open System.Threading
open System.Collections.Generic
open System.Runtime.InteropServices
open Garnet
open Garnet.Formatting

[<AutoOpen>]
module internal Sending =           
    type internal IOutboxSender =
        abstract member Send<'a> : Message<'a> -> unit

    and internal NullOutboxSender() =
        static let mutable instance = NullOutboxSender() :> IOutboxSender
        static member Instance = instance
        interface IOutboxSender with
            member c.Send batch = ()
           
    // Not thread-safe, but intended for read-only sharing 
    // among threads with thread-safe refcounting to ensure
    // only one thread clears.
    and internal Message<'a>() =
        let mutable buffer = Array.zeroCreate<'a> 1
        let mutable recipients = Array.zeroCreate<int> 1
        let mutable bufferLength = 0
        let mutable recipientCount = 0
        let mutable receiveCount = 0
        let mutable sourceId = ActorId.undefined
        member c.SourceId = sourceId
        member c.Buffer = Buffer.ofArrayStart buffer bufferLength
        member c.Recipients = Buffer.ofArrayStart recipients recipientCount
        member val Sender = NullOutboxSender.Instance with get, set
        member val OriginPool : obj = null with get, set
        member val OriginStack : Stack<Message<'a>> = null with get, set
        member c.SetSource id = 
            sourceId <- id
        member c.IncrementReceived() =
            recipientCount = 1 || 
            Interlocked.Increment(&receiveCount) = recipientCount
        member c.Clear() =
            receiveCount <- 0
            c.OriginPool <- null
            c.OriginStack <- null                    
            c.Sender <- NullOutboxSender.Instance
            sourceId <- ActorId.undefined
            bufferLength <- 0
            recipientCount <- 0
        interface IMessageWriter<'a> with
            member c.SetSource id = 
                sourceId <- id
            member c.AddRecipient x =
                Buffer.addToArray &recipientCount &recipients x.value
            member c.Write x =
                Buffer.addToArray &bufferLength &buffer x
            member c.WriteAll x =
                Buffer.addAllToArray &bufferLength &buffer x
            member c.Dispose() =
                c.Sender.Send(c)            
        override c.ToString() =
            let strMessages = buffer |> Seq.map (sprintf "%A")
            sprintf "Message (%s):\n  Source: %d\n  Recipients (%d): %s\n  Buffer (%d): %s" 
                (typeToString typeof<'a>) sourceId.value
                recipientCount (String.Join(", ", recipients |> Seq.take recipientCount))
                bufferLength (String.Join(", ", strMessages |> Seq.take bufferLength))

[<AutoOpen>]
module internal Pooling =                            
    module TypeInfo =
        let mutable private count = 0
        let next() = Interlocked.Increment(&count) - 1

    type TypeInfo<'a>() =
        static let mutable id = TypeInfo.next()
        static member Id = id

    // Thread-safe
    type SharedPool<'a>() =
        let pool = Stack<Message<'a>>()
        let sync = obj()
        member c.Rent() =
            Monitor.Enter sync
            if pool.Count > 0 then 
                let r = pool.Pop()
                Monitor.Exit sync
                r
            else
                Monitor.Exit sync
                let r = new Message<'a>()
                r
        member c.Return batch =
            Monitor.Enter sync
            pool.Push batch
            Monitor.Exit sync
        override c.ToString() =
            lock sync <| fun () ->
                let count = pool.Count
                let total = pool |> Seq.sumBy (fun p -> p.Buffer.Array.Length)
                let mean = if count > 0 then total / count else 0
                sprintf "%s (%d): %d total, %d mean" 
                    (typeToString typeof<'a>)
                    count total mean

    // Thread-safe
    type SharedPool() =
        let mutable pools = Array.zeroCreate<obj> 8
        let sync = obj()
        member c.GetPool<'a>() =
            let id = TypeInfo<'a>.Id
            Monitor.Enter sync
            Buffer.resizeArray (id + 1) &pools
            let pool = pools.[id]
            let pool =
                if pool <> null then pool :?> SharedPool<'a>
                else
                    let p = SharedPool<'a>()
                    pools.[id] <- p :> obj
                    p
            Monitor.Exit sync
            pool
        override c.ToString() =
            lock sync <| fun () ->
                let pools = pools |> Seq.filter (fun p -> p <> null)
                let count = pools |> Seq.length
                let str = String.Join("\n", pools)
                formatList "Shared pools" count str

    // Local pool for use by a single thread to minimize
    // accessing shared pool.
    // Not thread-safe
    type LocalPool<'a>(owner : obj, shared : SharedPool<'a>) =
        let pool = Stack<Message<'a>>()
        let mutable rentCount = 0
        member c.Rent() =
            let r =
                if pool.Count > 0 then pool.Pop()
                else 
                    rentCount <- rentCount + 1
                    shared.Rent()
            r.OriginPool <- owner
            r.OriginStack <- pool
            r
        member c.Return (x : Message<'a>) =
            // Only return to local pools that have rented
            // out this type, otherwise we could have batches
            // accumulate that are never sent out from actors
            // within the current dispatcher pool.
            if pool.Count < rentCount then 
                pool.Push x
            else shared.Return x
        override c.ToString() =
            let count = pool.Count
            let total = pool |> Seq.sumBy (fun p -> p.Buffer.Array.Length)
            let mean = if count > 0 then total / count else 0
            sprintf "%s (%d): %d total, %d mean" 
                (typeToString typeof<'a>)
                count total mean

    // Not thread-safe
    type LocalPool(shared : SharedPool) =
        let mutable pools = Array.zeroCreate<obj> 8
        member c.GetPool<'a>() =
            let id = TypeInfo<'a>.Id
            Buffer.resizeArray (id + 1) &pools
            let pool = pools.[id]
            if pool <> null then pool :?> LocalPool<'a>
            else
                let p = LocalPool<'a>(c, shared.GetPool<'a>())
                pools.[id] <- p :> obj
                p
        override c.ToString() =
            let pools = pools |> Seq.filter (fun p -> p <> null)
            let count = pools |> Seq.length
            let str = String.Join("\n", pools)
            formatList "Local pools" count str

    [<AutoOpen>]
    module BatchExtensions =
        type Message<'a> with
            member c.Release(pool : LocalPool) =
                if c.IncrementReceived() then
                    let sourcePool = c.OriginPool
                    let sourceStack = c.OriginStack
                    c.Clear()        
                    // Fast path: if local pool is the same as the one
                    // batch came from, we can just return directly.
                    if obj.ReferenceEquals(pool, sourcePool) 
                        then sourceStack.Push c
                        else pool.GetPool<'a>().Return c

[<AutoOpen>]
module internal Processing =
    // Stateless
    type IDeliverer =
        abstract Deliver : LocalPool * IInbox * Envelope<obj> -> unit        

    // Stateless
    type Deliverer<'a>() =
        static let instance = Deliverer<'a>() :> IDeliverer
        static member Instance = instance
        interface IDeliverer with
            member c.Deliver(pool, inbox, mail) =
                let msg = mail.message :?> Message<'a>
                try
                    try
                        inbox.Receive {
                            sourceId = mail.sourceId
                            destinationId = mail.destinationId
                            outbox = mail.outbox
                            message = msg.Buffer
                            }
                    with
                    | ex -> 
                        let msg = sprintf "Processing failed:\n%s" (msg.ToString())
                        raise (exn(msg, ex))
                finally
                    msg.Release(pool)
                        
    // Thread-safe
    type IDispatcher =
        inherit IDisposable
        abstract Run : bool -> bool
        abstract Enqueue : ActorProcessor -> unit

    // Thread-safe
    and ActorProcessor(actorId, inbox : IInbox, dispose, dispatcher : IDispatcher, onError) =
        let processSync = obj()
        let queueSync = obj()
        let queue = Queue<struct(int * int * IDeliverer * obj)>()
        let mutable maxQueued = 0
        let mutable total = 0
        // returns number of messages before processing
        let run outbox pool =
            // lock for duration of dequeue, which ends
            // in handler process method
            Monitor.Enter queueSync
            let remaining = queue.Count
            if remaining = 0 then Monitor.Exit queueSync
            else
                let struct(sourceId, destId, deliverer, msg) = queue.Dequeue()
                Monitor.Exit queueSync
                try
                    deliverer.Deliver(pool, inbox, {
                        outbox = outbox
                        sourceId = ActorId sourceId
                        destinationId = ActorId destId
                        message = msg
                        })
                with
                | ex -> 
                    let msg = sprintf "Actor %d failed" actorId
                    onError (exn(msg, ex))
            remaining
        member c.ActorId = actorId
        member c.Enqueue<'a>(sourceId, destId, message : Message<'a>) =
            Monitor.Enter queueSync
            queue.Enqueue(struct(sourceId, destId, Deliverer<'a>.Instance, message :> obj))
            let r = if queue.Count = 1 then ValueSome dispatcher else ValueNone
            maxQueued <- max maxQueued queue.Count
            Monitor.Exit queueSync
            r            
        member c.Process(outbox, pool, throughput) =
            let mutable count = 0
            if Monitor.TryEnter processSync then                    
                // using remaining count to avoid locking again
                // when throughput >1
                let mutable remaining = 1
                while count < throughput && remaining > 0 do
                    // get count in queue prior to dequeuing
                    remaining <- run outbox pool
                    if remaining > 0 then
                        // count the message that was just processed
                        count <- count + 1    
                        remaining <- remaining - 1
                total <- total + count
                Monitor.Exit processSync
            count
        member c.ProcessAll(outbox, pool, throughput) =
            let mutable count = 0
            let mutable pending = true
            while pending do
                let delta = c.Process(outbox, pool, throughput)
                count <- count + delta
                pending <- delta > 0
            count
        member c.Dispose() =
            lock processSync (fun () ->
                lock queueSync (fun () -> queue.Clear())
                dispose()
                )                
        override c.ToString() =
            sprintf "Actor %d: %d processed, %d max queued"//, inbox: %s" 
                actorId total maxQueued
                //(addIndent (inbox.ToString()))
                            
    // Thread-safe
    type IDispatcherLookup =
        abstract GetDispatcher : Execution -> IDispatcher

    // Thread-safe
    type SharedActorMap(lookup : IDispatcherLookup) =
        let sync = obj()
        let actors = Dictionary<int, ActorProcessor>()
        let factory = ActorFactoryCollection()
        let onError ex =
            printfn "%s" (ex.ToString())
        let rec getProcessor destId maxDepth =
            match actors.TryGetValue(destId) with
            | true, x -> x
            | false, _ ->
                // limit depth for the case of repeated routing
                let actor = 
                    if maxDepth <= 0 then Actor.none
                    else factory.Create destId
                let proc =
                    if int actor.execution = int Execution.Route 
                    then getProcessor actor.routedId.value (maxDepth - 1)
                    else 
                        let dispatcher = lookup.GetDispatcher actor.execution
                        new ActorProcessor(destId, 
                            actor.inbox, 
                            actor.dispose, 
                            dispatcher,
                            onError)
                // replace existing to handle cycles
                actors.[destId] <- proc
                proc
        member c.Register f =
            Monitor.Enter sync
            factory.Add f
            Monitor.Exit sync
        member c.GetProcessor(destId) =           
            Monitor.Enter sync
            let r = getProcessor destId 5
            Monitor.Exit sync
            r
        member c.Dispose() =
            lock sync <| fun () ->
                for proc in actors.Values do
                    proc.Dispose()
        interface IDisposable with
            member c.Dispose() = c.Dispose()
        override c.ToString() =
            lock sync <| fun () ->
                let count = min actors.Count 20
                let ordered = 
                    actors.Values 
                    |> Seq.sortBy (fun a -> a.ActorId) 
                    |> Seq.take count
                formatList "Actors" actors.Count (String.Join("\n", ordered))
                
[<AutoOpen>]
module internal Dispatchers =
    // Thread-safe
    type DispatchQueue(owner : IDispatcher) =
        let mutable maxCount = 0
        let queue = Queue<ActorProcessor>()
        let sync = obj()
        member c.GetCount() =
            Monitor.Enter sync
            let r = queue.Count
            Monitor.Exit sync
            r
        member c.Enqueue x =
            Monitor.Enter sync
            queue.Enqueue x
            maxCount <- max queue.Count maxCount
            Monitor.Exit sync
        member c.TryDequeue([<Out>] item : byref<_>) =
            Monitor.Enter sync
            let r = queue.Count > 0
            if r then item <- queue.Dequeue()
            Monitor.Exit sync
            r        
        member c.Enqueue(actor : ActorProcessor, recipientId, message : Message<'a>) =
            match actor.Enqueue<'a>(message.SourceId.value, recipientId, message) with
            | ValueNone -> ()
            | ValueSome dispatcher ->
                if obj.ReferenceEquals(dispatcher, owner) 
                    then c.Enqueue(actor)
                    else dispatcher.Enqueue(actor)
        override c.ToString() =
            lock sync <| fun () ->
                sprintf "%d max queued" maxCount

    // Not thread-safe
    type OutboxSender(queue : DispatchQueue, actorMap : SharedActorMap) =
        let actors = Dictionary<int, ActorProcessor>()
        member private c.Get(destId) =
            match actors.TryGetValue(destId) with
            | true, actor -> actor
            | false, _ -> 
                let actor = actorMap.GetProcessor(destId)
                actors.Add(destId, actor)
                actor
        member c.Send<'a>(message : Message<'a>) =
            // Avoiding foreach since recipients will be cleared.
            let recipients = message.Recipients
            for i = 0 to recipients.Count - 1 do
                let recipientId = recipients.[i]
                queue.Enqueue(c.Get(recipientId), recipientId, message)
        interface IOutboxSender with
            member c.Send<'a>(message) =
                c.Send<'a> message
        override c.ToString() =
            sprintf "%d actors" actors.Count

    // Thread-safe
    type SharedOutbox(pool : SharedPool, actors : SharedActorMap) =
        member c.BeginSend<'a>() =
            // Use shared pool here instead of going directly
            // to worker, since its pool isn't thread-safe
            let msg = pool.GetPool<'a>().Rent()
            msg.Sender <- c
            msg :> IMessageWriter<'a>
        interface IOutbox with
            member c.BeginSend<'a>() =
                c.BeginSend<'a>()
        interface IOutboxSender with
            member c.Send<'a>(message : Message<'a>) =
                // Avoiding foreach since recipients will be cleared.
                let recipients = message.Recipients
                let sourceId = message.SourceId.value
                for i = 0 to recipients.Count - 1 do
                    let recipientId = recipients.[i]
                    // Using shared map here
                    let actor = actors.GetProcessor(recipientId)
                    match actor.Enqueue<'a>(sourceId, recipientId, message) with
                    | ValueSome dispatcher -> dispatcher.Enqueue(actor)
                    | ValueNone -> ()

    // Not thread-safe
    type LocalOutbox(name, pool : LocalPool, sender : OutboxSender) =
        member val ActiveId = 0 with get, set
        member c.BeginSend<'a>() =
            let msg = pool.GetPool<'a>().Rent()
            msg.Sender <- sender
            msg.SetSource (ActorId c.ActiveId)
            msg :> IMessageWriter<'a>
        interface IOutbox with
            member c.BeginSend<'a>() =
                c.BeginSend<'a>()
        override c.ToString() =
            name

    [<Struct>]
    type WorkerStatus = {
        queuedCount : int
        processedCount : int64
        }

    module WorkerStatus =
        let empty = {
            queuedCount = 0
            processedCount = 0L
            }

        let add a b = {
            queuedCount = a.queuedCount + b.queuedCount
            processedCount = a.processedCount + b.processedCount
            }                

        let isRunning s1 s2 =
            s1.processedCount <> s2.processedCount ||
            s2.queuedCount > 0

    // Thread-safe
    type private Worker(owner : IDispatcher, actorMap, sharedPool, workerId, throughput, workers : Worker[]) =
        let pool = LocalPool(sharedPool)
        let queue = DispatchQueue(owner)
        let sender = OutboxSender(queue, actorMap)
        let outbox = LocalOutbox(workerId.ToString(), pool, sender)
        let sync = obj()
        let mutable active = Unchecked.defaultof<ActorProcessor>
        let mutable isRunning = true
        let mutable waits = 0
        let mutable total = 0L
        let runStealing() =
            let mutable found = false
            if workers.Length > 0 then
                let mutable i = 0
                while i < workers.Length && not found do
                    if i <> workerId then
                        found <- workers.[i].TryDequeue(&active)
                    i <- i + 1
            found
        let runDequeue() =
            queue.TryDequeue(&active)
        let runActive() =
            let mutable count = 0
            if not (obj.ReferenceEquals(active, null)) then
                // run single actor until no messages remain, respecting
                // throughput param
                outbox.ActiveId <- active.ActorId
                count <- count + active.ProcessAll(outbox, pool, throughput)
                outbox.ActiveId <- 0
                active <- Unchecked.defaultof<_>
            total <- total + int64 count
            count > 0
        let runAll() =
            runActive() || 
            runDequeue() ||
            runStealing()
        let run() =
            try
                while isRunning do
                    Monitor.Enter sync
                    while isRunning && runAll() do ()
                    Monitor.Exit sync
                    Thread.Sleep 1
                    waits <- waits + 1
            with ex ->
                printfn "Failed %s" <| ex.ToString()
        let thread = 
            let t = new Thread(run)
            t.Name <- sprintf "Worker %d" workerId
            t
        member c.TryDequeue([<Out>] item : byref<_>) =
            queue.TryDequeue(&item)
        member c.Start() =
            thread.Start()
        member c.Dispose() =
            isRunning <- false
            thread.Join()
        member c.GetStatus() =
            Monitor.Enter sync
            let r = {
                queuedCount = queue.GetCount()
                processedCount = total
                }
            Monitor.Exit sync
            r
        member c.Enqueue (x : ActorProcessor) =
            queue.Enqueue x
        override c.ToString() =
            lock sync <| fun () ->
                sprintf "Worker %d: %d processed, %d waits\n  %s, %s\n  %s" 
                    workerId total waits
                    (sender.ToString())
                    (queue.ToString())
                    (addIndent (pool.ToString()))

    // Thread-safe
    type WorkerDispatcher(actorMap, pool, workerCount, throughput) as c =
        let workers = 
            let arr = Array.zeroCreate workerCount 
            for i = 0 to workerCount - 1 do
                arr.[i] <- new Worker(c, actorMap, pool, i, throughput, arr)
            // start separately since all workers must be created first
            for worker in arr do
                worker.Start()            
            arr
        let primary = workers.[0]
        let getStatus() =
            // get aggregate status of all workers
            let mutable status = WorkerStatus.empty
            for worker in workers do
                status <- WorkerStatus.add status (worker.GetStatus())
            status
        interface IDispatcher with
            member c.Run(waitThreads) = 
                let spinPeriod = 128
                let mutable count = 0
                if waitThreads then
                    // keep polling status to see if anything changed or
                    // there are queued items outstanding
                    let mutable s1 = getStatus()
                    let mutable s2 = getStatus()
                    while WorkerStatus.isRunning s1 s2 do
                        s1 <- s2
                        s2 <- getStatus()
                        count <- count + 1
                        // if we've polled a bunch of times in a row with
                        // continued activity, sleep and then resume
                        if count &&& (spinPeriod - 1) = 0 then
                            Thread.Sleep 1
                count > 0
            member c.Enqueue(actor) =
                primary.Enqueue(actor)
            member c.Dispose() =
                for worker in workers do
                    worker.Dispose()
        override c.ToString() =
            (formatList "Workers" workers.Length (String.Join("\n", workers)))

    // Thread-safe
    type Dispatcher(actorMap, sharedPool, throughput) as c =
        let pool = LocalPool(sharedPool)
        let queue = DispatchQueue(c)
        let sender = OutboxSender(queue, actorMap)
        let outbox = LocalOutbox("Main", pool, sender)
        let sync = obj()
        let mutable total = 0L
        interface IDispatcher with
            member c.Run(waitThreads) =
                // Run on current thread until queue is empty
                Monitor.Enter sync
                let mutable actor = Unchecked.defaultof<_>
                let mutable count = 0
                while queue.TryDequeue &actor do
                    outbox.ActiveId <- actor.ActorId
                    count <- count + actor.ProcessAll(outbox, pool, throughput)
                    outbox.ActiveId <- 0
                Interlocked.Add(&total, int64 count) |> ignore
                Monitor.Exit sync
                count > 0
            member c.Enqueue(actor) =
                queue.Enqueue actor
            member c.Dispose() = ()
        override c.ToString() =
            sprintf "Main: %d processed\n  %s, %s\n  %s" 
                total 
                (sender.ToString())
                (queue.ToString())
                (addIndent (pool.ToString()))

    // Thread-safe
    type CurrentDispatcher(actorMap, sharedPool, throughput) =
        let pool = LocalPool(sharedPool)
        let outbox = SharedOutbox(sharedPool, actorMap)
        let sync = obj()
        let mutable total = 0L
        interface IDispatcher with
            member c.Run(waitThreads) = false
            member c.Enqueue(actor) =
                Monitor.Enter sync
                actor.ProcessAll(outbox, pool, throughput) |> ignore
                Monitor.Exit sync
            member c.Dispose() = ()
        override c.ToString() =
            sprintf "Current: %d processed\n  %s" 
                total (addIndent (pool.ToString()))

    /// Thread-safe if dispatchers do not change
    type DispatcherLookup() =
        let maxExecutionCount = 8
        let list = List<IDispatcher>()
        let lookup = Array.zeroCreate<IDispatcher> maxExecutionCount
        let runOnce waitThreads =
            // go once through dispatchers, waiting until
            // they individually have no work remaining, then
            // return whether work was done
            let mutable pending = false
            for d in list do 
                pending <- d.Run(waitThreads) || pending
            pending
        member c.Add(execution : Execution, dispatcher) =
            lookup.[int execution] <- dispatcher
            if not (list.Contains dispatcher) then
                list.Add dispatcher
        member c.GetDispatcher execution =
            lookup.[int execution]
        interface IDispatcherLookup with
            member c.GetDispatcher execution =
                c.GetDispatcher execution
        member c.Run(waitThreads) =
            // keep running until no additional work is done
            // any blocking should be done at lower level like worker
            let mutable s1 = runOnce(waitThreads)
            let mutable s2 = runOnce(waitThreads)
            while s1 || s2 do
                s1 <- s2
                s2 <- runOnce(waitThreads)
        /// Runs until all foreground work is done
        member c.Run() = 
            c.Run(false)
        /// Sleep/poll while background threads complete
        member c.RunAll() = 
            c.Run(true)
        member c.Dispose() =
            for d in list do 
                d.Dispose()                    
        interface IDisposable with
            member c.Dispose() = c.Dispose()
        override c.ToString() =
            formatList "Dispatchers" list.Count (String.Join("\n", list))

    type DispatcherLookup with
        member c.AddDefaults(actors, pool, workerCount) =
            let throughput = 100
            let main = new Dispatcher(actors, pool, throughput)
            let none = new CurrentDispatcher(actors, pool, throughput)
            let workers = 
                if workerCount <= 0 
                then main :> IDispatcher
                else 
                    new WorkerDispatcher(actors, pool, workerCount, throughput) 
                    :> IDispatcher
            c.Add(Execution.None, none)                
            c.Add(Execution.Main, main)                
            c.Add(Execution.Default, workers)

type IMessagePump =
    inherit IOutbox
    inherit IDisposable
    abstract member Run : unit -> unit
    abstract member RunAll : unit -> unit

// Thread-safe
type ActorSystem(threadCount) =
    let pool = SharedPool()
    let dispatchers = new DispatcherLookup()
    let actors = new SharedActorMap(dispatchers)
    let outbox = SharedOutbox(pool, actors)
    do 
        dispatchers.AddDefaults(actors, pool, threadCount)
    new() = new ActorSystem(ActorSystem.DefaultThreadCount)
    member c.Register f = actors.Register f
    member c.Dispose() =
        dispatchers.Dispose()
        actors.Dispose()
    /// Runs until all foreground work is done
    member c.Run() = 
        dispatchers.Run()
    /// Sleep/poll while background threads complete
    member c.RunAll() =
        dispatchers.RunAll()
    interface IMessagePump with
        member c.BeginSend<'a>() =
            outbox.BeginSend<'a>()
        member c.Run() = c.Run()
        member c.RunAll() = c.RunAll()
        member c.Dispose() = c.Dispose()
    static member DefaultThreadCount = 
        // Allow at least one background thread by default
        Environment.ProcessorCount - 1 |> max 1
    override c.ToString() =
        sprintf "Actor system\n  %s\n  %s\n  %s"
            (addIndent (actors.ToString()))
            (addIndent (pool.ToString()))
            (addIndent (dispatchers.ToString()))

[<Struct>]
type ActorReference = {
    actorId : ActorId
    pump : IMessagePump
    } with
    override c.ToString() = c.actorId.ToString()

[<AutoOpen>]
module ActorSystem =
    type IMessagePump with
        member c.Get id = {
            actorId = id
            pump = c
            }

        member c.Run<'a>(destId, msg : 'a) =
            c.Send(destId, msg)
            c.Run()

        member c.Run<'a>(destId, msg : 'a, sourceId) =
            c.Send(destId, msg, sourceId)
            c.Run()

    type ActorSystem with
        member c.Register(actorId, inbox) =
            c.Register(fun id ->
                if id <> actorId then Actor.none
                else Actor.inbox inbox)

        member c.Register(actorId, handler) =
            c.Register(ActorFactory.handler actorId handler)

        member c.RegisterAll(factories) =
            c.Register(ActorFactory.combine factories)
                
    type ActorReference with
        member c.BeginSend<'a>() =
            c.pump.BeginSend c.actorId

        member c.Send(msg) =
            c.pump.Send(c.actorId, msg)

        member c.Send(msg, sourceId) =
            c.pump.Send(c.actorId, msg, sourceId)

        member c.SendAll(msgs) =
            c.pump.SendAll(c.actorId, msgs)

        member c.SendAll<'a>(msgs : Buffer<'a>, sourceId) =
            c.pump.SendAll(c.actorId, msgs, sourceId)

        member c.Run msg =
            c.pump.Run(c.actorId, msg)
    
type Sender = IOutbox -> unit

module Sender =
    let send actorId msg (a : IOutbox) =
        a.Send(actorId, msg)

    let sendAll actorId msg (a : IOutbox) =
        a.SendAll(actorId, msg)

    let list list =
        fun (a : IOutbox) ->
            for send in list do
                send a
