namespace Garnet.Composition

open System
open System.Buffers
open System.Diagnostics
open System.Threading
open System.Collections.Generic
open System.Runtime.InteropServices

/// Defines which kind of thread an actor can be executed on
type DispatcherType =
    /// Actor which can be run on a background thread
    | Background = 0
    /// Actor which must be run on the main thread
    | Foreground = 1

type DispatcherDescriptor = {
    DispatcherType : DispatcherType
    /// Optional, useful to see thread name when printing, debugging, or profiling.
    Name : string
    /// For background dispatchers, the number of worker threads
    ThreadCount : int
    /// Max number of batches to process for an actor before releasing lock. When
    /// the number of actors exceeds the number of workers, lower values increase 
    /// fairness while higher values reduce overhead from locking and other worker
    /// queue operations.
    Throughput : int
    } with

    // Allow at least one background thread
    static member DefaultThreadCount =
        Environment.ProcessorCount - 1 |> max 1

    /// Pool of background threads
    static member Background = {
        DispatcherType = DispatcherType.Background
        Name = "Background"
        ThreadCount = DispatcherDescriptor.DefaultThreadCount
        Throughput = 100
        }

    /// Single foreground thread
    static member Foreground = {
        DispatcherType = DispatcherType.Foreground
        Name = "Foreground"
        ThreadCount = 0
        Throughput = 100
        }

    /// Single dedicated background thread
    static member Dedicated = {
        DispatcherType = DispatcherType.Background
        Name = "Dedicated"
        ThreadCount = 1
        Throughput = 1000
        }

type ActorSystemConfiguration = {
    /// The last dispatcher in the list is used if an actor defines 
    /// a dispatcher ID that does not exist
    Dispatchers : DispatcherDescriptor[]
    } with
    
    static member SingleThread = {
        Dispatchers = [| 
            DispatcherDescriptor.Foreground 
            |]
        }
    
    static member Create(workerCount) = 
        if workerCount = 0 then ActorSystemConfiguration.SingleThread
        else {
            Dispatchers = [| 
                { DispatcherDescriptor.Background with ThreadCount = workerCount }
                DispatcherDescriptor.Foreground
                |]
            }
        
    static member Default =
        ActorSystemConfiguration.Create(DispatcherDescriptor.DefaultThreadCount)

type ActorException(sourceId : ActorId, destId : ActorId, msg : string, innerEx : Exception) =
    inherit Exception(msg, innerEx)
    member _.SourceId = sourceId
    member _.DestinationId = destId
           
[<AutoOpen>]
module internal Pooling =                            
    module TypeInfo =
        let mutable private count = 0
        let next() = Interlocked.Increment(&count) - 1

    type TypeInfo<'a>() =
        static let mutable id = TypeInfo.next()
        static member Id = id

[<AutoOpen>]
module internal Processing =
    [<Struct>]
    type QueuedMessage = {
        Buffer : obj
        Count : int
        SourceId : ActorId
        DestinationId : ActorId
        Pool : obj
        }
    
    // Stateless
    type IDeliverer =
        abstract Deliver : IInbox * IOutbox * QueuedMessage -> unit        

    // Stateless
    type Deliverer<'a>() =
        static let instance = Deliverer<'a>() :> IDeliverer
        static member Instance = instance
        interface IDeliverer with
            member c.Deliver(inbox, outbox, message) =
                let buffer = message.Buffer :?> 'a[]
                let pool = message.Pool :?> ArrayPool<'a>
                try
                    try
                        inbox.Receive(outbox, {
                            Buffer = buffer
                            Count = message.Count
                            SourceId = message.SourceId
                            DestinationId = message.DestinationId
                            Pool = pool
                            })
                    with
                    | ex -> 
                        let msg = $"Processing failed:\n%A{buffer.[0]}"
                        raise (exn(msg, ex))
                finally
                    pool.Return(buffer)
                        
    // Thread-safe
    type IDispatcher =
        inherit IDisposable
        abstract Process : bool -> bool
        abstract Enqueue : ActorProcessor -> unit
        abstract OnException : ActorException -> unit
        abstract ToString : IStringBlockWriter -> unit

    // Thread-safe
    and ActorProcessor(actorId, inbox : IInbox, dispose, dispatcher : IDispatcher) =
        let processSync = obj()
        let queueSync = obj()
        let queue = Queue<struct(QueuedMessage * IDeliverer)>()
        let mutable inbox = inbox
        let mutable maxQueued = 0
        let mutable total = 0
        let mutable waitDuration = 0L
        let mutable waitCount = 0
        // returns number of messages before processing
        let run outbox =
            // lock for duration of dequeue, which ends
            // in handler process method
            Monitor.Enter(queueSync)
            let remaining = queue.Count
            if remaining = 0 then Monitor.Exit queueSync
            else
                let struct(message, deliverer) = queue.Dequeue()
                Monitor.Exit(queueSync)
                try
                    deliverer.Deliver(inbox, outbox, message)
                with
                | ex ->
                    // Halt any further processing of actor
                    inbox <- NullInbox.Instance
                    // Report exception
                    let msg = $"Actor %d{actorId} failed"
                    let actorEx = ActorException(message.SourceId, message.DestinationId, msg, ex)
                    dispatcher.OnException(actorEx)
            remaining
        member c.ActorId = actorId
        member c.WaitDuration = waitDuration
        member c.Enqueue<'a>(message : Message<'a>) =
            let queued : QueuedMessage = {
                SourceId = message.SourceId
                DestinationId = message.DestinationId
                Count = message.Count
                Buffer = message.Buffer
                Pool = message.Pool
            }
            let entry = struct(queued, Deliverer<'a>.Instance)
            Monitor.Enter(queueSync)
            queue.Enqueue(entry)
            // This check is an optimization to avoid queuing actors repeatedly in dispatchers,
            // but it relies on dispatchers taking full responsibility to ensure all messages
            // are handled. Otherwise, the actor will be orphaned and continue to accumulate 
            // messages without a dispatcher to run it.
            let r = if queue.Count = 1 then ValueSome dispatcher else ValueNone
            maxQueued <- max maxQueued queue.Count
            Monitor.Exit queueSync
            r            
        member private c.Process(outbox, throughput) =
            if not (Monitor.TryEnter processSync) then
                // This is a case where actor is in more than one dispatcher queue and
                // contention has occurred. This should only occur when a worker has
                // completed all messages and has already decided to release its lock
                // when another message is enqueued, causing a second worker to pick it
                // up and attempt to process it (before the first has released lock).
                let start = Stopwatch.GetTimestamp()
                Monitor.Enter processSync
                let stop = Stopwatch.GetTimestamp()
                waitDuration <- waitDuration + stop - start
                waitCount <- waitCount + 1
            // using remaining count to avoid locking again
            // when throughput >1
            let mutable remaining = 1
            let original = total
            let target = total + throughput
            while total < target && remaining > 0 do
                // get count in queue prior to dequeuing
                remaining <- run outbox
                if remaining > 0 then
                    // count the batch that was just processed
                    total <- total + 1
                    remaining <- remaining - 1
            let count = total - original
            Monitor.Exit processSync
            count
        /// Takes total byref to allow frequent updating in case this takes a long
        /// time to return.
        member c.ProcessAll(outbox, throughput, total : byref<int64>) =
            let mutable count = 0
            let mutable pending = true
            while pending do
                let delta = c.Process(outbox, throughput)
                count <- count + delta
                total <- total + int64 delta
                pending <- delta > 0
            count
        member c.Dispose() =
            lock processSync (fun () ->
                lock queueSync (fun () -> queue.Clear())
                dispose()
                )     
        override c.ToString() =
            $"Actor %d{actorId}: %d{total} batches processed (%d{waitCount} waits, %d{waitDuration} ticks), %d{maxQueued} max queued"
        interface IDisposable with
            member c.Dispose() = c.Dispose()
                            
    // Thread-safe
    type IDispatcherLookup =
        abstract GetDispatcher : int -> IDispatcher

    // Thread-safe
    type SharedActorMap(lookup : IDispatcherLookup) =
        let sync = obj()
        let dict = Dictionary<int, ActorProcessor>()
        let actors = List<ActorProcessor>()
        let factories = ActorFactoryCollection()
        member c.Count =
            actors.Count
        member c.Register(factory : IActorFactory) =
            Monitor.Enter sync
            factories.Add(factory)
            Monitor.Exit sync
        member c.GetProcessor(destId) =           
            Monitor.Enter sync
            let r =
                match dict.TryGetValue(destId) with
                | true, x -> x
                | false, _ ->
                    let actor = factories.Create(ActorId destId)
                    let proc = 
                        new ActorProcessor(destId, 
                            actor.Inbox, 
                            actor.Dispose, 
                            lookup.GetDispatcher(actor.DispatcherId))
                    actors.Add(proc)
                    dict.Add(destId, proc)
                    proc
            Monitor.Exit sync
            r
        member c.Dispose() =
            lock sync <| fun () ->
                for proc in actors do
                    proc.Dispose()
        interface IDisposable with
            member c.Dispose() = c.Dispose()
        member c.ToString(writer : IStringBlockWriter) =
            lock sync <| fun () ->
                let count = min dict.Count 20
                let ordered = 
                    dict.Values 
                    |> Seq.sortBy (fun a -> -a.WaitDuration, a.ActorId) 
                    |> Seq.take count
                if writer.BeginList("Actors", actors.Count) then
                    for actor in ordered do
                        writer.Write(actor.ToString())
                    writer.End()
        override c.ToString() =
            StringBlockWriter.Format(c.ToString)
                
[<AutoOpen>]
module internal Dispatchers =
    // Thread-safe
    type DispatchQueue(owner : IDispatcher) =
        let mutable maxCount = 0
        let queue = Queue<ActorProcessor>()
        let signal = new ManualResetEventSlim(false)
        let sync = obj()
        member c.Wait() =
            signal.Wait()
        member c.GetCount() =
            Monitor.Enter(sync)
            let r = queue.Count
            Monitor.Exit(sync)
            r
        member c.Enqueue(x) =
            Monitor.Enter(sync)
            queue.Enqueue(x)
            maxCount <- max queue.Count maxCount
            if queue.Count = 1 then signal.Set()
            Monitor.Exit(sync)
        member c.TryDequeue([<Out>] item : byref<_>) =
            Monitor.Enter(sync)
            let r = queue.Count > 0
            // note we don't reset when queue is empty, which can be when disposing
            let canReset = queue.Count = 1
            if r then item <- queue.Dequeue()
            if canReset then signal.Reset()
            Monitor.Exit(sync)
            r        
        member c.Enqueue(actor : ActorProcessor, message : Message<'a>) =
            match actor.Enqueue<'a>(message) with
            | ValueNone -> ()
            | ValueSome dispatcher ->
                if obj.ReferenceEquals(dispatcher, owner) 
                    then c.Enqueue(actor)
                    else dispatcher.Enqueue(actor)
        member c.Dispose() =
            Monitor.Enter(sync)            
            queue.Clear()
            signal.Set()
            signal.Dispose()
            Monitor.Exit(sync)
        interface IDisposable with
            member c.Dispose() = c.Dispose()
        override c.ToString() =
            lock sync <| fun () -> $"%d{maxCount} actors max queued"

    // Not thread-safe
    type LocalOutbox(name, queue : DispatchQueue, actorMap : SharedActorMap) =
        let actors = Dictionary<int, ActorProcessor>()
        member private c.Get(destId) =
            match actors.TryGetValue(destId) with
            | true, actor -> actor
            | false, _ -> 
                let actor = actorMap.GetProcessor(destId)
                actors.Add(destId, actor)
                actor
        member val ActiveId = 0 with get, set
        member c.SendAll<'a>(message : Message<'a>, deliveryId : ActorId) =
            let processor = c.Get(deliveryId.Value)
            let message = if message.SourceId.Value = 0 then { message with SourceId = ActorId c.ActiveId } else message
            queue.Enqueue(processor, message)
        interface IOutbox with
            member c.SendAll<'a>(message, deliveryId) =
                c.SendAll<'a>(message, deliveryId)
        override c.ToString() =
            $"{name}: %d{actors.Count} outbox actors"

    // Thread-safe
    type SharedOutbox(actors : SharedActorMap) =
        member c.SendAll<'a>(message : Message<'a>, deliveryId : ActorId) =
            let actor = actors.GetProcessor(deliveryId.Value)
            match actor.Enqueue<'a>(message) with
            | ValueSome dispatcher -> dispatcher.Enqueue(actor)
            | ValueNone -> ()
        interface IOutbox with
            member c.SendAll<'a>(message, deliveryId) =
                c.SendAll<'a>(message, deliveryId)

    [<Struct>]
    type WorkerStatus = {
        QueuedCount : int
        ProcessedCount : int64
        }

    module WorkerStatus =
        let empty = {
            QueuedCount = 0
            ProcessedCount = 0L
            }

        let add a b = {
            QueuedCount = a.QueuedCount + b.QueuedCount
            ProcessedCount = a.ProcessedCount + b.ProcessedCount
            }                

        let isRunning s1 s2 =
            s1.ProcessedCount <> s2.ProcessedCount ||
            s2.QueuedCount > 0

    // Thread-safe
    type private Worker(owner : IDispatcher, actorMap, name, workerId, throughput, workers : Worker[]) =
        let queue = new DispatchQueue(owner)
        let outbox = LocalOutbox(workerId.ToString(), queue, actorMap)
        let sync = obj()
        let mutable active = Unchecked.defaultof<ActorProcessor>
        let mutable isRunning = true
        let mutable waits = 0
        let mutable total = 0L
        let runStealing() =
            // Note we are stealing actors, not individual batches of messages. This way
            // we avoid situations where a actor that takes a long time to process spreads 
            // across and blocks all workers.
            let mutable found = false
            if workers.Length > 1 then
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
                count <- count + active.ProcessAll(outbox, throughput, &total)
                outbox.ActiveId <- 0
                active <- Unchecked.defaultof<_>
            count > 0
        let runAll() =
            runActive() || 
            runDequeue() ||
            runStealing()
        let run() =
            try
                while isRunning do
                    Monitor.Enter(sync)
                    while isRunning && runAll() do ()
                    Monitor.Exit(sync)
                    queue.Wait()
                    waits <- waits + 1
            with ex ->
                printfn "Failed %s" <| ex.ToString()
        let thread = 
            let t = Thread(run)
            t.Name <- $"{name} %d{workerId}"
            t
        member c.TryDequeue([<Out>] item : byref<_>) =
            queue.TryDequeue(&item)
        member c.Start() =
            thread.Start()
        member c.Dispose() =
            isRunning <- false
            queue.Dispose()
            thread.Join()
        member c.GetStatus() =
            Monitor.Enter sync
            let r = {
                QueuedCount = queue.GetCount()
                ProcessedCount = total
                }
            Monitor.Exit sync
            r
        member c.Enqueue(x : ActorProcessor) =
            queue.Enqueue(x)
        member c.ToString(writer : IStringBlockWriter) =
            let name = $"{thread.Name} (%d{total} batches processed, %d{waits} waits)"
            let id = thread.Name
            if writer.Begin(name, id) then
                writer.Write(outbox.ToString())
                writer.Write(queue.ToString())
                writer.End()
        override c.ToString() =
            StringBlockWriter.Format(c.ToString)
        interface IDisposable with
            member c.Dispose() = c.Dispose()

    // Thread-safe
    type WorkerDispatcher(actorMap, name, workerCount, throughput, onException) as c =
        let workers = 
            // round up to pow2 size so we can avoid modulus
            let size = Bits.getNextPow2 workerCount
            let arr = Array.zeroCreate size
            // create workers
            for i = 0 to workerCount - 1 do
                arr.[i] <- new Worker(c, actorMap, name, i, throughput, arr)
            // fill in remaining by distributing original workers
            for i = workerCount to arr.Length - 1 do
                arr.[i] <- arr.[i % workerCount]
            // start separately since all workers must be created first
            for i = 0 to workerCount - 1 do
                arr.[i].Start()            
            arr
        let getStatus() =
            // get aggregate status of all workers
            let mutable status = WorkerStatus.empty
            for i = 0 to workerCount - 1 do
                status <- WorkerStatus.add status (workers.[i].GetStatus())
            status
        interface IDispatcher with
            member c.Process(waitThreads) = 
                // pow2 size to avoid modulus
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
                            Thread.Sleep(1)
                count > 0
            member c.Enqueue(actor) =
                // Distribute according to actor ID. If ID is uniformly
                // distributed, this is round-robin. Even if distribution is
                // not uniform, expect other workers to steal as needed. Note 
                // worker array is a pow2 size.
                // For waiting/polling, we could just always add to the first
                // worker queue and rely on stealing, so this is really more 
                // for use of signals instead of wait polling.
                let index = actor.ActorId &&& (workers.Length - 1)
                workers.[index].Enqueue(actor)
            member c.OnException(ex) =
                onException ex
            member c.Dispose() =
                for i = 0 to workerCount - 1 do
                    workers.[i].Dispose()
            member c.ToString(writer) =
                c.ToString(writer)
        member c.ToString(writer : IStringBlockWriter) =
            if writer.BeginList("Workers", workerCount) then
                for i = 0 to workerCount - 1 do
                    workers.[i].ToString(writer)
                writer.End()
        override c.ToString() =
            StringBlockWriter.Format(c.ToString)

    // Thread-safe
    type Dispatcher(actorMap, name, throughput) as c =
        let queue = new DispatchQueue(c)
        let outbox = LocalOutbox(name, queue, actorMap)
        let sync = obj()
        let mutable total = 0L
        interface IDispatcher with
            member c.Process _ =
                // Process on current thread until queue is empty
                Monitor.Enter sync
                let mutable actor = Unchecked.defaultof<_>
                let mutable count = 0
                while queue.TryDequeue &actor do
                    outbox.ActiveId <- actor.ActorId
                    count <- count + actor.ProcessAll(outbox, throughput, &total)
                    outbox.ActiveId <- 0
                Monitor.Exit sync
                count > 0
            member c.Enqueue(actor) =
                queue.Enqueue actor
            member c.OnException(ex) =
                raise ex
            member c.Dispose() =
                queue.Dispose()
            member c.ToString(writer) =
                c.ToString(writer)
        member c.ToString(writer : IStringBlockWriter) =
            let name = $"{name} (%d{total} processed)"
            let id = name
            if writer.Begin(name, id) then
                writer.Write(outbox.ToString())
                writer.Write(queue.ToString())
                writer.End()
        override c.ToString() =
            StringBlockWriter.Format(c.ToString)

    // Thread-safe
    type CurrentDispatcher(actorMap, name, throughput) =
        let outbox = SharedOutbox(actorMap)
        let sync = obj()
        let mutable total = 0L
        interface IDispatcher with
            member c.Process _ = false
            member c.Enqueue(actor) =
                Monitor.Enter sync
                actor.ProcessAll(outbox, throughput, &total) |> ignore
                Monitor.Exit sync
            member c.OnException(ex) =
                raise ex
            member c.Dispose() = ()
            member c.ToString(writer) =
                c.ToString(writer)
        member c.ToString(writer : IStringBlockWriter) =
            let name = $"{name} (%d{total} processed)"
            let id = name
            if writer.Begin(name, id) then
                writer.End()
        override c.ToString() =
            StringBlockWriter.Format(c.ToString)

    /// Thread-safe if dispatchers do not change
    type DispatcherLookup(dispatcherCount) =
        let lookup = Array.zeroCreate<IDispatcher> dispatcherCount
        let runOnce waitThreads =
            // go once through dispatchers, waiting until
            // they individually have no work remaining, then
            // return whether work was done
            let mutable pending = false
            for d in lookup do 
                pending <- d.Process(waitThreads) || pending
            pending
        member c.SetDispatcher(dispatcherId, dispatcher) =
            lookup.[dispatcherId] <- dispatcher
        member c.GetDispatcher(dispatcherId) =
            // assume last entry is the fallback
            let index = min dispatcherId (lookup.Length - 1)                
            lookup.[index]
        interface IDispatcherLookup with
            member c.GetDispatcher execution =
                c.GetDispatcher execution
        member c.Process(waitThreads) =
            // keep running until no additional work is done
            // any blocking should be done at lower level like worker
            let mutable s1 = runOnce(waitThreads)
            let mutable s2 = runOnce(waitThreads)
            while s1 || s2 do
                s1 <- s2
                s2 <- runOnce(waitThreads)
        /// Runs until all foreground work is done
        member c.Process() = 
            c.Process(false)
        /// Sleep/poll while background threads complete
        member c.ProcessAll() = 
            c.Process(true)
        member c.Dispose() =
            for d in lookup do 
                d.Dispose()                    
        interface IDisposable with
            member c.Dispose() = c.Dispose()
        member c.ToString(writer : IStringBlockWriter) =
            if writer.BeginList("Dispatchers", lookup.Length) then
                for dispatcher in lookup do
                    dispatcher.ToString(writer)
                writer.End()
        override c.ToString() =
            StringBlockWriter.Format(c.ToString)

    type DispatcherLookup with
        member c.SetDispatchers(actors, onWorkerError, dispatchers : _[]) =
            for i = 0 to dispatchers.Length - 1 do
                let desc = dispatchers.[i]
                let dispatcher : IDispatcher = 
                    match desc.DispatcherType with
                    | DispatcherType.Foreground -> 
                        new Dispatcher(actors, desc.Name, desc.Throughput) :> IDispatcher
                    | DispatcherType.Background | _ -> 
                        if desc.ThreadCount <= 0 
                        then new Dispatcher(actors, desc.Name, desc.Throughput) :> IDispatcher
                        else 
                            new WorkerDispatcher(actors, desc.Name, desc.ThreadCount, 
                                desc.Throughput, onWorkerError) :> IDispatcher                            
                c.SetDispatcher(i, dispatcher)

type IMessagePump =
    inherit IOutbox
    inherit IDisposable
    abstract member Process : unit -> unit
    abstract member ProcessAll : unit -> unit

type internal ExceptionHandlers() =
    let handlers = List<Action<ActorException>>()
    let sync = obj()
    member private c.Remove(handler) =
        lock sync (fun () ->
            handlers.Remove(handler) |> ignore)        
    member c.Handle(ex) =
        lock sync (fun () ->
            for handler in handlers do
                handler.Invoke(ex))
    member c.Add(handler) =
        lock sync (fun () ->
            handlers.Add(handler))        
        new Disposable(fun () -> c.Remove(handler)) :> IDisposable

// Thread-safe
type ActorSystem(config) =
    let dispatchers = new DispatcherLookup(config.Dispatchers.Length)
    let actors = new SharedActorMap(dispatchers)
    let outbox = SharedOutbox(actors)
    let handlers = ExceptionHandlers()
    do dispatchers.SetDispatchers(actors, handlers.Handle, config.Dispatchers)
    new() = new ActorSystem(ActorSystemConfiguration.Default)
    new(workerCount) = new ActorSystem(ActorSystemConfiguration.Create(workerCount))
    member c.ActorCount =
        actors.Count
    member c.Register(factory : IActorFactory) =
        actors.Register(factory)
    member c.RegisterExceptionHandler(onException) =
        handlers.Add(onException)
    member c.Dispose() =
        dispatchers.Dispose()
        actors.Dispose()
    /// Runs until all foreground work is done
    member c.Process() = 
        dispatchers.Process()
    /// Sleep/poll while background threads complete
    member c.ProcessAll() =
        dispatchers.ProcessAll()
    member c.SendAll<'a>(message, deliveryId) =
        outbox.SendAll<'a>(message, deliveryId)
    interface IMessagePump with
        member c.SendAll<'a>(message, deliveryId) = c.SendAll<'a>(message, deliveryId)
        member c.Process() = c.Process()
        member c.ProcessAll() = c.ProcessAll()
        member c.Dispose() = c.Dispose()
    member c.ToString(writer : IStringBlockWriter) =
        actors.ToString(writer)
        dispatchers.ToString(writer)
    override c.ToString() =
        $"Actor system\n  %s{Format.addIndent (actors.ToString())}%s{Format.addIndent (dispatchers.ToString())}"
    static member CreateSingleThread() =
        new ActorSystem(ActorSystemConfiguration.SingleThread)

type NullMessagePump() =
    static let mutable instance = new NullMessagePump() :> IMessagePump
    static member Instance = instance
    interface IMessagePump with
        member c.SendAll<'a>(message, deliveryId) = NullOutbox.Instance.SendAll<'a>(message, deliveryId)
        member c.Process() = ()
        member c.ProcessAll() = ()
        member c.Dispose() = ()

[<Struct>]
type ActorReference = {
    ActorId : ActorId
    MessagePump : IMessagePump
    } with
    override c.ToString() = c.ActorId.ToString()
    static member Null = {
        ActorId = ActorId.Undefined
        MessagePump = NullMessagePump.Instance
        }

[<AutoOpen>]
module ActorSystem =
    type IMessagePump with
        member c.Get id = {
            ActorId = id
            MessagePump = c
            }

        member c.Process<'a>(destId, msg : 'a) =
            c.Send(destId, msg)
            c.Process()

        member c.Process<'a>(destId, sourceId, msg : 'a) =
            c.Send(destId, sourceId, msg)
            c.Process()

    type ActorSystem with
        member c.Register(factories : IActorFactory seq) =
            for f in factories do
                c.Register(f)

        member c.Register(tryCreate : ActorId -> Actor voption) =
            c.Register(ActorFactory.Create(tryCreate))

        /// Creates for any actor ID
        member c.Register(create : ActorId -> Actor) =
            c.Register(ActorFactory.Create(create))

        /// Creates conditionally for actor ID
        member c.Register(predicate : ActorId -> bool, create : ActorId -> Actor) =
            c.Register(ActorFactory.Create(predicate, create))

        /// Creates for a specific actor ID
        member c.Register(actorId : ActorId, create : ActorId -> Actor) =
            c.Register(ActorFactory.Create(actorId, create))

    /// Extensions for Container
    type ActorSystem with
        /// Creates conditionally for actor ID
        member c.Register(predicate, dispatcherId, register : Container -> IDisposable) =
            let create createId =
                let inbox = new LazyContainerInbox(createId, register)
                Actor(inbox, dispatcherId, inbox.Dispose)
            c.Register(predicate, create)

        member c.Register(predicate, register : Container -> IDisposable) =
            c.Register(predicate, 0, register)
            
        /// Creates for any actor ID
        member c.Register(dispatcherId, register : Container -> IDisposable) =
            let predicate (_ : ActorId) = true
            c.Register(predicate, dispatcherId, register)

        member c.Register(register : Container -> IDisposable) =
            c.Register(0, register)

        /// Creates for a specific actor ID
        member c.Register(actorId : ActorId, dispatcherId, register : Container -> IDisposable) =
            c.Register((=)actorId, dispatcherId, register)

        member c.Register(actorId : ActorId, register : Container -> IDisposable) =
            c.Register(actorId, 0, register)
    
    type ActorReference with
        member c.Send(msg) =
            c.MessagePump.Send(c.ActorId, msg)

        member c.Send(msg, sourceId) =
            c.MessagePump.Send(c.ActorId, sourceId, msg)

        member c.SendAll(span) =
            c.MessagePump.SendAll(c.ActorId, span)

        member c.SendAll<'a>(sourceId, span : ReadOnlySpan<'a>) =
            c.MessagePump.SendAll(c.ActorId, sourceId, span)

        member c.Process msg =
            c.MessagePump.Process(c.ActorId, msg)
    