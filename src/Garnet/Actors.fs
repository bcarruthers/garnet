namespace Garnet.Composition

open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic
open System.Runtime.InteropServices
open Garnet
open Garnet.Formatting

/// Defines how an actor is executed
type DispatcherType =
    /// Actor which can be run on a background thread
    | Background = 0
    /// Actor which must be run on the main thread
    | Foreground = 1

type DispatcherDescriptor = {
    dispatcherType : DispatcherType
    /// For background dispatchers, the number of worker threads
    threadCount : int
    /// Max number of batches to process for an actor before releasing lock. When
    /// the number of actors exceeds the number of workers, lower values increase 
    /// fairness while higher values reduce overhead from locking and other worker
    /// queue operations.
    throughput : int
    }

module DispatcherDescriptor =
    // Allow at least one background thread
    let defaultThreadCount = Environment.ProcessorCount - 1 |> max 1

    /// Pool of background threads
    let background = {
        dispatcherType = DispatcherType.Background
        threadCount = defaultThreadCount
        throughput = 100
        }

    /// Single foreground thread
    let foreground = {
        dispatcherType = DispatcherType.Foreground
        threadCount = 0
        throughput = 100
        }

    /// Single dedicated background thread
    let dedicated = {
        dispatcherType = DispatcherType.Background
        threadCount = 1
        throughput = 1000
        }

type ActorSystemConfiguration = {
    /// The last dispatcher in the list is used if an actor defines 
    /// a dispatcher ID that does not exist
    dispatchers : DispatcherDescriptor[]
    }

module ActorSystemConfiguration =
    let singleThread = {
        dispatchers = [| 
            DispatcherDescriptor.foreground 
            |]
        }

    let createDefaults workerCount = 
        if workerCount = 0 then singleThread
        else {
            dispatchers = [| 
                { DispatcherDescriptor.background with threadCount = workerCount }
                DispatcherDescriptor.foreground
                |]
            }

    let defaults = createDefaults DispatcherDescriptor.defaultThreadCount

type ActorException(sourceId : ActorId, destId : ActorId, msg : string, innerEx : Exception) =
    inherit Exception(msg, innerEx)
    member _.SourceId = sourceId
    member _.DestinationId = destId

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
        let mutable recipients = Array.zeroCreate<Destination> 1
        let mutable bufferLength = 0
        let mutable recipientCount = 0
        let mutable receiveCount = 0
        let mutable sourceId = ActorId.undefined
        member c.SourceId = sourceId
        member c.Capacity = buffer.Length
        member c.Buffer = ReadOnlyMemory(buffer, 0, bufferLength)
        member c.Destinations = ReadOnlyMemory(recipients, 0, recipientCount)
        member val Sender = NullOutboxSender.Instance with get, set
        member val Recycle : Action<Message<'a>> = null with get, set
        member val OriginPool : obj = null with get, set
        member val OriginStack : Stack<Message<'a>> = null with get, set
        member c.SetSource id = 
            sourceId <- id
        member c.IncrementReceived() =
            recipientCount = 1 || 
            Interlocked.Increment(&receiveCount) = recipientCount
        member c.Clear() =
            receiveCount <- 0
            c.Recycle <- null
            c.OriginPool <- null
            c.OriginStack <- null                    
            c.Sender <- NullOutboxSender.Instance
            sourceId <- ActorId.undefined
            bufferLength <- 0
            recipientCount <- 0
        interface IMessageWriter<'a> with
            member c.SetSource(id) = 
                sourceId <- id
            member c.AddDestination(x) =
                Buffer.addToArray &recipientCount &recipients x
            member c.Advance(count) =
                bufferLength <- bufferLength + count
            member c.GetMemory(minSize) =
                let required = bufferLength + max 1 minSize
                if buffer.Length < required then
                    Buffer.resizeArray required &buffer
                Memory(buffer, bufferLength, buffer.Length - bufferLength)
            member c.GetSpan(minSize) =
                let required = bufferLength + max 1 minSize
                if buffer.Length < required then
                    Buffer.resizeArray required &buffer
                Span(buffer, bufferLength, buffer.Length - bufferLength)
            member c.Dispose() =
                if bufferLength > 0 then c.Sender.Send(c)            
                else
                    // if empty message, just return to pool without sending
                    let recycle = c.Recycle
                    c.Clear()
                    recycle.Invoke c
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
        let recycle batch =
            Monitor.Enter sync
            pool.Push batch
            Monitor.Exit sync
        let recycleAction = Action<_>(recycle)
        member c.Rent() =
            Monitor.Enter sync
            let r =
                if pool.Count > 0 then 
                    let r = pool.Pop()
                    Monitor.Exit sync
                    r
                else
                    Monitor.Exit sync
                    new Message<'a>()
            r.Recycle <- recycleAction
            r
        member c.Return batch =
            recycle batch
        override c.ToString() =
            lock sync <| fun () ->
                let count = pool.Count
                let total = pool |> Seq.sumBy (fun p -> p.Capacity)
                if count = 0 && total = 0 then""
                else
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
        member c.ToString(writer : IStringBlockWriter) =
            lock sync <| fun () ->
                let pools = pools |> Seq.filter (fun p -> p <> null)
                let count = pools |> Seq.length
                if writer.BeginList("Shared pools", count) then
                    for pool in pools do
                        let str = pool.ToString()
                        if str <> "" then writer.Write(str)
                    writer.End()
        override c.ToString() =
            StringBlockWriter.Format(c.ToString)

    // Local pool for use by a single thread to minimize
    // accessing shared pool.
    // Not thread-safe
    type LocalPool<'a>(owner : obj, shared : SharedPool<'a>) =
        let pool = Stack<Message<'a>>()
        let mutable rentCount = 0
        let recycle (x : Message<'a>) =
            // Only return to local pools that have rented
            // out this type, otherwise we could have batches
            // accumulate that are never sent out from actors
            // within the current dispatcher pool.
            if pool.Count < rentCount then 
                pool.Push x
            else shared.Return x
        let recycleAction = Action<_>(recycle)
        member c.Rent() =
            let r =
                if pool.Count > 0 then pool.Pop()
                else 
                    rentCount <- rentCount + 1
                    shared.Rent()
            r.OriginPool <- owner
            r.OriginStack <- pool
            r.Recycle <- recycleAction
            r
        member c.Return (x : Message<'a>) =
            recycle x
        override c.ToString() =
            let count = pool.Count
            let total = pool |> Seq.sumBy (fun p -> p.Capacity)
            if count = 0 && total = 0 then""
            else
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
        member c.ToString(writer : IStringBlockWriter) =
            let pools = pools |> Seq.filter (fun p -> p <> null)
            let count = pools |> Seq.length
            if writer.BeginList("Local pools", count) then
                for pool in pools do
                    let str = pool.ToString()
                    if str <> "" then writer.Write(str)
                writer.End()
        override c.ToString() =
            StringBlockWriter.Format(c.ToString)

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
        abstract Process : bool -> bool
        abstract Enqueue : ActorProcessor -> unit
        abstract OnException : ActorException -> unit
        abstract ToString : IStringBlockWriter -> unit

    // Thread-safe
    and ActorProcessor(actorId, inbox : IInbox, dispose, dispatcher : IDispatcher) =
        let processSync = obj()
        let queueSync = obj()
        let queue = Queue<struct(int * int * IDeliverer * obj)>()
        let mutable maxQueued = 0
        let mutable total = 0
        let mutable waitDuration = 0L
        let mutable waitCount = 0
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
                    let actorEx = ActorException(ActorId sourceId, ActorId destId, msg, ex)
                    dispatcher.OnException(actorEx)
            remaining
        member c.ActorId = actorId
        member c.WaitDuration = waitDuration
        member c.Enqueue<'a>(sourceId, destId, message : Message<'a>) =
            let entry = struct(sourceId, destId, Deliverer<'a>.Instance, message :> obj)
            Monitor.Enter queueSync
            queue.Enqueue(entry)
            // This check is an optimization to avoid queuing actors repeatedly in dispatchers,
            // but it relies on dispatchers taking full responsibility to ensure all messages
            // are handled. Otherwise, the actor will be orphaned and continue to accumulate 
            // messages without a dispatcher to run it.
            let r = if queue.Count = 1 then ValueSome dispatcher else ValueNone
            maxQueued <- max maxQueued queue.Count
            Monitor.Exit queueSync
            r            
        member private c.Process(outbox, pool, throughput) =
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
                remaining <- run outbox pool
                if remaining > 0 then
                    // count the batch that was just processed
                    total <- total + 1
                    remaining <- remaining - 1
            let count = total - original
            Monitor.Exit processSync
            count
        /// Takes total byref to allow frequent updating in case this takes a long
        /// time to return.
        member c.ProcessAll(outbox, pool, throughput, total : byref<int64>) =
            let mutable count = 0
            let mutable pending = true
            while pending do
                let delta = c.Process(outbox, pool, throughput)
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
            sprintf "Actor %d: %d batches processed (%d waits, %d ticks), %d max queued"
                actorId total waitCount waitDuration maxQueued 
                            
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
        member c.Enqueue(actor : ActorProcessor, recipientId, message : Message<'a>) =
            match actor.Enqueue<'a>(message.SourceId.value, recipientId, message) with
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
            lock sync <| fun () ->
                sprintf "%d actors max queued" maxCount

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
            // Avoiding foreach since recipients will be cleared on last iteration.
            let destinations = message.Destinations.Span
            for i = 0 to destinations.Length - 1 do
                let dest = destinations.[i]
                queue.Enqueue(c.Get(dest.recipientId.value), dest.destinationId.value, message)
        interface IOutboxSender with
            member c.Send<'a>(message) =
                c.Send<'a> message
        override c.ToString() =
            sprintf "%d outbox actors" actors.Count

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
                let destinations = message.Destinations.Span
                let sourceId = message.SourceId.value
                for i = 0 to destinations.Length - 1 do
                    let dest = destinations.[i]
                    // Using shared map here
                    let actor = actors.GetProcessor(dest.recipientId.value)
                    match actor.Enqueue<'a>(sourceId, dest.destinationId.value, message) with
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
        let queue = new DispatchQueue(owner)
        let sender = OutboxSender(queue, actorMap)
        let outbox = LocalOutbox(workerId.ToString(), pool, sender)
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
                count <- count + active.ProcessAll(outbox, pool, throughput, &total)
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
            let t = new Thread(run)
            t.Name <- sprintf "Worker %d" workerId
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
                queuedCount = queue.GetCount()
                processedCount = total
                }
            Monitor.Exit sync
            r
        member c.Enqueue(x : ActorProcessor) =
            queue.Enqueue(x)
        member c.ToString(writer : IStringBlockWriter) =
            let name = sprintf "Worker %d (%d batches processed, %d waits)" workerId total waits
            let id = sprintf "Worker %d" workerId
            if writer.Begin(name, id) then
                writer.Write(sender.ToString())
                writer.Write(queue.ToString())
                pool.ToString(writer)
                writer.End()
        override c.ToString() =
            StringBlockWriter.Format(c.ToString)

    // Thread-safe
    type WorkerDispatcher(actorMap, pool, workerCount, throughput, onException) as c =
        let workers = 
            // round up to pow2 size so we can avoid modulus
            let size = getNextPow2 workerCount
            let arr = Array.zeroCreate size
            // create workers
            for i = 0 to workerCount - 1 do
                arr.[i] <- new Worker(c, actorMap, pool, i, throughput, arr)
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
    type Dispatcher(actorMap, sharedPool, throughput) as c =
        let pool = LocalPool(sharedPool)
        let queue = new DispatchQueue(c)
        let sender = OutboxSender(queue, actorMap)
        let outbox = LocalOutbox("Main", pool, sender)
        let sync = obj()
        let mutable total = 0L
        interface IDispatcher with
            member c.Process(waitThreads) =
                // Process on current thread until queue is empty
                Monitor.Enter sync
                let mutable actor = Unchecked.defaultof<_>
                let mutable count = 0
                while queue.TryDequeue &actor do
                    outbox.ActiveId <- actor.ActorId
                    count <- count + actor.ProcessAll(outbox, pool, throughput, &total)
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
            let name = sprintf "Main (%d processed)" total
            let id = "Main"
            if writer.Begin(name, id) then
                writer.Write(sender.ToString())
                writer.Write(queue.ToString())
                pool.ToString(writer)
                writer.End()
        override c.ToString() =
            StringBlockWriter.Format(c.ToString)

    // Thread-safe
    type CurrentDispatcher(actorMap, sharedPool, throughput) =
        let pool = LocalPool(sharedPool)
        let outbox = SharedOutbox(sharedPool, actorMap)
        let sync = obj()
        let mutable total = 0L
        interface IDispatcher with
            member c.Process(waitThreads) = false
            member c.Enqueue(actor) =
                Monitor.Enter sync
                actor.ProcessAll(outbox, pool, throughput, &total) |> ignore
                Monitor.Exit sync
            member c.OnException(ex) =
                raise ex
            member c.Dispose() = ()
            member c.ToString(writer) =
                c.ToString(writer)
        member c.ToString(writer : IStringBlockWriter) =
            let name = sprintf "Current (%d processed)" total
            let id = "Current"
            if writer.Begin(name, id) then
                pool.ToString(writer)
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
        member c.SetDispatchers(actors, pool, onWorkerError, dispatchers : _[]) =
            for i = 0 to dispatchers.Length - 1 do
                let desc = dispatchers.[i]
                let dispatcher : IDispatcher = 
                    match desc.dispatcherType with
                    | DispatcherType.Foreground -> 
                        new Dispatcher(actors, pool, desc.throughput) :> IDispatcher
                    | DispatcherType.Background | _ -> 
                        if desc.threadCount <= 0 
                        then new Dispatcher(actors, pool, desc.throughput) :> IDispatcher
                        else 
                            new WorkerDispatcher(actors, pool, desc.threadCount, 
                                desc.throughput, onWorkerError) :> IDispatcher                            
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
    let pool = SharedPool()
    let dispatchers = new DispatcherLookup(config.dispatchers.Length)
    let actors = new SharedActorMap(dispatchers)
    let outbox = SharedOutbox(pool, actors)
    let handlers = ExceptionHandlers()
    do dispatchers.SetDispatchers(actors, pool, handlers.Handle, config.dispatchers)
    new() = new ActorSystem(ActorSystemConfiguration.defaults)
    new(workerCount) = new ActorSystem(ActorSystemConfiguration.createDefaults workerCount)
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
    member c.BeginSend<'a>() =
        outbox.BeginSend<'a>()
    interface IMessagePump with
        member c.BeginSend<'a>() = c.BeginSend<'a>()
        member c.Process() = c.Process()
        member c.ProcessAll() = c.ProcessAll()
        member c.Dispose() = c.Dispose()
    member c.ToString(writer : IStringBlockWriter) =
        actors.ToString(writer)
        pool.ToString(writer)
        dispatchers.ToString(writer)
    override c.ToString() =
        sprintf "Actor system\n  %s%s%s"
            (addIndent (actors.ToString()))
            (addIndent (pool.ToString()))
            (addIndent (dispatchers.ToString()))
    static member CreateSingleThread() =
        new ActorSystem(ActorSystemConfiguration.singleThread)

type NullMessagePump() =
    static let mutable instance = new NullMessagePump() :> IMessagePump
    static member Instance = instance
    interface IMessagePump with
        member c.BeginSend<'a>() = NullOutbox.Instance.BeginSend<'a>()
        member c.Process() = ()
        member c.ProcessAll() = ()
        member c.Dispose() = ()

[<Struct>]
type ActorReference = {
    actorId : ActorId
    pump : IMessagePump
    } with
    override c.ToString() = c.actorId.ToString()
    static member Null = {
        actorId = ActorId.undefined
        pump = NullMessagePump.Instance
        }

[<AutoOpen>]
module ActorSystem =
    type IMessagePump with
        member c.Get id = {
            actorId = id
            pump = c
            }

        member c.Process<'a>(destId, msg : 'a) =
            c.Send(destId, msg)
            c.Process()

        member c.Process<'a>(destId, msg : 'a, sourceId) =
            c.Send(destId, msg, sourceId)
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
        member c.Register(disptcherId, register : Container -> IDisposable) =
            let predicate (_ : ActorId) = true
            c.Register(predicate, disptcherId, register)

        member c.Register(register : Container -> IDisposable) =
            c.Register(0, register)

        /// Creates for a specific actor ID
        member c.Register(actorId : ActorId, dispatcherId, register : Container -> IDisposable) =
            c.Register((=)actorId, dispatcherId, register)

        member c.Register(actorId : ActorId, register : Container -> IDisposable) =
            c.Register(actorId, 0, register)

    type ActorReference with
        member c.BeginSend<'a>() =
            c.pump.BeginSend(c.actorId)

        member c.Send(msg) =
            c.pump.Send(c.actorId, msg)

        member c.Send(msg, sourceId) =
            c.pump.Send(c.actorId, msg, sourceId)

        member c.SendAll(msgs) =
            c.pump.SendAll(c.actorId, msgs)

        member c.SendAll<'a>(msgs : ReadOnlySpan<'a>, sourceId) =
            c.pump.SendAll(c.actorId, msgs, sourceId)

        member c.Process msg =
            c.pump.Process(c.actorId, msg)
    