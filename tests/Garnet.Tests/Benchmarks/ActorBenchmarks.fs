﻿module Garnet.Benchmarks.Actors

open System.Collections.Generic
open System.Diagnostics
open System.Runtime.InteropServices
open System.Threading
open Garnet.Composition

module PingPong =
    type RecordedMessage = {
        sourceId : ActorId 
        destId : ActorId 
        sequence : int
        payload : int64
        dispatcher : string
        timestamp : int64
        }

    module RecordedMessage =
        let formatPair (s, r) =
            $"%d{r.payload} from [%d{s.sequence}] a%d{s.sourceId.Value} (d%s{s.dispatcher}) to [%d{r.sequence}] a%d{r.destId.Value} (d%s{r.dispatcher}) in %d{r.timestamp - s.timestamp}"
        
    type SynchronizedQueue<'a>() =
        let incoming = Queue<'a>()
        let sync = obj()
        member c.Enqueue x =
            Monitor.Enter sync
            incoming.Enqueue x
            Monitor.Exit sync
        member c.TryDequeue([<Out>] item : byref<_>) =
            Monitor.Enter sync
            let r = incoming.Count > 0
            if r then item <- incoming.Dequeue()
            Monitor.Exit sync
            r        
        member c.Flush() = seq {
            let mutable item = Unchecked.defaultof<'a>
            while c.TryDequeue(&item) do
                yield item
            }

    module Tests =
        let runLogging log onSend onReceive poolCount actorsPerPool workerCount duration initCount maxCount batchSize =
            let actorCount = actorsPerPool * poolCount
            let receivedCount = ref 0
            let sentCount = ref 0
            let config = {
                Dispatchers = [|
                    for i = 1 to poolCount do
                        yield {
                            // workers
                            Name = $"Worker{i}"
                            ThreadCount = workerCount
                            Throughput = 100
                            DispatcherType = DispatcherType.Background
                        }
                    yield {
                        // main
                        Name = "Main"
                        DispatcherType = DispatcherType.Foreground
                        ThreadCount = 0
                        Throughput = 100
                    }
                    |]
                }
            use a = new ActorSystem(config)
            a.Register(fun (actorId : ActorId) ->
                let writer = new MessageWriter<_>()
                let mailbox = Mailbox()
                mailbox.OnAll<int64> <| fun e ->
                    let span = e.Span
                    let c = Interlocked.Increment receivedCount
                    if log then
                        onReceive { 
                            sourceId = ActorId.Undefined
                            destId = actorId
                            sequence = c
                            payload = span.[0]
                            timestamp = Stopwatch.GetTimestamp()
                            dispatcher = mailbox.ToString()
                            }
                    if c <= maxCount then
                        let _ = Interlocked.Increment sentCount
                        let rand = uint64 c * 2862933555777941757UL + 3037000493UL
                        let destId = (abs (int rand) % actorCount) + 1 |> ActorId
                        if duration > 0 then
                            Thread.Sleep duration
                        if log  then
                            let nextItem = span.[0] + 1L
                            onSend { 
                                sourceId = actorId
                                destId = destId
                                sequence = c
                                payload = nextItem
                                timestamp = Stopwatch.GetTimestamp()
                                dispatcher = mailbox.ToString()
                                }
                        for i = 0 to span.Length - 1 do
                            writer.WriteValue(span.[i] + 1L)
                        writer.DestinationId <- destId
                        writer.Outbox <- mailbox
                        writer.Send()
                let dispatcherId = (actorId.Value - 1) / actorsPerPool
                Actor(mailbox, dispatcherId))
            let writer = new MessageWriter<_>()
            for i = 0 to initCount - 1 do
                let destId = (i % actorCount) + 1 |> ActorId
                let payload = (i + 1) * 10000000
                for i = 0 to batchSize - 1 do
                    writer.WriteValue (payload + i * 10000 |> int64)
                if log then
                    onSend { 
                        sourceId = ActorId.Undefined
                        destId = destId
                        sequence = 0
                        payload = payload |> int64
                        timestamp = Stopwatch.GetTimestamp()
                        dispatcher = ""
                        }
                writer.Outbox <- a
                writer.DestinationId <- destId
                writer.Send()
            a.ProcessAll()
            let expected = maxCount + initCount
            let actual = receivedCount.Value
            if actual <> expected then
                printfn $"Expected received count: %d{expected}, actual: %d{actual}"
                printfn "%s" <| a.ToString()
            let expected = maxCount
            let actual = sentCount.Value
            if actual <> expected then
                printfn $"Expected sent count: %d{expected}, actual: %d{actual}"
            if log then
                printfn $"%s{a.ToString()}"

        let run = runLogging false ignore ignore

        let runHistory log poolCount actorsPerPool workerCount duration initCount maxCount batchSize =
            let sent = SynchronizedQueue<_>()
            let received = SynchronizedQueue<_>()
            runLogging log sent.Enqueue received.Enqueue poolCount actorsPerPool workerCount duration initCount maxCount batchSize
            let sent = sent.Flush() |> Seq.toArray
            let received = received.Flush() |> Seq.toArray
            let sentSet = sent |> Seq.map (fun x -> (x.destId, x.payload), x) |> Map.ofSeq
            received |> Seq.map (fun x -> sentSet.[x.destId, x.payload], x) |> Seq.toArray

        let runMain log useMain (workerCount : int) initCount maxCount =
            let maxActorCount = maxCount * 2 - initCount
            let count = ref 0
            let createInbox _ =
                let writer = new MessageWriter<_>()
                let mailbox = Mailbox()
                mailbox.OnAll<int64> <| fun e ->
                    if Interlocked.Increment count <= maxActorCount then
                        let span = e.Span
                        for i = 0 to span.Length - 1 do
                            writer.WriteValue span.[i]
                        writer.Outbox <- mailbox
                        writer.DestinationId <- mailbox.SourceId 
                        writer.Send()
                mailbox
            use a = new ActorSystem(workerCount)
            a.Register(ActorId 1, fun _ -> Actor(createInbox()))
            a.Register(ActorId 2, fun _ -> Actor(createInbox(), if useMain then 1 else 0))
            for i = 1 to initCount do
                a.Send(ActorId 1, ActorId 2, int64 i)
            a.ProcessAll()
            if log then
                printfn $"%s{a.ToString()}\n%d{count.Value}"

type Run = struct end
type Ping = struct end
type Pong = struct end

//[<SimpleJob(RuntimeMoniker.CoreRt50)>]
//type SimplePingPongBenchmark() =
//    let a = new ActorSystem(0)
//    let mutable count = 0
//    [<Params(1000)>]
//    member val N = 1 with get, set
//    [<GlobalSetup>]
//    member this.Setup() =
//        a.Register(ActorId 1, fun _ ->
//            let h = Mailbox()
//            h.On<Run> <| fun e ->
//                h.Send(ActorId 2, Ping())
//            h.On<Pong> <| fun e ->
//                count <- count + 1
//                if count < this.N then
//                    h.Respond(Ping())
//            Actor(h))
//        a.Register(ActorId 2, fun _ -> 
//            let h = Mailbox()
//            h.On<Ping> <| fun e -> 
//                h.Respond(Pong())
//            Actor(h)
//            )
//    [<Benchmark>]
//    member this.SingleThread() = 
//        count <- 0
//        a.Process(ActorId 1, Run())
//        a.ProcessAll()
//        count
