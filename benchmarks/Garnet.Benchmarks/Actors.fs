module Garnet.Benchmarks.Actors

open BenchmarkDotNet.Attributes
open Garnet.Actors

type Run = struct end
type Ping = struct end
type Pong = struct end

[<CoreJob>]
type SimplePingPongBenchmark() =
    let a = new ActorSystem(0)
    let mutable count = 0
    [<Params(1000)>]
    member val N = 1 with get, set
    [<GlobalSetup>]
    member this.Setup() =
        a.Register(ActorId 1, fun h ->
            h.On<Run> <| fun e ->
                h.Send(ActorId 2, Ping())
            h.On<Pong> <| fun e ->
                count <- count + 1
                if count < this.N then
                    h.Respond(Ping())
            )
        a.Register(ActorId 2, fun h -> 
            h.On<Ping> <| fun e -> 
                h.Respond(Pong())
            )
    [<Benchmark>]
    member this.SingleThread() = 
        count <- 0
        a.Run(ActorId 1, Run())
        a.RunAll()
        count
