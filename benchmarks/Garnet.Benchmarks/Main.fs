module Garnet.Benchmarks.Program

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Garnet.Ecs
open Garnet.Actors

[<CoreJob>]
type SegmentStorageBenchmark() =
    let c = Container()
    [<Params(10000)>]
    member val N = 1 with get, set
    [<GlobalSetup>]
    member this.Setup() =
        ()
    [<Benchmark>]
    member this.AddRemove() = 
        for i = 1 to this.N do
            c.Create()
                .With(1)
                .With("a")
                .Add('c')
        c.Commit()
        c.DestroyAll()
        c.Commit()            

[<CoreJob>]
type IterationBenchmark() =
    let c = Container()
    [<Params(100000)>]
    member val N = 1 with get, set
    [<GlobalSetup>]
    member this.Setup() =
        for i = 1 to this.N do
            c.Create()
                .With(1)
                .With("a")
                .Add('c')
        c.Commit()
    [<Benchmark>]
    member this.Iterate() = 
        let mutable count = 0
        let iter =
            fun param struct(a : int, b : string, c : char) ->
                count <- count + param + a
            |> Join.iter3
            |> Join.over c
        for i = 1 to this.N do
            iter i
        count

type Run = struct end
type Ping = struct end
type Pong = struct end

[<CoreJob>]
type PingPongBenchmark() =
    let a = new ActorSystem(0)
    let mutable count = 0
    [<Params(1000)>]
    member val N = 1 with get, set
    [<GlobalSetup>]
    member this.Setup() =
        a.Register(ActorId 1, fun h ->
            h.OnInbound<Run> <| fun e ->
                e.outbox.Send(ActorId 2, Ping())
            h.OnInbound<Pong> <| fun e ->
                count <- count + 1
                if count < this.N then
                    e.Respond(Ping())
            )
        a.Register(ActorId 2, fun h -> 
            h.OnInbound<Ping> <| fun e -> 
                e.Respond(Pong())
            )
    [<Benchmark>]
    member this.SingleThread() = 
        count <- 0
        a.Run(ActorId 1, Run())
        a.RunAll()
        count

[<EntryPoint>]
let main argv =
    //BenchmarkRunner.Run<SegmentStorageBench>() |> ignore
    BenchmarkRunner.Run<PingPongBenchmark>() |> ignore
    0
