module Garnet.Benchmarks.Program

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Garnet.Ecs
open Garnet.Benchmarks

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

[<EntryPoint>]
let main argv =
    //BenchmarkRunner.Run<SegmentStorageBench>() |> ignore
    BenchmarkRunner.Run<Actors.SimplePingPongBenchmark>() |> ignore
    0
