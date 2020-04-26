#r "netstandard"
#r "bin/Release/netcoreapp2.1/Garnet.dll"
#r "../../packages/BenchmarkDotNet/lib/netstandard2.0/BenchmarkDotNet.dll"
#r "../../packages/BenchmarkDotNet.Annotations/lib/netstandard2.0/BenchmarkDotNet.Annotations.dll"
#load "ActorBenchmarks.fs"

open Garnet.Benchmarks.Actors.PingPong
open Garnet.Benchmarks.Actors.PingPong.Tests

#time

runHistory true 2 2 0 10 20 1
|> Seq.map RecordedMessage.formatPair
|> Seq.iter (printfn "%s")

runHistory false 200 16 0 1000 200000 1 |> ignore

runMain true true 1 10 6000
runMain true true 0 10 4000000
runMain true false 2 10 4000000

// 0ms tasks
run 1000 1 0 5000 4000000 1
run 1000 2 0 5000 4000000 1
run 1000 4 0 5000 4000000 1
run 1000 8 0 5000 4000000 1

// 1ms tasks
run 1000 1 1 50 200 1
run 1000 2 1 50 200 1
run 1000 4 1 50 200 1
run 1000 8 1 50 200 1

run 1000 8 0 5000 1000000 100
run 1000 1 0 5000 10000000 1
run 1000 4 0 5000 10000000 1
run 10000 8 0 50000 10000000 1
run 1000 8 1 5000 400 1

run 5 1 0 10 20 1
run 2000 4 0 500 2000 1
run 1000 1 0 5000 5000000 20
run 1000 4 0 5000 4000000 1

// batch size 1000
run 1000 8 0 500 1000000 1000
run 1000 12 0 500 1000000 1000

// More threads than cores with large number of messages:
// This reproduces issue where not all messages would be
// completed. Using throughput=1 can also help to repro.
runLogging true ignore ignore 5000 32 0 500 10000000 1
runLogging true ignore ignore 5000 16 0 500 10000000 1
runLogging true ignore ignore 5000 8 0 500 10000000 1
runLogging true ignore ignore 5000 6 0 500 10000000 1
runLogging true ignore ignore 5000 4 0 500 10000000 1
runLogging true ignore ignore 5000 2 0 500 10000000 1
runLogging true ignore ignore 5000 1 0 500 10000000 1
