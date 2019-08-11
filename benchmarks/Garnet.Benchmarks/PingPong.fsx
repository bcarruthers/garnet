#r "netstandard"
#r "bin/Release/netcoreapp2.1/Garnet.dll"
#load "PingPong.fs"

open Garnet.Benchmarks.PingPong
open Garnet.Benchmarks.PingPong.Tests

#time

runHistory true 2 2 0 10 20 1
|> Seq.map RecordedMessage.formatPair
|> Seq.iter (printfn "%s")

runHistory false 200 16 0 1000 200000 1 |> ignore

runMain true true 1 10 6000
runMain true true 0 10 4000000
runMain true false 2 10 4000000

run 1000 1 0 5000 4000000 1
run 1000 2 0 5000 4000000 1
run 1000 8 0 5000 1000000 100
run 1000 4 0 5000 10000000 1
run 10000 8 0 50000 10000000 1
run 1000 8 1 5000 400 1

run 5 1 0 10 20 1
run 2000 4 0 500 2000 1
run 1000 1 0 5000 5000000 20
run 1000 4 0 5000 4000000 1
run 1000 8 0 500 1000000 1000
