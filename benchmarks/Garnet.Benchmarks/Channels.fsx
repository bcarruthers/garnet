#r "netstandard"
#r "bin/Release/netcoreapp2.1/Garnet.dll"
#load "Channels.fs"

open Garnet.Benchmarks.Channels

#time

run 30_000_000 1
run 30_000_000 10
runBatches 10_000_000 1
runBatches 1_000_000 100
