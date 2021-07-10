﻿#r "netstandard"
#r "../bin/Release/net5.0/Garnet.dll"
#load "ChannelBenchmarks.fs"

open Garnet.Benchmarks.Channels

#time

run 30_000_000 1
run 30_000_000 10
runWithoutCachingChannel 30_000_000 1
runBatches 10_000_000 1
runBatches 1_000_000 100
