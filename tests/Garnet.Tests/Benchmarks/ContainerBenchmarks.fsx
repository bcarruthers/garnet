#r "../bin/Release/net5.0/Garnet.dll"
#load "ContainerBenchmarks.fs"

open Garnet.Benchmarks.Containers

#time

runCreateDestroyEntities 10000 1000
runCreateDestroyMultipleComponents 10000 100
runAddRemoveComponent 10000 1000
runAddRemoveComponentDirect 10000 1000
runAddRemoveMultipleComponents 10000 100
runIterateEntities 100000 1000
runIterateMultipleComponents 10000 100
