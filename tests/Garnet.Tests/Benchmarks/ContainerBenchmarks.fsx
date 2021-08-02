#r "../bin/Release/net5.0/Garnet.dll"
#load "ContainerBenchmarks.fs"

open Garnet.Benchmarks.Containers

#time

runCreateDestroyEntities 10000 1000
runCreateDestroyMultipleComponents 10000 100
runAddRemoveComponent 10000 1000
runAddRemoveComponentDirect 10000 1000
runAddRemoveMultipleComponents 10000 100
runIterateComponents1 100000 1000
runIterateComponents2 100000 1000
runIterateComponents4 100000 1000

runQueryComponents1 100000 1000
runQueryComponents2 100000 1000
runQueryComponents3 100000 1000
