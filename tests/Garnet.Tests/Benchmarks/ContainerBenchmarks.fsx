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
runIterateComponents2 100000 1000 // 00:00:00.970
runIterateComponents3 100000 1000
runIterateComponents4 100000 1000

runQueryComponents1 100000 1000 // 00:00:00.265
runQueryComponents2 100000 1000 // 00:00:00.368
runQueryComponents3 100000 1000 // 00:00:00.518
runQueryComponents4 100000 1000 // 00:00:00.728

runQueryComponentsTuple2 100000 1000 // 00:00:01.122
runQueryComponentsTuple3 100000 1000
runQueryComponentsTuple4 100000 1000

runQuerySegments1 100000 1000 // 00:00:00.200
runQuerySegments2 100000 1000 // 00:00:00.204
runQuerySegments3 100000 1000 // 00:00:00.328
runQuerySegments4 100000 1000 // 00:00:00.578

