#r "../bin/Release/net6.0/Garnet.dll"
#load "ContainerBenchmarks.fs"

open Garnet.Benchmarks.Containers

#time

runCreateDestroyEntities 10000 1000
runCreateDestroyMultipleComponents 10000 100
runAddRemoveComponent 10000 1000
runAddRemoveComponentDirect 10000 1000
runAddRemoveMultipleComponents 10000 100

// These are different ways of iterating over entities

// Original function-based approach:
runIterateComponents1 100000 1000 // 00:00:00.628
runIterateComponents2 100000 1000 // 00:00:00.970
runIterateComponents3 100000 1000 // 00:00:01.215
runIterateComponents4 100000 1000 // 00:00:01.201

// New enumerable approach:
runQueryComponents1 100000 1000 // 00:00:00.265
runQueryComponents2 100000 1000 // 00:00:00.368
runQueryComponents3 100000 1000 // 00:00:00.518
runQueryComponents4 100000 1000 // 00:00:00.728

// New approach reading values to tuple:
runQueryComponentsTuple2 100000 1000 // 00:00:01.122
runQueryComponentsTuple3 100000 1000
runQueryComponentsTuple4 100000 1000

// New approach using batches:
runQuerySegments1 100000 1000 // 00:00:00.176
runQuerySegments2 100000 1000 // 00:00:00.254
runQuerySegments3 100000 1000 // 00:00:00.328
runQuerySegments4 100000 1000 // 00:00:00.578

