#r "netstandard"
#r "bin/Release/netcoreapp2.1/Garnet.dll"
#load "Containers.fs"

open Garnet.Benchmarks.Containers

#time

runCreateDestroyEntities 10000 1000
runCreateDestroyMultipleComponents 10000 100
runCreateDestroyMultipleComponents 1000 1000
runAddRemoveComponent 10000 100
runAddRemoveMultipleComponents 10000 100
