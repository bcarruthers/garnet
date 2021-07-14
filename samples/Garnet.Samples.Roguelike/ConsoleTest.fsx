#load "Types.fs"
#load "Functions.fs"

open Garnet.Samples.Roguelike.Types
open Garnet.Samples.Roguelike

let test() =
    let world = World.generate 10 1 
    World.format world |> printfn "%s"
    World.getDistanceMap world [ Vector.zero ] |> DistanceMap.format |> printfn "%s"
    
    [ Move West; Move West ] 
    |> Seq.fold Loop.stepWorld world
    |> World.format 
    |> printfn "%s"

let testInteractive() =
    World.generate 10 1 |> Loop.run
