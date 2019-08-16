module Garnet.Samples.Strategy

// This sample demonstrates using component storage for grid cells
// for use in a strategy game.

open System
open System.Text
open Garnet.Composition

// primitives
[<Struct>]
type Loc = { x : int; y : int }
    with 
        override c.ToString() = 
            sprintf "(%d, %d)" c.x c.y

[<Struct>]
type Size = { w : int; h : int }

module Loc =
    // Components are stored in size 64 segments, so
    // we need to define a mapping from component keys
    // to tuple of (segment key, index within segment)
    let toKeyPair p = 
        struct(
            { x = p.x >>> 3; y = p.y >>> 3 }, 
            ((p.y &&& 7) <<< 3) ||| (p.x &&& 7))

// events
type ResetMap = {
    worldSeed : int
    size : Size
    }

type PrintMap = struct end
type StepSim = struct end

module ResetMap =
    let defaultParams = {
        worldSeed = 1
        size = { w = 32; h = 32 }
        }

// map cell components
[<Struct>]
type Terrain =
    | Grassland
    | Desert
    | Mountains

[<Struct>]
type Ore = 
    | Iron
    | Gold

[<Struct>]
type Climate = {
    temperature : int
    humidity : int
    }

[<Struct>]
type City = {
    population : int
    }
    
[<Struct>]
type Occupant = {
    unitEid : Eid
    }

module Occupant =
    let none = { unitEid = Eid.undefined }

// entity components    
[<Struct>]
type UnitType = 
    | Swordsmen
    | Archers
    
[<Struct>]
type UnitSize = {
    unitSize : int
    }

// storage            
type WorldGrid() = 
    let store = ComponentStore<Loc, Loc>(Loc.toKeyPair)
    let mutable size = { w = 0; h = 0 }
    member c.Size = size
    member c.Reset newSize =
        size <- newSize
        store.Clear()
    member c.Get p = { 
        id = p
        container = store 
        recycle = ignore
        }
    member c.Commit() =
        store.Commit()
    interface ISegmentStore<Loc> with
        member c.GetSegments() = store.GetSegments()
    override c.ToString() =
        store.ToString()

// systems
module MapSystem =
    let register (c : Container) =
        let map = c.GetInstance<WorldGrid>()
        Disposable.list [
            c.On<Commit> <| fun e ->
                map.Commit()
            c.On<ResetMap> <| fun e ->
                let rand = Random(e.worldSeed)
                // create map cells
                map.Reset e.size
                for y = 0 to e.size.h - 1 do
                    for x = 0 to e.size.w - 1 do
                        let cell = map.Get { x = x; y = y}
                        cell.Add {
                            temperature = rand.Next(0, 100)
                            humidity = rand.Next(0, 100)
                            }
                        cell.Add Grassland
                        if rand.Next(10) = 0 then
                            cell.Add Iron
                        if rand.Next(20) = 0 then
                            cell.Add<City> {
                                population = rand.Next(1, 10)
                                }
                // add units/entities into map
                c.DestroyAll()
                for i = 1 to 50 do
                    // Pick a random location in map.
                    // Note we aren't checking for collisions. Changes are 
                    // not immediately applied, so we can't rely on map
                    // itself and would need another way to track.
                    let loc = {
                        x = rand.Next(e.size.w) 
                        y = rand.Next(e.size.w)
                        }
                    let entity =
                        c.Create()
                            .With(loc)
                            .With(Swordsmen)
                            .With({ unitSize = rand.Next(1, 10)})                
                    // add (cached) reference to entity in map the entity 
                    // loc is the source of truth, so we'll need to keep 
                    // map synchronized with it.
                    map.Get(loc).Add { unitEid = entity.id }
            c.On<StepSim> (
                // Each sim step, increase the temperature of all
                // cells with iron. Cells can be iterated over the
                // same way as non-grid entities.
                fun param struct(cl : Climate, _ : Ore) ->
                    { cl with temperature = cl.temperature + 1 }
                |> Join.update2
                |> Join.over map)
            c.On<PrintMap> <| fun e ->
                let sb = StringBuilder()
                for y = 0 to map.Size.h - 1 do
                    for x = 0 to map.Size.w - 1 do
                        let cell = map.Get { x = x; y = y}
                        let occupant = cell.GetOrDefault<Occupant>(Occupant.none)
                        let ch =
                            if occupant.unitEid.IsDefined then '!'
                            elif cell.Contains<Ore>() then '$'
                            else 
                                match cell.Get<Terrain>() with
                                | Grassland -> '.'
                                | Desert -> 'd'
                                | Mountains -> 'm'
                        sb.Append ch |> ignore
                    sb.AppendLine() |> ignore
                printfn "%s" (sb.ToString())
            ]

// startup
let run() =
    let c = Container()
    let systems = [
        MapSystem.register c
        ]
    c.Run {
        worldSeed = 1
        size = { w = 20; h = 20 }
        }
    c.Run <| StepSim()
    c.Run <| PrintMap()
    // print all components
    printfn "%s" <| c.ToString()
    // print a single unit
    printfn "%s" <| c.Get(Eid 64).ToString()
    // print a single grid cell
    printfn "%s" <| c.GetInstance<WorldGrid>().Get({ x = 10; y = 15 }).ToString()

