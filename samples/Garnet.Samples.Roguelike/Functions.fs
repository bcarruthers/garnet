namespace Garnet.Samples.Roguelike

open System
open System.Collections.Generic

module Vector =
    let init x y = { X = x; Y = y }
    let zero = init 0 0
    let one = init 1 1

    let min a b = { X = min a.X b.X; Y = min a.Y b.Y }
    let max a b = { X = max a.X b.X; Y = max a.Y b.Y }

    let add a b = { 
        X = a.X + b.X
        Y = a.Y + b.Y
        }

    let subtract a b = { 
        X = a.X - b.X
        Y = a.Y - b.Y
        }

module Bounds =
    let init min max = { Min = min; Max = max }
    let sized min size = init min (Vector.add min size)
    let zero = init Vector.zero Vector.zero
    let zeroToOne = init Vector.zero Vector.one

    let maxToMin = {
        Min = { X = Int32.MaxValue; Y = Int32.MaxValue }
        Max = { X = Int32.MinValue; Y = Int32.MinValue }
        }

    let including bounds p = { 
        Min = Vector.min bounds.Min p
        Max = Vector.max bounds.Max p
        }

    let union a b = { 
        Min = Vector.min a.Min b.Min
        Max = Vector.max a.Max b.Max
        }

    let getSize b =
        Vector.subtract b.Max b.Min

    let getCenter b =
        let v = Vector.add b.Max b.Min
        { X = v.X / 2; Y = v.Y / 2 }

    let getCentered contentSize b = 
        let size = getSize b
        {
            X = b.Min.X + (size.X - contentSize.X) / 2
            Y = b.Min.Y + (size.Y - contentSize.Y) / 2
        }
        
    let expand margin b = {
        Min = Vector.subtract b.Min margin.Min
        Max = Vector.add b.Max margin.Max
        }

    let includingAll locs =
        locs 
        |> Seq.fold including maxToMin
        |> expand zeroToOne

module Direction =
    let all = [|
        East
        North
        West
        South
        |]

    let getNext loc dir =
        match dir with
        | East -> { loc with X = loc.X + 1 } 
        | West -> { loc with X = loc.X - 1 }
        | North -> { loc with Y = loc.Y - 1 }
        | South -> { loc with Y = loc.Y + 1 }
    
module DistanceMap =
    let empty = {
        Distances = Map.empty
        }

    let create isPassable (tiles : Map<Vector, _>) seeds =
        let result = Dictionary<Vector, int>()
        let queue = Queue<struct(Vector * int)>()
        let enqueue p dist =
            if not (result.ContainsKey(p)) then
                let canVisit =
                    match tiles.TryGetValue(p) with
                    | false, _ -> false
                    | true, tile -> isPassable tile
                result.Add(p, if canVisit then dist else Int32.MaxValue)
                if canVisit then queue.Enqueue(struct(p, dist))                            
        for seed in seeds do
            enqueue seed 0
        while queue.Count > 0 do
            let struct(p, dist) = queue.Dequeue()
            let nextDist = dist + 1
            for dir in Direction.all do
                let next = Direction.getNext p dir
                enqueue next nextDist
        {
            Distances = 
                result
                |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
                |> Map.ofSeq
        }

    let getDistance p map =
        match map.Distances.TryGetValue(p) with
        | true, dist -> dist
        | false, _ -> Int32.MaxValue

    let distanceToChar x =
        if x = 0 then '.'
        elif x < 10 then '0' + char x
        elif x < 36 then 'a' + char (x - 10)
        elif x < 62 then 'A' + char (x - 36)
        elif x = Int32.MaxValue then '#'
        else '+'
    
    let format map =
        let b = map.Distances |> Seq.map (fun kvp -> kvp.Key) |> Bounds.includingAll
        let size = Bounds.getSize b
        let dw = size.X + 1
        let data = Array.create (dw * size.Y) ' '
        for y = 0 to size.Y - 1 do
            data.[y * dw + dw - 1] <- '\n'
        for kvp in map.Distances do
            let p = Vector.subtract kvp.Key b.Min
            data.[p.Y * dw + p.X] <- distanceToChar kvp.Value
        String(data)

module Tile =
    let getChar tile =
        match tile.Entity with
        | Some e ->
            match e.EntityType with
            | Rogue -> '@'
            | Minion -> 'm'
        | None ->
            match tile.Terrain with
            | Floor -> '.'
            | Wall -> '#' 

    let getMoveEvents loc nextLoc dir tile = seq {
        match tile.Entity with
        | Some entity -> 
            if entity.Hits = 1 then yield Destroyed nextLoc
            else
                yield Attacked {
                    AttackerLoc = loc
                    AttackDir = dir
                    Damage = 1     
                    }
        | None -> yield Moved {
            SourceLoc = loc
            MoveDir = dir
            }
        }
    
    let addEntity entity tile =
        { tile with Entity = Some entity }

    let removeEntity tile =
        { tile with Entity = None }

    let isPassable tile =
        match tile.Terrain with
        | Wall -> false
        | Floor -> true
        
module Entity =
    let rogue = {
        EntityType = Rogue
        Hits = 3
    }

    let minion = {
        EntityType = Minion
        Hits = 1
    }

    let applyDamage damage entity =
        { entity with Hits = entity.Hits - damage }

module Animation =
    let format =
        function
        | Moving e -> $"Moved {e.MoveDir}"
        | Attacking e -> $"{e.AttackerEntityType} attacked {e.TargetEntityType}"
        | Destroying e -> $"{e.DestroyedEntityType} destroyed"

module World =
    let empty = {
        Turn = 0
        RandomSeed = 0UL
        Tiles = Map.empty
        Animations = List.empty
        }

    let generate mapRadius seed =
        let r = mapRadius + 1
        let extent = r * 2 + 1 
        let count = extent * extent
        let rand = Random(seed)
        // draw random walls with border
        let cells1 = Array.zeroCreate count
        for y = -r to r do
            for x = -r to r do
                let i = (y + r) * extent + (x + r)
                let dist = max (abs x) (abs y)
                let cell = dist = r || (dist > 2 && rand.Next(10) = 0)
                cells1.[i] <- cell
        // apply morphological dilate
        let cells2 = Array.zeroCreate count
        let rm = r - 1
        for y = -rm to rm do
            for x = -rm to rm do
                let i = (y + r) * extent + (x + r)
                let cell =
                    if cells1.[i] then true
                    else
                        let ix0 = i - 1
                        let ix1 = i + 1
                        let iy0 = i - extent
                        let iy1 = i + extent
                        cells1.[ix0] || cells1.[ix1] || cells1.[iy0] || cells1.[iy1]
                cells2.[i] <- cell
        // populate tiles
        let tiles = seq {
            for y = -rm to rm do
                for x = -rm to rm do
                    let i = (y + r) * extent + (x + r)
                    let terrain = if cells2.[i] then Wall else Floor
                    let p = Vector.init x y
                    yield p, {
                        Terrain = terrain
                        Entity =
                            match terrain with
                            | Wall -> None
                            | Floor ->
                                if p = Vector.zero then Some Entity.rogue
                                elif rand.Next(8) = 0 then Some Entity.minion
                                else None
                    }
            }
        { empty with 
            RandomSeed = uint64 seed
            Tiles = Map.ofSeq tiles
        }

    let getMinLocation world =
        world.Tiles |> Seq.map (fun kvp -> kvp.Key) |> Seq.reduce Vector.min

    let formatTiles world =
        let b = world.Tiles |> Seq.map (fun kvp -> kvp.Key) |> Bounds.includingAll
        let size = Bounds.getSize b
        let dw = size.X + 1
        let data = Array.create (dw * size.Y) ' '
        for y = 0 to size.Y - 1 do
            data.[y * dw + dw - 1] <- '\n'
        for kvp in world.Tiles do
            let p = Vector.subtract kvp.Key b.Min
            data.[p.Y * dw + p.X] <- Tile.getChar kvp.Value
        String(data)

    let formatAnimations world =
        world.Animations
        |> List.rev
        |> Seq.map Animation.format
        |> String.concat "\n"

    let format world =
        $"Turn {world.Turn}:\n{formatAnimations world}\n{formatTiles world}"

    let getEntityLocations entityType world = seq {
        for kvp in world.Tiles do
            match kvp.Value.Entity with
            | Some entity -> if entity.EntityType = entityType then yield kvp.Key
            | None -> ()
        }
        
    let isOccupied loc world =
        match Map.tryFind loc world.Tiles with
        | Some tile -> tile.Terrain = Wall || tile.Entity.IsSome
        | None -> true
        
    let tryGetEntity loc world =
        Map.tryFind loc world.Tiles
        |> Option.bind (fun tile -> tile.Entity)

    let mapTile map loc world =
        match Map.tryFind loc world.Tiles with
        | Some tile -> { world with Tiles = Map.add loc (map tile) world.Tiles }
        | None -> world

    let mapEntity map loc world =
        mapTile (fun tile -> { tile with Entity = Option.map map tile.Entity }) loc world

    let addEntity loc entity world =
        mapTile (Tile.addEntity entity) loc world

    let removeEntity loc world =
        mapTile Tile.removeEntity loc world

    let moveEntity loc newLoc world =
        match tryGetEntity loc world with
        | Some entity ->
            world 
            |> removeEntity loc
            |> addEntity newLoc entity
        | None -> world

    let appendAnimation anim world = {
        world with Animations = anim :: world.Animations
        }

    let find entityType world =
        world.Tiles
        |> Map.tryPick (fun loc tile ->
            tile.Entity 
            |> Option.bind (fun e -> 
                if e.EntityType = entityType then Some (loc, e) else None))

    let getDistanceMap map targets =
        DistanceMap.create Tile.isPassable map.Tiles targets

    let stepTurn world =
        { world with Turn = world.Turn + 1 }

module Action =
    let getEvents action loc world =
        match action with
        | Move dir ->
            let nextLoc = Direction.getNext loc dir
            match Map.tryFind nextLoc world.Tiles with
            | Some tile -> Tile.getMoveEvents loc nextLoc dir tile
            | None -> Seq.empty

    let getPlayerEvents action world =
        match World.find Rogue world with
        | Some (loc, _) -> getEvents action loc world
        | None -> Seq.empty

module Event =
    let applyEvent world event =
        match event with
        | Attacked e -> 
            match World.tryGetEntity e.AttackerLoc world with
            | None -> world
            | Some attacker ->
                let targetLoc = Direction.getNext e.AttackerLoc e.AttackDir
                match World.tryGetEntity targetLoc world with
                | None -> world
                | Some target ->
                    world
                    |> World.mapEntity (Entity.applyDamage e.Damage) targetLoc
                    |> World.appendAnimation (Attacking {
                        AttackerLoc = e.AttackerLoc
                        AttackerEntityType = attacker.EntityType
                        AttackDir = e.AttackDir
                        Damage = e.Damage
                        TargetEntityType = target.EntityType
                        })
        | Moved e -> 
            let targetLoc = Direction.getNext e.SourceLoc e.MoveDir
            world
            |> World.moveEntity e.SourceLoc targetLoc
            |> World.appendAnimation (Moving {
                SourceLoc = e.SourceLoc
                MoveDir = e.MoveDir
                })
        | Destroyed p -> 
            match World.tryGetEntity p world with
            | None -> world
            | Some target ->
                world
                |> World.removeEntity p
                |> World.appendAnimation (Destroying {
                    DestroyedLoc = p
                    DestroyedEntityType = target.EntityType
                    })

module Loop =
    let tryGetAction key =
        match key with
        | ConsoleKey.RightArrow -> Move East |> Some
        | ConsoleKey.LeftArrow -> Move West |> Some
        | ConsoleKey.UpArrow -> Move North |> Some
        | ConsoleKey.DownArrow -> Move South |> Some
        | _ -> None

    let readPlayerActions() = seq {
        let mutable isRunning = true
        while isRunning do
            let key = Console.ReadKey().Key
            match tryGetAction key with
            | Some action -> yield action
            | None -> isRunning <- key <> ConsoleKey.Escape
        }

    let printWorld world =
        world |> World.format |> printfn "%s"

    let applyPlayerEvents action world =
        Action.getPlayerEvents action world
        |> Seq.fold Event.applyEvent world
        
    let getHostileMoveEvents p dm world =
        let dirs =
            Direction.all
            |> Seq.filter (fun dir ->
                let next = Direction.getNext p dir
                not (World.isOccupied next world))
            |> Seq.toArray
        if dirs.Length = 0 then world
        else
            let nearestDir =
                dirs
                |> Seq.minBy (fun dir ->
                    let next = Direction.getNext p dir
                    DistanceMap.getDistance next dm)
            Moved {
                SourceLoc = p
                MoveDir = nearestDir
                }
            |> Event.applyEvent world
        
    let applyHostileEvents world =
        let targetLocs = World.getEntityLocations Rogue world
        let dm = World.getDistanceMap world targetLocs
        World.getEntityLocations Minion world
        |> Seq.sortBy (fun p -> DistanceMap.getDistance p dm)
        |> Seq.fold (fun state p -> getHostileMoveEvents p dm state) world           
            
    let stepWorld world action =
        { world with Animations = List.empty }
        |> applyPlayerEvents action
        |> applyHostileEvents
        |> World.stepTurn

    let run world =
        readPlayerActions()
        |> Seq.scan stepWorld world
        |> Seq.iter printWorld
