namespace Garnet.Samples.Roguelike

open System
open Garnet.Samples.Roguelike.Types

module Vector =
    let init x y = { x = x; y = y }
    let zero = init 0 0
    let one = init 1 1

    let min a b = { x = min a.x b.x; y = min a.y b.y }
    let max a b = { x = max a.x b.x; y = max a.y b.y }

    let add a b = { 
        x = a.x + b.x
        y = a.y + b.y
        }

    let subtract a b = { 
        x = a.x - b.x
        y = a.y - b.y
        }

    let getNextLocation loc dir =
        match dir with
        | East -> { loc with x = loc.x + 1 } 
        | West -> { loc with x = loc.x - 1 }
        | North -> { loc with y = loc.y - 1 }
        | South -> { loc with y = loc.y + 1 }

module Bounds =
    let init min max = { min = min; max = max }
    let sized min size = init min (Vector.add min size)
    let zero = init Vector.zero Vector.zero
    let zeroToOne = init Vector.zero Vector.one

    let maxToMin = {
        min = { x = Int32.MaxValue; y = Int32.MaxValue }
        max = { x = Int32.MinValue; y = Int32.MinValue }
        }

    let including bounds p = { 
        min = Vector.min bounds.min p
        max = Vector.max bounds.max p
        }

    let union a b = { 
        min = Vector.min a.min b.min
        max = Vector.max a.max b.max
        }

    let getSize b =
        Vector.subtract b.max b.min

    let getCenter b =
        let v = Vector.add b.max b.min
        { x = v.x / 2; y = v.y / 2 }

    let getCentered contentSize b = 
        let size = getSize b
        {
            x = b.min.x + (size.x - contentSize.x) / 2
            y = b.min.y + (size.y - contentSize.y) / 2
        }
        
    let expand margin b = {
        min = Vector.subtract b.min margin.min
        max = Vector.add b.max margin.max
        }

    let includingAll locs =
        locs 
        |> Seq.fold including maxToMin
        |> expand zeroToOne

module Tile =
    let getChar tile =
        match tile.entity with
        | Some e ->
            match e.entityType with
            | Player -> '@'
            | Minion -> 'm'
        | None ->
            match tile.terrain with
            | Floor -> '.'
            | Wall -> '#' 

    let getMoveEvents loc nextLoc dir tile = seq {
        match tile.entity with
        | Some entity -> 
            if entity.hits = 1 then yield Destroyed nextLoc
            else
                yield Attacked {
                    attackerLoc = loc
                    attackDir = dir
                    damage = 1     
                    }
        | None -> yield Moved {
            sourceLoc = loc
            moveDir = dir
            }
        }
    
    let addEntity entity tile =
        { tile with entity = Some entity }

    let removeEntity tile =
        { tile with entity = None }

module Entity =
    let player = {
        entityType = Player
        hits = 3
    }

    let minion = {
        entityType = Minion
        hits = 1
    }

    let applyDamage damage entity =
        { entity with hits = entity.hits - damage }

module Animation =
    let format =
        function
        | Moving e -> $"Moved {e.moveDir}"
        | Attacking e -> $"{e.attackerEntityType} attacked {e.targetEntityType}"
        | Destroying e -> $"{e.destroyedEntityType} destroyed"

module World =
    let empty = {
        turn = 0
        randomSeed = 0UL
        tiles = Map.empty
        animations = List.empty
        }

    let generate seed =
        let tiles = seq {
            let rand = System.Random seed
            let r = 10
            for y = -r to r do
                for x = -r to r do
                    let p = Vector.init x y
                    let terrain = if max (abs x) (abs y) = r then Wall else Floor
                    yield p, {
                        terrain = terrain
                        entity =
                            match terrain with
                            | Wall -> None
                            | Floor ->
                                if p = Vector.zero then Some Entity.player
                                elif rand.Next 8 = 0 then Some Entity.minion
                                else None
                    }
            }
        { empty with 
            randomSeed = uint64 seed
            tiles = Map.ofSeq tiles
        }

    let getMinLocation world =
        world.tiles |> Seq.map (fun kvp -> kvp.Key) |> Seq.reduce Vector.min

    let formatTiles world =
        let b = world.tiles |> Seq.map (fun kvp -> kvp.Key) |> Bounds.includingAll
        let size = Bounds.getSize b
        let dw = size.x + 1
        let data = Array.create (dw * size.y) ' '
        for y = 0 to size.y - 1 do
            data.[y * dw + dw - 1] <- '\n'
        for kvp in world.tiles do
            let p = Vector.subtract kvp.Key b.min
            data.[p.y * dw + p.x] <- Tile.getChar kvp.Value
        String(data)

    let formatAnimations world =
        world.animations
        |> List.rev
        |> Seq.map Animation.format
        |> String.concat "\n"

    let format world =
        $"Turn {world.turn}:\n{formatAnimations world}\n{formatTiles world}"

    let tryGetEntity loc world =
        Map.tryFind loc world.tiles
        |> Option.bind (fun tile -> tile.entity)

    let mapTile map loc world =
        match Map.tryFind loc world.tiles with
        | Some tile -> { world with tiles = Map.add loc (map tile) world.tiles }
        | None -> world

    let mapEntity map loc world =
        mapTile (fun tile -> { tile with entity = Option.map map tile.entity }) loc world

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
        world with animations = anim :: world.animations
        }

    let find entityType world =
        world.tiles
        |> Map.tryPick (fun loc tile ->
            tile.entity 
            |> Option.bind (fun e -> 
                if e.entityType = entityType then Some (loc, e) else None))

    let stepTurn world =
        { world with turn = world.turn + 1 }

module Action =
    let getEvents action loc world =
        match action with
        | Move dir ->
            let nextLoc = Vector.getNextLocation loc dir
            match Map.tryFind nextLoc world.tiles with
            | Some tile -> Tile.getMoveEvents loc nextLoc dir tile
            | None -> Seq.empty

    let getPlayerEvents action world =
        match World.find EntityType.Player world with
        | Some (loc, _) -> getEvents action loc world
        | None -> Seq.empty

module Event =
    let applyEvent world event =
        match event with
        | Attacked e -> 
            match World.tryGetEntity e.attackerLoc world with
            | None -> world
            | Some attacker ->
                let targetLoc = Vector.getNextLocation e.attackerLoc e.attackDir
                match World.tryGetEntity targetLoc world with
                | None -> world
                | Some target ->
                    world
                    |> World.mapEntity (Entity.applyDamage e.damage) targetLoc
                    |> World.appendAnimation (Attacking {
                        attackerLoc = e.attackerLoc
                        attackerEntityType = attacker.entityType
                        attackDir = e.attackDir
                        damage = e.damage
                        targetEntityType = target.entityType
                        })
        | Moved e -> 
            let targetLoc = Vector.getNextLocation e.sourceLoc e.moveDir
            world
            |> World.moveEntity e.sourceLoc targetLoc
            |> World.appendAnimation (Moving {
                sourceLoc = e.sourceLoc
                moveDir = e.moveDir
                })
        | Destroyed p -> 
            match World.tryGetEntity p world with
            | None -> world
            | Some target ->
                world
                |> World.removeEntity p
                |> World.appendAnimation (Destroying {
                    destroyedLoc = p
                    destroyedEntityType = target.entityType
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

    let stepWorld world action =
        let resetWorld = { world with animations = List.empty }
        let events = Action.getPlayerEvents action resetWorld
        let newWorld = 
            events
            |> Seq.fold Event.applyEvent resetWorld
            |> World.stepTurn
        newWorld

    let run world =
        readPlayerActions()
        |> Seq.scan stepWorld world
        |> Seq.iter printWorld
