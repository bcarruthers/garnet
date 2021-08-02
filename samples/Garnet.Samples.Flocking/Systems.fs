namespace Garnet.Samples.Flocking

open System
open System.Collections.Generic
open System.Numerics
open Veldrid
open Garnet.Samples.Engine
open Garnet.Composition
open Garnet.Samples.Flocking.Types

module CoreSystems =
    let registerSteering (c : Container) =
        let neighbors = List<Neighbor>()
        Disposable.Create [
            c.On<Update> <| fun _ ->
                let settings = c.GetInstance<WorldSettings>().steering
                for seg, eids, hs, ps, fs, vs in c.Query<Eid, Heading, Position, Faction, Vehicle>() do
                    for i in seg do
                        let current = {
                            eid = eids.[i]
                            pos = ps.[i].pos
                            dir = hs.[i].direction
                            faction = fs.[i]
                            }
                        // For simplicity and testing performance, we're iterating over all vehicles
                        // rather than using any spatial partitioning.
                        for seg, eids, hs, fs, ps in c.Query<Eid, Heading, Faction, Position>() do
                            for i in seg do
                                if eids.[i] <> current.eid then
                                    let offset = ps.[i].pos - current.pos
                                    let distance = offset.Length()
                                    neighbors.Add { 
                                        direction = hs.[i].direction
                                        teamWeight = if current.faction = fs.[i] then 1.0f else 0.0f
                                        directionToNeighbor = offset.DivideOrZero(distance)
                                        distance = distance
                                        }
                        let current = { 
                            steerPos = current.pos
                            steerDir = current.dir
                            }                    
                        let dir = Steering.getSteeringDirection settings neighbors current
                        let velocity = dir * vs.[i].maxSpeed
                        neighbors.Clear()
                        hs.[i] <- Heading.fromVelocity velocity
            ]

    let registerReset (c : Container) =
        c.On<Start> <| fun _ ->
            c.DestroyAll()
            let settings = c.GetInstance<WorldSettings>()
            let rand = Random(settings.seed)
            let nextCoord() = float32 (rand.NextDouble() - 0.5) * settings.spawnRange
            for i = 1 to settings.vehicleCount do
                c.Create()
                    .With(Faction.all.[rand.Next Faction.all.Length])
                    .With({ maxSpeed = settings.maxVehicleSpeed; radius = 1.0f })
                    .With({ pos = Vector2(nextCoord(), nextCoord()) })
                    .With({ direction = Vector2(0.0f, 1.0f); speed = 0.0f })
                    .Add(TrailEmitter())
                        
    let registerLifespan (c : Container) =
        c.On<Update> <| fun e ->
            let dt = float32 e.deltaTime / 1000.0f
            for seg, lifespans, eids in c.Query<Lifespan, Eid>() do
                for i in seg do
                    let ls = lifespans.[i]
                    let r = { lifespan = ls.lifespan - dt }
                    if ls.lifespan <= 0.0f then
                        c.Destroy(eids.[i])
                    lifespans.[i] <- r

    let registerUpdatePosition (c : Container) =
        c.On<Update> <| fun e ->
            let dt = float32 e.deltaTime / 1000.0f
            for seg, positions, headings in c.Query<Position, Heading>() do
                for i in seg do positions.[i] <- {
                    pos = Heading.getNextPosition dt headings.[i] positions.[i].pos
                }

    let registerUpdateRotation (c : Container) =
        c.On<Update> <| fun e ->
            let dt = float32 e.deltaTime / 1000.0f
            for seg, rs, vs in c.Query<Rotation, AngularVelocity>() do
                for i in seg do rs.[i] <- {
                    radians = rs.[i].radians + dt * vs.[i].rotationSpeed
                }
        
    let registerTrailEmission (c : Container) =
        c.On<Update> <| fun _ ->
            for seg, _, ps, fs, hs in c.Query<TrailEmitter, Position, Faction, Heading>() do
                for i in seg do
                    c.Create()
                        .With(fs.[i])
                        .With(ps.[i])
                        .With({ radians = hs.[i].direction.GetRadians() })
                        .With({ lifespan = 0.6f })
                        .Add(Trail())

    let register (c : Container) =
        Disposable.Create [
            registerReset c
            registerLifespan c
            registerUpdatePosition c
            registerUpdateRotation c
            registerTrailEmission c
            registerSteering c
            ]

module ViewSystems =
    let registerVehicleSprites (c : Container) =
        c.On<Draw> <| fun _ ->
            let atlas = c.GetInstance<TextureAtlas>()
            let layers = c.GetInstance<ColorTextureQuadLayers>()
            let texBounds = atlas.GetBounds("triangle.png")
            let mesh = layers.GetLayer(2)
            for seg, _, positions, factions, headings in c.Query<Vehicle, Position, Faction, Heading>() do
                for i in seg do
                    mesh.DrawSprite(
                        center = positions.[i].pos, 
                        rotation = headings.[i].direction,
                        size = 0.1f * Vector2(1.0f, 1.0f) * 140.0f,
                        texBounds = texBounds,
                        fg = Faction.toColor factions.[i],
                        bg = RgbaFloat.Clear)
            mesh.Flush()

    let registerTrailSprites (c : Container) =
        c.On<Draw> <| fun _ ->
            let atlas = c.GetInstance<TextureAtlas>()
            let layers = c.GetInstance<ColorTextureQuadLayers>()
            let texBounds = atlas.GetBounds("hex.png")
            let mesh = layers.GetLayer(1)
            let query = c.Query<Trail, Position, Faction, Lifespan, Rotation>() 
            for seg, _, positions, factions, lifespans, rotations in query do
                for i in seg do
                    mesh.DrawSprite(
                        center = positions.[i].pos, 
                        rotation = Vector2.fromRadians rotations.[i].radians,
                        size = lifespans.[i].lifespan * 0.3f * Vector2.One * 60.0f,
                        texBounds = texBounds,
                        fg = (Faction.toColor factions.[i]).MultiplyAlpha(lifespans.[i].lifespan * 0.3f),
                        bg = RgbaFloat.Clear)
            mesh.Flush()

    let register (c : Container) =
        Disposable.Create [
            registerVehicleSprites c
            registerTrailSprites c
            ]
        
module Systems =
    let register (c : Container) =
        Disposable.Create [
            CoreSystems.register c
            ViewSystems.register c
            ]
