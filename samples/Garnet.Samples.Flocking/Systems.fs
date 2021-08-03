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
                let settings = c.GetValue<WorldSettings>().steering
                for r in c.Query<Eid, Position, Heading, Faction, Vehicle>() do
                    let h = &r.Value3
                    let current = {
                        eid = r.Value1
                        pos = r.Value2.pos
                        dir = h.direction
                        faction = r.Value4
                        }
                    // For simplicity and testing performance, we're iterating over all vehicles
                    // rather than using any spatial partitioning.
                    for r in c.Query<Eid, Heading, Faction, Position>() do
                        if r.Value1 <> current.eid then
                            let offset = r.Value4.pos - current.pos
                            let distance = offset.Length()
                            neighbors.Add { 
                                direction = r.Value2.direction
                                teamWeight = if current.faction = r.Value3 then 1.0f else 0.0f
                                directionToNeighbor = offset.DivideOrZero(distance)
                                distance = distance
                                }
                    let current = { 
                        steerPos = current.pos
                        steerDir = current.dir
                        }                    
                    let dir = Steering.getSteeringDirection settings neighbors current
                    let velocity = dir * r.Value5.maxSpeed
                    neighbors.Clear()
                    h <- Heading.fromVelocity velocity
            ]

    let registerReset (c : Container) =
        c.On<Start> <| fun _ ->
            c.DestroyAll()
            let settings = c.GetValue<WorldSettings>()
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
            for r in c.Query<Lifespan, Eid>() do
                let ls = r.Value1
                let newLifespan = { lifespan = ls.lifespan - dt }
                if ls.lifespan <= 0.0f then
                    let eid = r.Value2
                    c.Destroy(eid)
                r.Value1 <- newLifespan

    let registerUpdatePosition (c : Container) =
        c.On<Update> <| fun e ->
            let dt = float32 e.deltaTime / 1000.0f
            for r in c.Query<Position, Heading>() do
                r.Value1 <- { pos = Heading.getNextPosition dt r.Value2 r.Value1.pos }

    let registerUpdateRotation (c : Container) =
        c.On<Update> <| fun e ->
            let dt = float32 e.deltaTime / 1000.0f
            for r in c.Query<Rotation, AngularVelocity>() do
                r.Value1 <- { radians = r.Value1.radians + dt * r.Value2.rotationSpeed }
        
    let registerTrailEmission (c : Container) =
        c.On<Update> <| fun _ ->
            for r in c.Query<TrailEmitter, Position, Faction, Heading>() do
                c.Create()
                    .With<Faction>(r.Value3)
                    .With<Position>(r.Value2)
                    .With({ radians = r.Value4.direction.GetRadians() })
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
            let atlas = c.GetValue<TextureAtlas>()
            let layers = c.GetValue<ColorTextureQuadLayers>()
            let texBounds = atlas.GetBounds("triangle.png")
            let mesh = layers.GetLayer(2)
            for r in c.Query<Vehicle, Position, Faction, Heading>() do
                mesh.DrawSprite(
                    center = r.Value2.pos, 
                    rotation = r.Value4.direction,
                    size = 0.1f * Vector2(1.0f, 1.0f) * 140.0f,
                    texBounds = texBounds,
                    fg = Faction.toColor r.Value3,
                    bg = RgbaFloat.Clear)
            mesh.Flush()

    let registerTrailSprites (c : Container) =
        c.On<Draw> <| fun _ ->
            let atlas = c.GetValue<TextureAtlas>()
            let layers = c.GetValue<ColorTextureQuadLayers>()
            let texBounds = atlas.GetBounds("hex.png")
            let mesh = layers.GetLayer(1)
            for r in c.Query<Trail, Position, Faction, Lifespan, Rotation>() do
                mesh.DrawSprite(
                    center = r.Value2.pos, 
                    rotation = Vector2.fromRadians r.Value5.radians,
                    size = r.Value4.lifespan * 0.3f * Vector2.One * 60.0f,
                    texBounds = texBounds,
                    fg = (Faction.toColor r.Value3).MultiplyAlpha(r.Value4.lifespan * 0.3f),
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
