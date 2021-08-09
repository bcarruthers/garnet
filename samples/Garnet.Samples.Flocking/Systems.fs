namespace Garnet.Samples.Flocking

open System
open System.Collections.Generic
open System.Numerics
open Veldrid
open Garnet.Samples.Engine
open Garnet.Composition
open Garnet.Samples.Flocking.Types

[<AutoOpen>]
module CoreSystems =
    type Container with
        member c.AddSteering() =
            let neighbors = List<Neighbor>()
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

        member c.AddReset() =
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
                            
        member c.AddLifespan() =
            c.On<Update> <| fun e ->
                let dt = float32 e.deltaTime / 1000.0f
                for r in c.Query<Lifespan, Eid>() do
                    let ls = r.Value1
                    let newLifespan = { lifespan = ls.lifespan - dt }
                    if ls.lifespan <= 0.0f then
                        let eid = r.Value2
                        c.Destroy(eid)
                    r.Value1 <- newLifespan

        member c.AddUpdatePosition() =
            c.On<Update> <| fun e ->
                let dt = float32 e.deltaTime / 1000.0f
                for r in c.Query<Position, Heading>() do
                    r.Value1 <- { pos = Heading.getNextPosition dt r.Value2 r.Value1.pos }

        member c.AddUpdateRotation() =
            c.On<Update> <| fun e ->
                let dt = float32 e.deltaTime / 1000.0f
                for r in c.Query<Rotation, AngularVelocity>() do
                    r.Value1 <- { radians = r.Value1.radians + dt * r.Value2.rotationSpeed }
            
        member c.AddTrailEmission() =
            c.On<Update> <| fun _ ->
                for r in c.Query<TrailEmitter, Position, Faction, Heading>() do
                    c.Create()
                        .With<Faction>(r.Value3)
                        .With<Position>(r.Value2)
                        .With({ radians = r.Value4.direction.GetRadians() })
                        .With({ lifespan = 0.6f })
                        .Add(Trail())

        member c.AddCoreSystems() =
            Disposable.Create [
                c.AddReset()
                c.AddLifespan()
                c.AddUpdatePosition()
                c.AddUpdateRotation()
                c.AddTrailEmission()
                c.AddSteering()
                ]

[<AutoOpen>]
module DrawingSystems =
    type Container with
        member c.AddVehicleSprites() =
            let layerDesc = {
                Depth = 2
                Blend = Blend.Alpha
                Filtering = Filtering.Linear
                Primitive = Quad
                ViewportId = 0
                }
            c.On<Draw> <| fun _ ->
                let atlas = c.GetValue<TextureAtlas>()
                let layers = c.GetValue<SpriteRenderer>()
                let texBounds = atlas.["triangle.png"].NormalizedBounds
                let mesh = layers.GetLayer(layerDesc)
                for r in c.Query<Vehicle, Position, Faction, Heading>() do
                    mesh.DrawSprite(
                        center = r.Value2.pos, 
                        rotation = r.Value4.direction,
                        size = 0.1f * Vector2(1.0f, 1.0f) * 140.0f,
                        texBounds = texBounds,
                        fg = Faction.toColor r.Value3,
                        bg = RgbaFloat.Clear)
                mesh.Flush()

        member c.AddTrailSprites() =
            let layerDesc = {
                Depth = 1
                Blend = Blend.Alpha
                Filtering = Filtering.Linear
                Primitive = Quad
                ViewportId = 0
                }
            c.On<Draw> <| fun _ ->
                let atlas = c.GetValue<TextureAtlas>()
                let layers = c.GetValue<SpriteRenderer>()
                let texBounds = atlas.["hex.png"].NormalizedBounds
                let mesh = layers.GetLayer(layerDesc)
                for r in c.Query<Trail, Position, Faction, Lifespan, Rotation>() do
                    mesh.DrawSprite(
                        center = r.Value2.pos, 
                        rotation = Vector2.fromRadians r.Value5.radians,
                        size = r.Value4.lifespan * 0.3f * Vector2.One * 60.0f,
                        texBounds = texBounds,
                        fg = (Faction.toColor r.Value3).MultiplyAlpha(r.Value4.lifespan * 0.3f),
                        bg = RgbaFloat.Clear)
                mesh.Flush()

        member c.AddDrawingSystems() =
            Disposable.Create [
                c.AddVehicleSprites()
                c.AddTrailSprites()
                ]
