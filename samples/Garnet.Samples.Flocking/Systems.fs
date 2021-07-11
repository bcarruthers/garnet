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
        let collectNeighbors =
            // for simplicity, brute-force iteration over all vehicles
            fun struct(current, neighbors : List<Neighbor>) struct(eid : Eid, h : Heading, t : Faction, p : Position) ->
                if eid <> current.eid then
                    let offset = p.pos - current.pos
                    let distance = offset.Length()
                    neighbors.Add { 
                        direction = h.direction
                        teamWeight = if current.faction = t then 1.0f else 0.0f
                        directionToNeighbor = offset.DivideOrZero(distance)
                        distance = distance
                        }
            |> Join.iter4
            |> Join.over c
        let update = 
            let neighbors = List<Neighbor>()
            fun settings struct(h : Heading, eid : Eid, p : Position, faction : Faction, veh : Vehicle) ->
                let current = {
                    eid = eid
                    pos = p.pos
                    dir = h.direction
                    faction = faction
                    }
                collectNeighbors struct(current, neighbors)
                let steerer = { 
                    steerPos = current.pos
                    steerDir = current.dir
                    }                    
                let dir = Steering.getSteeringDirection settings neighbors steerer
                let velocity = dir * veh.maxSpeed
                neighbors.Clear()
                Heading.fromVelocity velocity
            |> Join.update5
            |> Join.over c
        Disposable.Create [
            c.On<Update> <| fun e ->
                let settings = c.GetInstance<WorldSettings>().steering
                update settings
            ]

    let registerReset (c : Container) =
        c.On<Start> <| fun e ->
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
        let update = 
            fun dt struct(ls : Lifespan, eid : Eid) ->
                let r = { lifespan = ls.lifespan - dt }
                if ls.lifespan <= 0.0f then c.Destroy eid
                r
            |> Join.update2
            |> Join.over c
        c.On<Update> <| fun e ->
            let dt = float32 e.deltaTime / 1000.0f
            update dt

    let registerUpdatePosition (c : Container) =
        let update =
            fun dt struct(p : Position, h : Heading) -> { 
                pos = Heading.getNextPosition dt h p.pos 
                }
            |> Join.update2
            |> Join.over c
        c.On<Update> <| fun e ->
            let dt = float32 e.deltaTime / 1000.0f
            update dt

    let registerUpdateRotation (c : Container) =
        let update =
            fun dt struct(r : Rotation, v : AngularVelocity) -> { 
                radians = r.radians + dt * v.rotationSpeed 
                }
            |> Join.update2
            |> Join.over c
        c.On<Update> <| fun e ->
            let dt = float32 e.deltaTime / 1000.0f
            update dt
        
    let registerTrailEmission (c : Container) =
        c.On<Update> (
            fun e struct(_ : TrailEmitter, p : Position, faction : Faction, h : Heading) ->
                c.Create()
                    .With(faction)
                    .With(p)
                    .With({ radians = h.direction.GetRadians() })
                    .With({ lifespan = 0.6f })
                    .Add(Trail())
            |> Join.iter4
            |> Join.over c)

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
        let update =
            fun struct(texBounds, mesh : BufferedQuadMesh<PositionTextureDualColorVertex>) 
                struct(_ : Vehicle, p : Position, faction : Faction, h : Heading) -> 
                mesh.DrawSprite(
                    center = p.pos, 
                    rotation = h.direction,
                    size = 0.1f * Vector2(1.0f, 1.0f) * 140.0f,
                    texBounds = texBounds,
                    fg = Faction.toColor faction,
                    bg = RgbaFloat.Clear)
            |> Join.iter4
            |> Join.over c
        c.On<Draw> <| fun e ->
            let atlas = c.GetInstance<TextureAtlas>()
            let layers = c.GetInstance<ColorTextureQuadLayers>()
            let texBounds = atlas.GetBounds("triangle.png")
            let mesh = layers.GetLayer(2)
            update struct(texBounds, mesh)
            mesh.Flush()

    let registerTrailSprites (c : Container) =
        let update =
            fun struct(texBounds, mesh : BufferedQuadMesh<PositionTextureDualColorVertex>) 
                struct(_ : Trail, p : Position, faction : Faction, ls : Lifespan, r : Rotation) ->
                mesh.DrawSprite(
                    center = p.pos, 
                    rotation = Vector2.fromRadians r.radians,
                    size = ls.lifespan * 0.3f * Vector2.One * 60.0f,
                    texBounds = texBounds,
                    fg = (Faction.toColor faction).MultiplyAlpha(ls.lifespan * 0.3f),
                    bg = RgbaFloat.Clear)
            |> Join.iter5
            |> Join.over c
        c.On<Draw> <| fun e ->
            let atlas = c.GetInstance<TextureAtlas>()
            let layers = c.GetInstance<ColorTextureQuadLayers>()
            let texBounds = atlas.GetBounds("hex.png")
            let mesh = layers.GetLayer(1)
            update struct(texBounds, mesh)
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
