module Garnet.Samples.Flocking.Systems

open System
open System.Collections.Generic
open Garnet.Ecs
open Garnet.Samples.Common.Comparisons
open Garnet.Samples.Common.Numerics
open Garnet.Samples.Flocking.Domain

module CoreSystems =
    [<Struct>]
    type CurrentVehicle = {
        eid : Eid
        pos : Vec2f
        dir : Vec2f
        team : Team
        }

    // brute-force iteration over all vehicles rather than only nearby
    // using spatial partitioning
    let iterNeighbors action (c : Container) =
        fun current struct(eid : Eid, h : Heading, t : Team, p : Position) ->
            if eid <> current.eid then
                let offset = Vec2f.subtract p.pos current.pos
                let distance = Vec2f.length offset
                action { 
                    direction = h.direction
                    teamWeight = if current.team = t then 1.0f else 0.0f
                    directionToNeighbor = Vec2f.divideOrZero distance offset
                    distance = distance
                    }
        |> Iter.join4
        |> Iter.over c
        
    let createForceCalculator (c : Container) =
        let calc = 
            let settings = c.GetResource<WorldSettings>()
            Forces.getCalculator settings.steering
        let neighbors = List<_>()
        let collect = iterNeighbors neighbors.Add c
        fun current ->
            collect current
            let force = calc neighbors { 
                steerPos = current.pos
                steerDir = current.dir
                }                    
            neighbors.Clear()
            force            

    let registerUpdateVehicleHeading (c : Container) =
        // lazy because calculation depends on a resource, which may
        // not be available at the time of registering (and we don't
        // want to depend on ordering of registrations)
        let update = lazy(
            let calc = createForceCalculator c
            fun e struct(h : Heading, eid : Eid, p : Position, team : Team, veh : Vehicle) ->
                calc {
                    eid = eid
                    pos = p.pos
                    dir = h.direction
                    team = team
                    }
                |> Vec2f.multiply veh.maxSpeed
                |> Heading.fromVelocity
            |> Iter.update5
            |> Iter.over c)
        Disposable.list [
            c.On<Update> <| fun e ->
                update.Value e
            ]

    let registerReset (c : Container) =
        c.On<Reset> <| fun e ->
            c.DestroyAll()
            let settings = c.GetResource<WorldSettings>()
            let rand = Random(settings.seed)
            let nextCoord() = float32 (rand.NextDouble() - 0.5) * settings.spawnRange
            for i = 1 to settings.vehicleCount do
                c.Create()
                    .With({ maxSpeed = settings.maxVehicleSpeed; radius = 1.0f })
                    .With(Team.teams.[rand.Next Team.teams.Length])
                    .With({ pos = Vec2f.init (nextCoord()) (nextCoord()) })
                    .With({ direction = Vec2f.init 0.0f 1.0f; speed = 0.0f })
                    .Add(TrailEmitter())
                        
    let registerLifespan (c : Container) =
        c.On<Update> (
            fun e struct(ls : Lifespan, eid : Eid) ->
                let r = { lifespan = ls.lifespan - e.deltaTime }
                if ls.lifespan <= 0.0f then c.Destroy eid
                r
            |> Iter.update2
            |> Iter.over c)

    let registerUpdatePosition (c : Container) =
        c.On<Update> <| (
            fun e struct(p : Position, h : Heading) ->
                { pos = Heading.getNextPosition e.deltaTime h p.pos }
            |> Iter.update2
            |> Iter.over c)

    let registerUpdateRotation (c : Container) =
        c.On<Update> (
            fun e struct(r : Rotation, v : AngularVelocity) ->
                { radians = r.radians + e.deltaTime * v.rotationSpeed }
            |> Iter.update2
            |> Iter.over c)
        
    let registerTrailEmission (c : Container) =
        c.On<Update> (
            fun e struct(_ : TrailEmitter, p : Position, team : Team, h : Heading) ->
                c.Create()
                    .With(team)
                    .With(p)
                    .With({ radians = Vec2f.radians h.direction })
                    .With({ lifespan = 0.6f })
                    .Add(Trail())
            |> Iter.join4
            |> Iter.over c)

    let definition =
        Registration.listNamed "Core" [
            registerReset
            registerLifespan
            registerUpdatePosition
            registerUpdateRotation
            registerTrailEmission
            registerUpdateVehicleHeading
            ]

module ViewSystems =
    let registerVehicleSprites (c : Container) =
        let drawSprite = c.GetSender<Sprite>()
        c.On<Draw> (
            fun e struct(_ : Vehicle, p : Position, team : Team, h : Heading) -> 
                drawSprite {
                    radians = Vec2f.radians h.direction
                    spriteType = Triangle
                    center = p.pos
                    size = 0.1f
                    color = Team.toColor team
                    }
            |> Iter.join4
            |> Iter.over c)

    let registerTrailSprites (c : Container) =
        let drawSprite = c.GetSender<Sprite>()
        c.On<Draw> (
            fun e struct(_ : Trail, p : Position, team : Team, ls : Lifespan, r : Rotation) ->
                drawSprite {
                    radians = r.radians
                    spriteType = Hex
                    center = p.pos
                    size = ls.lifespan * 0.3f
                    color = Team.toColor team |> Rgba.multiplyAlpha (ls.lifespan * 0.3f)
                    }
            |> Iter.join5
            |> Iter.over c)

    let definition =
        Registration.listNamed "View" [
            registerVehicleSprites
            registerTrailSprites
            ]
