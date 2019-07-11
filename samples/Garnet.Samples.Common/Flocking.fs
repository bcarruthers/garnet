module Garnet.Samples.Flocking

module Domain =
    open Garnet.Samples.Comparisons
    open Garnet.Samples.Numerics

    [<Struct>]
    type Team =
        | Red
        | Orange
        | Yellow
        | Green
        | Cyan
        | Blue
        | Purple

    [<Struct>]
    type Position = {
        pos : Vec2f
        }

    [<Struct>]
    type Heading = {
        direction : Vec2f
        speed : float32
        }

    [<Struct>]
    type Vehicle = {
        radius : float32
        maxSpeed : float32
        }

    [<Struct>]
    type TrailLifespan = {
        trailLifespan : float32
    }

    [<Struct>]
    type Lifespan = {
        lifespan : float32
        }

    [<Struct>]
    type AngularVelocity = {
        rotationSpeed : float32
        }

    [<Struct>]
    type Rotation = {
        radians : float32
    }

    // markers

    type TrailEmitter = struct end
    type Trail = struct end
    
    // resources

    type SteeringSettings = {
        forwardWeight : float32
        cohesionWeight : float32
        tetherWeight : float32
        separationWeight : float32
        alignmentWeight : float32
        maxAlignmentDistance : float32
        maxSeparationDistance : float32
        maxCohesionDistance : float32
        maxTetherDistance : float32
    }

    type WorldSettings = {
        seed : int
        spawnRange : float32
        vehicleCount : int
        maxVehicleSpeed : float32
        trailLifespan : float32
        steering : SteeringSettings
    }

    // internal types

    [<Struct>]
    type Steerer = {
        steerPos : Vec2f
        steerDir : Vec2f
    }

    [<Struct>]
    type Neighbor = {
        direction : Vec2f
        directionToNeighbor : Vec2f
        distance : float32
        teamWeight : float32
    }

    module WorldSettings =
        let settings = {
            seed = 1
            vehicleCount = 100
            spawnRange = 300.0f
            maxVehicleSpeed = 50.0f
            trailLifespan = 0.6f
            steering = {
                forwardWeight = 20.0f
                cohesionWeight = 3.0f
                tetherWeight = 1.0f
                separationWeight = 3.0f
                alignmentWeight = 1.0f
                maxAlignmentDistance = 100.0f
                maxSeparationDistance = 70.0f
                maxCohesionDistance = 400.0f
                maxTetherDistance = 300.0f
            }
        }

    module Heading =
        let getVelocity vehicle =
            Vec2f.multiply vehicle.speed vehicle.direction

        let fromVelocity newVelocity =
            let newSpeed = Vec2f.length newVelocity
            { 
                speed = newSpeed 
                direction = Vec2f.divideOrZero newSpeed newVelocity 
            }

        let getNextPosition deltaTime vehicle pos =
            let velocity = getVelocity vehicle
            let delta = Vec2f.multiply deltaTime velocity
            Vec2f.add pos delta

    module Forces =
        open System.Collections.Generic

        let getForward (neighbors : List<_>) current =
            current.steerDir

        let getTether maxDistance (neighbors : List<_>) current =
            let tetherPoint = Vec2f.init 0.0f 0.0f
            let toTether = Vec2f.subtract tetherPoint current.steerPos
            let distance = Vec2f.length toTether
            let scale = Scalar.smoothStep 0.0f maxDistance distance
            Vec2f.divideOrZero distance toTether
            |> Vec2f.multiply scale

        let getCohesion minDistance maxDistance (neighbors : List<_>) current =
            let mutable sum = Vec2f.zero
            for neighbor in neighbors do
                let weight = Scalar.smoothStep minDistance maxDistance neighbor.distance
                sum <- Vec2f.add sum <| Vec2f.multiply (neighbor.teamWeight * weight) neighbor.directionToNeighbor
            Vec2f.normalizeOrZero sum

        let getSeparation maxDistance (neighbors : List<_>) current =
            let mutable sum = Vec2f.zero
            for neighbor in neighbors do
                let weight = Scalar.smoothStep maxDistance 0.0f neighbor.distance
                sum <- Vec2f.add sum <| Vec2f.multiply -weight neighbor.directionToNeighbor
            sum

        let getAlignment maxDistance (neighbors : List<_>) current =
            let mutable sum = Vec2f.zero
            for neighbor in neighbors do
                let weight = Scalar.smoothStep maxDistance 0.0f neighbor.distance
                sum <- Vec2f.add sum <| Vec2f.multiply -(neighbor.teamWeight * weight) current.steerDir
            Vec2f.normalizeOrZero sum
    
        let getCalculator s =
            let calculators = [|
                s.forwardWeight, getForward 
                s.tetherWeight, getTether s.maxTetherDistance
                s.cohesionWeight, getCohesion s.maxSeparationDistance s.maxCohesionDistance
                s.separationWeight, getSeparation s.maxSeparationDistance
                s.alignmentWeight, getAlignment s.maxAlignmentDistance
                |]
            fun neighbors current ->
                let mutable sum = Vec2f.zero
                for (weight, calc) in calculators do
                    let force = 
                        calc neighbors current
                        |> Vec2f.multiply weight
                    sum <- Vec2f.add sum force
                Vec2f.normalizeOrZero sum

    module Team =
        let teams = [|
            Red
            Orange
            Yellow
            Green
            Cyan
            Blue
            Purple
        |]

module Systems =
    open System
    open System.Collections.Generic
    open Garnet.Ecs
    open Garnet.Samples.Comparisons
    open Garnet.Samples.Numerics
    open Garnet.Samples.FrameworkTypes
    open Domain

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
            |> Join.iter4
            |> Join.over c
        
        let createForceCalculator (c : Container) =
            let calc = 
                let settings = c.GetInstance<WorldSettings>()
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
                |> Join.update5
                |> Join.over c)
            Disposable.list [
                c.On<Update> <| fun e ->
                    update.Value e
                ]

        let registerReset (c : Container) =
            c.On<Reset> <| fun e ->
                c.DestroyAll()
                let settings = c.GetInstance<WorldSettings>()
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
                |> Join.update2
                |> Join.over c)

        let registerUpdatePosition (c : Container) =
            c.On<Update> <| (
                fun e struct(p : Position, h : Heading) ->
                    { pos = Heading.getNextPosition e.deltaTime h p.pos }
                |> Join.update2
                |> Join.over c)

        let registerUpdateRotation (c : Container) =
            c.On<Update> (
                fun e struct(r : Rotation, v : AngularVelocity) ->
                    { radians = r.radians + e.deltaTime * v.rotationSpeed }
                |> Join.update2
                |> Join.over c)
        
        let registerTrailEmission (c : Container) =
            c.On<Update> (
                fun e struct(_ : TrailEmitter, p : Position, team : Team, h : Heading) ->
                    c.Create()
                        .With(team)
                        .With(p)
                        .With({ radians = Vec2f.radians h.direction })
                        .With({ lifespan = 0.6f })
                        .Add(Trail())
                |> Join.iter4
                |> Join.over c)

        let registerDefaultSettings (c : Container) =
            c.Register <| fun () -> WorldSettings.settings
            Disposable.empty

        let definition =
            Registration.listNamed "Core" [
                registerReset
                registerLifespan
                registerUpdatePosition
                registerUpdateRotation
                registerTrailEmission
                registerUpdateVehicleHeading
                registerDefaultSettings
                ]

    module ViewSystems =
        module Team =
            let toColor = function
                | Red -> Rgba.init 1.0f 0.0f 0.2f 1.0f
                | Orange -> Rgba.init 1.0f 0.4f 0.0f 1.0f
                | Yellow -> Rgba.init 0.6f 1.0f 0.0f 1.0f
                | Green -> Rgba.init 0.0f 1.0f 0.1f 1.0f
                | Cyan -> Rgba.init 0.0f 0.8f 0.6f 1.0f
                | Blue -> Rgba.init 0.0f 0.4f 1.0f 1.0f
                | Purple -> Rgba.init 0.6f 0.0f 1.0f 1.0f

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
                |> Join.iter4
                |> Join.over c)

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
                |> Join.iter5
                |> Join.over c)

        let definition =
            Registration.listNamed "View" [
                registerVehicleSprites
                registerTrailSprites
                ]
        
    let test() =
        let c = Container()    
        CoreSystems.definition.register c |> ignore
        c.RegisterInstance WorldSettings.settings//{ settings with vehicleCount = 10 }
        c.Run <| Reset()
        for i = 1 to 10 do
            c.Run <| { Update.deltaTime = 0.1f }
        c.Get(Eid 64).ToString()

    let definition = 
        Registration.combine [ 
            CoreSystems.definition
            ViewSystems.definition
            ]
