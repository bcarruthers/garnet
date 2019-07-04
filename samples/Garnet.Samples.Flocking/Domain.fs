module Garnet.Samples.Flocking.Domain

open Garnet.Samples.Common.Comparisons
open Garnet.Samples.Common.Numerics

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

// events

type Start = struct end
type Reset = struct end

[<Struct>]
type Update = {
    deltaTime : float32
    }

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

// view types

[<Struct>]
type SpriteType =
    | Triangle
    | Hex
    | Square

[<Struct>]
type Rgba = {
    red : float32
    green : float32
    blue : float32
    alpha : float32
}

[<Struct>]
type Sprite = {
    radians : float32
    spriteType : SpriteType
    center : Vec2f
    size : float32
    color : Rgba
}

[<Struct>]
type ViewSize = {
    viewSize : Vec2f
}

[<Struct>]
type Zoom = {
    zoom : float32
}

type Draw = struct end

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

module Rgba =
    let init r g b a = { 
        red = r
        green = g
        blue = b
        alpha = a 
        }

    let rgb r g b = init r g b 1.0f

    let multiplyAlpha c color =
        { color with alpha = color.alpha * c }

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

    let toColor = function
        | Red -> Rgba.init 1.0f 0.0f 0.2f 1.0f
        | Orange -> Rgba.init 1.0f 0.4f 0.0f 1.0f
        | Yellow -> Rgba.init 0.6f 1.0f 0.0f 1.0f
        | Green -> Rgba.init 0.0f 1.0f 0.1f 1.0f
        | Cyan -> Rgba.init 0.0f 0.8f 0.6f 1.0f
        | Blue -> Rgba.init 0.0f 0.4f 1.0f 1.0f
        | Purple -> Rgba.init 0.6f 0.0f 1.0f 1.0f
