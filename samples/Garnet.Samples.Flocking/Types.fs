namespace Garnet.Samples.Flocking.Types

open System.Numerics
open Garnet.Composition

[<Struct>]
type Faction =
    | Red
    | Orange
    | Yellow
    | Green
    | Cyan
    | Blue
    | Purple

[<Struct>]
type Position = {
    pos : Vector2
    }

[<Struct>]
type Heading = {
    direction : Vector2
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

type TrailEmitter = struct end
type Trail = struct end

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

[<Struct>]
type Steerer = {
    steerPos : Vector2
    steerDir : Vector2
    }

[<Struct>]
type Neighbor = {
    direction : Vector2
    directionToNeighbor : Vector2
    distance : float32
    teamWeight : float32
    }

[<Struct>]
type CurrentVehicle = {
    eid : Eid
    pos : Vector2
    dir : Vector2
    faction : Faction
    }

type Draw = struct end
type Reset = struct end
