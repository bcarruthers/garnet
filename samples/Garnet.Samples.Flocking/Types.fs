namespace Garnet.Samples.Flocking

open System.Numerics
open Garnet.Composition

[<AutoOpen>]
module Components =
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
        Pos : Vector2
        }

    [<Struct>]
    type Heading = {
        Direction : Vector2
        Speed : float32
        }

    [<Struct>]
    type Vehicle = {
        Radius : float32
        MaxSpeed : float32
        }

    [<Struct>]
    type TrailLifespan = {
        TrailLifespan : float32
        }

    [<Struct>]
    type Lifespan = {
        Lifespan : float32
        }

    [<Struct>]
    type AngularVelocity = {
        RotationSpeed : float32
        }

    [<Struct>]
    type Rotation = {
        Radians : float32
        }

    type TrailEmitter = struct end
    type Trail = struct end

[<AutoOpen>]
module Settings =
    type SteeringSettings = {
        ForwardWeight : float32
        CohesionWeight : float32
        TetherWeight : float32
        SeparationWeight : float32
        AlignmentWeight : float32
        MaxAlignmentDistance : float32
        MaxSeparationDistance : float32
        MaxCohesionDistance : float32
        MaxTetherDistance : float32
        }

    type WorldSettings = {
        Seed : int
        SpawnRange : float32
        VehicleCount : int
        MaxVehicleSpeed : float32
        TrailLifespan : float32
        Steering : SteeringSettings
        }

[<AutoOpen>]
module SteeringTypes =
    [<Struct>]
    type Steerer = {
        SteerPos : Vector2
        SteerDir : Vector2
        }

    [<Struct>]
    type Neighbor = {
        Direction : Vector2
        DirectionToNeighbor : Vector2
        Distance : float32
        TeamWeight : float32
        }

    [<Struct>]
    type CurrentVehicle = {
        Eid : Eid
        Pos : Vector2
        Dir : Vector2
        Faction : Faction
        }

[<AutoOpen>]
module Events =
    type Start = struct end
    type Draw = struct end
    type Reset = struct end
