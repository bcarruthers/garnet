namespace Garnet.Samples.Flocking

open System.Collections.Generic
open System.Numerics
open Veldrid
open Garnet.Numerics

module WorldSettings =
    let defaults = {
        Seed = 1
        VehicleCount = 100
        SpawnRange = 300.0f
        MaxVehicleSpeed = 50.0f
        TrailLifespan = 0.6f
        Steering = {
            ForwardWeight = 20.0f
            CohesionWeight = 3.0f
            TetherWeight = 1.0f
            SeparationWeight = 3.0f
            AlignmentWeight = 1.0f
            MaxAlignmentDistance = 100.0f
            MaxSeparationDistance = 70.0f
            MaxCohesionDistance = 400.0f
            MaxTetherDistance = 300.0f
        }
    }

module Scalar =
    let tolerance = 1e-9f

    let clamp (s0 : float32) (s1 : float32) (s : float32) =
        s |> max s0 |> min s1

    let linearStep s0 s1 s =
        let length = s1 - s0
        if abs length < tolerance then 0.0f
        else clamp 0.0f 1.0f ((s - s0) / length)

    let smoothStep s0 s1 s =
        let x = linearStep s0 s1 s
        x * x * (3.0f - 2.0f * x)

module Heading =
    let getVelocity vehicle =
        vehicle.Speed * vehicle.Direction

    let fromVelocity (newVelocity : Vector2) =
        let newSpeed = newVelocity.Length()
        { 
            Speed = newSpeed 
            Direction = newVelocity.DivideOrZero(newSpeed)
        }

    let getNextPosition (deltaTime : float32) vehicle pos =
        let velocity = getVelocity vehicle
        let delta = deltaTime * velocity
        pos + delta

module Steering =
    let getForward current =
        current.SteerDir

    let getTether maxDistance current =
        let tetherPoint = Vector2.Zero
        let toTether = tetherPoint - current.SteerPos
        let distance = toTether.Length()
        let scale = Scalar.smoothStep 0.0f maxDistance distance
        toTether.DivideOrZero(distance) * scale

    let getCohesion minDistance maxDistance (neighbors : List<Neighbor>) =
        let mutable sum = Vector2.Zero
        for neighbor in neighbors do
            let weight = Scalar.smoothStep minDistance maxDistance neighbor.Distance
            sum <- sum + (neighbor.TeamWeight * weight) * neighbor.DirectionToNeighbor
        sum.NormalizeOrZero()

    let getSeparation maxDistance (neighbors : List<_>) =
        let mutable sum = Vector2.Zero
        for neighbor in neighbors do
            let weight = Scalar.smoothStep maxDistance 0.0f neighbor.Distance
            sum <- sum + -weight * neighbor.DirectionToNeighbor
        sum

    let getAlignment maxDistance (neighbors : List<_>) current =
        let mutable sum = Vector2.Zero
        for neighbor in neighbors do
            let weight = Scalar.smoothStep maxDistance 0.0f neighbor.Distance
            sum <- sum + -(neighbor.TeamWeight * weight) * current.SteerDir
        sum.NormalizeOrZero()

    let getSteeringDirection s neighbors current =
        let sum = 
            getForward current * s.ForwardWeight +
            getTether s.MaxTetherDistance current * s.TetherWeight +
            getCohesion s.MaxSeparationDistance s.MaxCohesionDistance neighbors * s.CohesionWeight +
            getSeparation s.MaxSeparationDistance neighbors * s.SeparationWeight +
            getAlignment s.MaxAlignmentDistance neighbors current * s.AlignmentWeight
        sum.NormalizeOrZero()
        
module Faction =
    let all = [|
        Red
        Orange
        Yellow
        Green
        Cyan
        Blue
        Purple
    |]

    let toColor = function
        | Red -> RgbaFloat(1.0f, 0.0f, 0.2f, 1.0f)
        | Orange -> RgbaFloat(1.0f, 0.4f, 0.0f, 1.0f)
        | Yellow -> RgbaFloat(0.6f, 1.0f, 0.0f, 1.0f)
        | Green -> RgbaFloat(0.0f, 1.0f, 0.1f, 1.0f)
        | Cyan -> RgbaFloat(0.0f, 0.8f, 0.6f, 1.0f)
        | Blue -> RgbaFloat(0.0f, 0.4f, 1.0f, 1.0f)
        | Purple -> RgbaFloat(0.6f, 0.0f, 1.0f, 1.0f)
        
