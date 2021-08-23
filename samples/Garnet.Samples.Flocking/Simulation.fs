namespace Garnet.Samples.Flocking

open System
open System.Collections.Generic
open System.Numerics
open Garnet.Numerics
open Garnet.Composition

module SimulationSystems =
    type Container with
        member c.AddReset() =
            c.On<Start> <| fun _ ->
                c.DestroyAll()
                let settings = c.GetValue<WorldSettings>()
                let rand = Random(settings.Seed)
                let nextCoord() = float32 (rand.NextDouble() - 0.5) * settings.SpawnRange
                for i = 1 to settings.VehicleCount do
                    c.Create()
                        .With(Faction.all.[rand.Next Faction.all.Length])
                        .With({ MaxSpeed = settings.MaxVehicleSpeed; Radius = 1.0f })
                        .With({ Pos = Vector2(nextCoord(), nextCoord()) })
                        .With({ Direction = Vector2(0.0f, 1.0f); Speed = 0.0f })
                        .Add(TrailEmitter())
                            
        member c.AddSteering() =
            let neighbors = List<Neighbor>()
            c.On<Update> <| fun _ ->
                let settings = c.GetValue<WorldSettings>().Steering
                for r in c.Query<Eid, Position, Heading, Faction, Vehicle>() do
                    let h = &r.Value3
                    let current = {
                        Eid = r.Value1
                        Pos = r.Value2.Pos
                        Dir = h.Direction
                        Faction = r.Value4
                        }
                    // For simplicity and testing performance, we're iterating over all vehicles
                    // rather than using any spatial partitioning.
                    for r in c.Query<Eid, Heading, Faction, Position>() do
                        if r.Value1 <> current.Eid then
                            let offset = r.Value4.Pos - current.Pos
                            let distance = offset.Length()
                            neighbors.Add { 
                                Direction = r.Value2.Direction
                                TeamWeight = if current.Faction = r.Value3 then 1.0f else 0.0f
                                DirectionToNeighbor = offset.DivideOrZero(distance)
                                Distance = distance
                                }
                    let current = { 
                        SteerPos = current.Pos
                        SteerDir = current.Dir
                        }                    
                    let dir = Steering.getSteeringDirection settings neighbors current
                    let velocity = dir * r.Value5.MaxSpeed
                    neighbors.Clear()
                    h <- Heading.fromVelocity velocity

        member c.AddLifespan() =
            c.On<Update> <| fun e ->
                let dt = float32 e.deltaTime / 1000.0f
                for r in c.Query<Lifespan, Eid>() do
                    let ls = r.Value1
                    let newLifespan = { Lifespan = ls.Lifespan - dt }
                    if ls.Lifespan <= 0.0f then
                        let eid = r.Value2
                        c.Destroy(eid)
                    r.Value1 <- newLifespan

        member c.AddUpdatePosition() =
            c.On<Update> <| fun e ->
                let dt = float32 e.deltaTime / 1000.0f
                for r in c.Query<Position, Heading>() do
                    r.Value1 <- { Pos = Heading.getNextPosition dt r.Value2 r.Value1.Pos }

        member c.AddUpdateRotation() =
            c.On<Update> <| fun e ->
                let dt = float32 e.deltaTime / 1000.0f
                for r in c.Query<Rotation, AngularVelocity>() do
                    r.Value1 <- { Radians = r.Value1.Radians + dt * r.Value2.RotationSpeed }
            
        member c.AddTrailEmission() =
            c.On<Update> <| fun _ ->
                for r in c.Query<TrailEmitter, Position, Faction, Heading>() do
                    c.Create()
                        .With<Faction>(r.Value3)
                        .With<Position>(r.Value2)
                        .With({ Radians = r.Value4.Direction.GetRadians() })
                        .With({ Lifespan = 0.6f })
                        .Add(Trail())

    let register (c : Container) =
        Disposable.Create [
            c.AddReset()
            c.AddLifespan()
            c.AddUpdatePosition()
            c.AddUpdateRotation()
            c.AddTrailEmission()
            c.AddSteering()
            ]

