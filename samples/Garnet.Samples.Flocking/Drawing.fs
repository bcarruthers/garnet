namespace Garnet.Samples.Flocking

open System
open System.Numerics
open Veldrid
open Garnet.Composition
open Garnet.Numerics
open Garnet.Graphics

module DrawingSystems =
    type Container with
        member c.AddVehicleSprites() =
            c.On<Draw> <| fun _ ->
                let atlas = c.GetValue<TextureAtlas>()
                let layers = c.GetValue<SpriteRenderer>()
                let texBounds = atlas.[Resources.triangleTexture].NormalizedBounds
                let mesh = layers.GetVertices(Resources.vehicleLayer)
                for r in c.Query<Vehicle, Position, Faction, Heading>() do
                    mesh.DrawQuad(
                        center = r.Value2.Pos, 
                        rotation = r.Value4.Direction,
                        size = 0.1f * Vector2(1.0f, 1.0f) * 140.0f,
                        texBounds = texBounds,
                        color = Faction.toColor r.Value3)

        member c.AddTrailSprites() =
            c.On<Draw> <| fun _ ->
                let atlas = c.GetValue<TextureAtlas>()
                let layers = c.GetValue<SpriteRenderer>()
                let texBounds = atlas.[Resources.hexTexture].NormalizedBounds
                let mesh = layers.GetVertices(Resources.trailLayer)
                for r in c.Query<Trail, Position, Faction, Lifespan, Rotation>() do
                    mesh.DrawQuad(
                        center = r.Value2.Pos, 
                        rotation = Vector2.FromRadians(r.Value5.Radians),
                        size = r.Value4.Lifespan * 0.3f * Vector2.One * 60.0f,
                        texBounds = texBounds,
                        color = (Faction.toColor r.Value3).MultiplyAlpha(r.Value4.Lifespan * 0.3f))

    let register (c : Container) =
        Disposable.Create [
            c.AddVehicleSprites()
            c.AddTrailSprites()
            ]
