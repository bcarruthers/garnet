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
                    mesh.DrawSprite(
                        center = r.Value2.pos, 
                        rotation = r.Value4.direction,
                        size = 0.1f * Vector2(1.0f, 1.0f) * 140.0f,
                        texBounds = texBounds,
                        fg = Faction.toColor r.Value3,
                        bg = RgbaFloat.Clear)

        member c.AddTrailSprites() =
            c.On<Draw> <| fun _ ->
                let atlas = c.GetValue<TextureAtlas>()
                let layers = c.GetValue<SpriteRenderer>()
                let texBounds = atlas.[Resources.hexTexture].NormalizedBounds
                let mesh = layers.GetVertices(Resources.trailLayer)
                for r in c.Query<Trail, Position, Faction, Lifespan, Rotation>() do
                    mesh.DrawSprite(
                        center = r.Value2.pos, 
                        rotation = Vector2.FromRadians(r.Value5.radians),
                        size = r.Value4.lifespan * 0.3f * Vector2.One * 60.0f,
                        texBounds = texBounds,
                        fg = (Faction.toColor r.Value3).MultiplyAlpha(r.Value4.lifespan * 0.3f),
                        bg = RgbaFloat.Clear)

    let register (c : Container) =
        Disposable.Create [
            c.AddVehicleSprites()
            c.AddTrailSprites()
            ]
