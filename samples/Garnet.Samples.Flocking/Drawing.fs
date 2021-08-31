namespace Garnet.Samples.Flocking

open System
open System.Numerics
open Garnet.Composition
open Garnet.Numerics
open Garnet.Graphics

module DrawingSystems =
    type Container with
        member c.AddCameraUpdates() =
            c.On<Draw> <| fun e ->
                // Update transforms so origin is in the center of the screen and we use pixel coords
                // with +Y as up.
                let displayScale = 1.0f
                let size = e.ViewSize.ToVector2() / displayScale
                let camera = c.Get<CameraSet>().[0]
                camera.ProjectionTransform <- Matrix4x4.CreateOrthographic(size.X, size.Y, -100.0f, 100.0f)

        member c.AddVehicleSprites() =
            c.On<Draw> <| fun _ ->
                let atlas = c.LoadResource<TextureAtlas>(Resources.atlas)
                let layers = c.Get<SpriteRenderer>()
                let texBounds = atlas.[Resources.triangleTexture].NormalizedBounds
                let mesh = layers.GetVertices(Resources.vehicleLayer)
                for r in c.Query<Vehicle, Position, Faction, Heading>() do
                    mesh.DrawQuad {
                        Center = r.Value2.Pos
                        Size = 0.1f * Vector2(1.0f, 1.0f) * 140.0f
                        Rotation = r.Value4.Direction
                        TexBounds = texBounds
                        Color = Faction.toColor r.Value3
                        }

        member c.AddTrailSprites() =
            c.On<Draw> <| fun _ ->
                let atlas = c.LoadResource<TextureAtlas>(Resources.atlas)
                let layers = c.Get<SpriteRenderer>()
                let texBounds = atlas.[Resources.hexTexture].NormalizedBounds
                let mesh = layers.GetVertices(Resources.trailLayer)
                for r in c.Query<Trail, Position, Faction, Lifespan, Rotation>() do
                    mesh.DrawQuad {
                        Center = r.Value2.Pos 
                        Size = r.Value4.Lifespan * 0.3f * Vector2.One * 60.0f
                        Rotation = Vector2.FromRadians(r.Value5.Radians)
                        TexBounds = texBounds
                        Color = (Faction.toColor r.Value3).MultiplyAlpha(r.Value4.Lifespan * 0.3f)
                        }

    let add (c : Container) =
        Disposable.Create [
            c.AddCameraUpdates()
            c.AddVehicleSprites()
            c.AddTrailSprites()
            ]
