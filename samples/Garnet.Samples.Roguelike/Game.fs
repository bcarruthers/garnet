namespace Garnet.Samples.Roguelike

open System
open System.Numerics
open System.Threading
open Veldrid
open Garnet.Numerics
open Garnet.Composition
open Garnet.Graphics
open Garnet.Input

module Resources =
    let tileTexture = "drake-10x10-transparent.png"

    let shaderSet : ShaderSetDescriptor<PositionTextureDualColorVertex> = {
        VertexShader = "texture-dual-color.vert"
        FragmentShader = "texture-dual-color.frag"
        }

    // Use point sampling for pixelated appearance
    let pipeline = {
        Blend = Blend.Alpha
        Filtering = Filtering.Point
        ShaderSet = shaderSet
        Texture = tileTexture
        }

    // Avoid auto flush since we only update when an action occurs
    let tileLayer = {
        LayerId = 0
        CameraId = 0
        Primitive = Quad
        FlushMode = NoFlush
        Pipeline = pipeline 
        }

module Command =
    let getCommand =
        function
        | Key.R -> Command.Reset
        | Key.Right -> Command.MoveEast
        | Key.Up -> Command.MoveNorth
        | Key.Left -> Command.MoveWest
        | Key.Down -> Command.MoveSouth
        | Key.F11 -> Command.FullScreen
        | _ -> Command.None

    let tryGetAction =
        function
        | Command.MoveEast -> Move East |> Some
        | Command.MoveNorth -> Move North |> Some
        | Command.MoveWest -> Move West |> Some
        | Command.MoveSouth -> Move South |> Some
        | _ -> None

[<AutoOpen>]
module DrawingExtensions =
    type SpriteRenderer with
        member c.DrawWorld(world) =
            let tiles = c.GetVertices(Resources.tileLayer)
            tiles.DrawWorld(world)
            tiles.Flush()

type Game(fs : IReadOnlyFolder) =
    // Image is a tilemap of ASCII chars (16x16=256 tiles)
    let image = fs.LoadImage(Resources.tileTexture)
    // Calculate window size to match map and tile size
    let tileScale = 2
    let tileWidth = image.Width / 16
    let tileHeight = image.Height / 16
    let mapRadius = 15
    let mapExtent = mapRadius * 2 + 1
    // Create window and graphics device
    let ren =
        new WindowRenderer {
            WindowSettings.Default with
                Title = "Roguelike"
                Width = mapExtent * tileWidth * tileScale
                Height = mapExtent * tileHeight * tileScale
                Redraw = Redraw.Manual
                Background = RgbaFloat.Black
                }
    // Initialize rendering
    let shaders = new ShaderSetCache()
    let cache = new ResourceCache()
    let sprites = new SpriteRenderer(ren.Device, shaders, cache)
    do 
        cache.AddShaderLoaders(ren.Device)
        fs.LoadShadersFromFolder(".", ren.Device.BackendType, cache)
        cache.AddResource(Resources.tileTexture, ren.Device.CreateTexture(image))
    member c.Run() =
        // Set transforms so drawing code can use tile coords for source (16x16 tileset) 
        // and destination (80x25 display tiles)
        let cameras = CameraSet()
        let texTileSize = 1.0f / 16.0f
        let camera = cameras.[0]
        camera.WorldTransform <- Matrix4x4.CreateScale(float32 tileWidth, float32 tileHeight, 1.0f)
        camera.TextureTransform <- Matrix4x4.CreateScale(texTileSize, texTileSize, 1.0f)
        // Start loop
        let inputs = InputCollection()
        let mutable state = World.generate mapRadius 1
        sprites.DrawWorld(state)
        while ren.Update(0.0f, inputs) do
            for e in inputs.KeyDownEvents do
                match Command.getCommand e.KeyCode with
                | Command.FullScreen -> ren.ToggleFullScreen()
                | Command.None -> ()
                | command ->
                    match Command.tryGetAction command with
                    | None -> ()
                    | Some action -> 
                        state <- Loop.stepWorld state action
                        sprites.DrawWorld(state)
                        // Note we only invalidate when something changes instead of every frame
                        ren.Invalidate()
            if ren.BeginDraw() then
                // Update transforms according to window size so we can draw using pixel coords
                // with origin in upper left of view
                let displayScale = float32 tileScale
                let size = ren.Size.ToVector2() / displayScale
                camera.ProjectionTransform <- Matrix4x4.CreateOrthographic(size.X, -size.Y, -100.0f, 100.0f)
                camera.ViewTransform <- Matrix4x4.CreateTranslation(-size.X * 0.5f, -size.Y * 0.5f, 0.0f)
                sprites.Draw(ren.RenderContext, cameras)
                ren.EndDraw()
            // Sleep to avoid spinning CPU
            Thread.Sleep(1)
    interface IDisposable with
        member c.Dispose() =
            cache.Dispose()
            shaders.Dispose()
            sprites.Dispose()
            ren.Dispose()
    static member Run(fs) =
        use game = new Game(fs)
        game.Run()
