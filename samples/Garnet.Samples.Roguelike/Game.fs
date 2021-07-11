namespace Garnet.Samples.Roguelike

open System
open System.Numerics
open System.Threading
open Veldrid
open Garnet.Samples.Engine
open Garnet.Samples.Roguelike.Types

module Command =
    let getCommand =
        function
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

type Game(fs : IStreamSource) =
    // Image is a tilemap of ASCII chars (16x16=256 tiles)
    let image = fs.LoadImage("drake-10x10-transparent.png")
    let tileScale = 3
    let tileWidth = image.Width / 16
    let tileHeight = image.Height / 16
    let ren = new WindowRenderer { 
        CreateWindow.defaults with 
            title = "Roguelike" 
            windowWidth = 21 * tileWidth * tileScale
            windowHeight = 21 * tileHeight * tileScale
        }
    let shaders = 
        fs.LoadShaderSet(ren.Device, 
            "texture-dual-color.vert", 
            "texture-dual-color.frag", 
            PositionTextureDualColorVertex.Description)
    let texture = ren.Device.CreateTexture(image)
    // Use point sampling for pixelated appearance
    let layers = new ColorTextureQuadLayers(ren.Device, shaders, texture, ren.Device.PointSampler)
    let audio = new AudioDevice()
    //let blip = fs.LoadWave(audio, "blip.wav")
    do 
        // Set transforms so drawing code can use tile coords for source (16x16 tileset) 
        // and destination (80x25 display tiles)
        let texTileSize = 1.0f / 16.0f
        layers.WorldTransform <- Matrix4x4.CreateScale(float32 tileWidth, float32 tileHeight, 1.0f)
        layers.TextureTransform <- Matrix4x4.CreateScale(texTileSize, texTileSize, 1.0f)
        ren.Add(layers)
    member private c.Draw() =
        // Update transforms according to window size so we can draw using pixel coords
        // with origin in upper left of view
        let displayScale = float32 tileScale
        let size = ren.WindowSize.ToVector2() / displayScale
        layers.ProjectionTransform <- Matrix4x4.CreateOrthographic(size.X, -size.Y, -100.0f, 100.0f)
        layers.ViewTransform <- Matrix4x4.CreateTranslation(-size.X * 0.5f, -size.Y * 0.5f, 0.0f)
        ren.Draw(RgbaFloat.Blue)
    member private c.DrawWorld(world) =
        let tiles = layers.GetLayer(1)
        tiles.DrawWorld(world)
        tiles.Flush()
        ren.Invalidate()
    member c.Run() =
        // Start loop
        let mutable state = World.generate 1
        c.DrawWorld(state)
        while ren.Update() do
            for e in ren.Inputs.KeyDownEvents do
                match Command.getCommand e.keyCode with
                | Command.FullScreen -> ren.ToggleFullScreen()
                | Command.None -> ()
                | command ->
                    match Command.tryGetAction command with
                    | None -> ()
                    | Some action -> 
                        state <- Loop.stepWorld state action
                        c.DrawWorld(state)
            c.Draw()
            // Sleep to avoid spinning CPU (note sleep(1) typically takes ~15 ms)
            Thread.Sleep(1)
    interface IDisposable with
        member c.Dispose() =
            texture.Dispose()
            shaders.Dispose()
            ren.Dispose()
            audio.Dispose()
    static member Run(fs) =
        use game = new Game(fs)
        game.Run()
