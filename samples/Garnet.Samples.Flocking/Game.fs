namespace Garnet.Samples.Flocking

open System
open System.Numerics
open System.Diagnostics
open System.Threading
open Veldrid
open Garnet.Engine
open Garnet.Composition
open Garnet.Samples.Flocking.Types

type UpdateTimer(fixedDeltaTime) =
    let mutable lastTime = 0L
    let mutable frameCount = 0L
    let sw = Stopwatch.StartNew()
    member c.Update() =
        let time = sw.ElapsedMilliseconds
        let result = { 
            frameNumber = frameCount
            time = time
            deltaTime = time - lastTime
            fixedDeltaTime = fixedDeltaTime
            fixedTime = time
            }
        frameCount <- frameCount + 1L
        lastTime <- time
        result

type Game(fs : IStreamSource) =
    let ren = new WindowRenderer { 
        CreateWindow.defaults with 
            title = "Flocking" 
            windowWidth = 800
            windowHeight = 600
        }
    let shaders = 
        fs.LoadShaderSet(ren.Device, 
            "texture-dual-color.vert", 
            "texture-dual-color.frag", 
            PositionTextureDualColorVertex.Description)
    let texture = fs.LoadTexture(ren.Device, "hex.png")
    let layers = new ColorTextureQuadLayers(ren.Device, shaders, texture)
    do 
        ren.Add(layers)
    member private c.Draw() =
        // Update transforms according to window size so we can draw using pixel coords
        // with origin in upper left of view
        let displayScale = 1.0f
        let size = ren.WindowSize.ToVector2() / displayScale
        layers.ProjectionTransform <- Matrix4x4.CreateOrthographic(size.X, -size.Y, -100.0f, 100.0f)
        layers.ViewTransform <- Matrix4x4.CreateTranslation(-size.X * 0.5f, -size.Y * 0.5f, 0.0f)
        // Set world transform so origin is in center of view and scaled
        layers.WorldTransform <- 
            Matrix4x4.CreateScale(1.0f) *
            Matrix4x4.CreateTranslation(size.X * 0.5f, size.Y * 0.5f, 0.0f)
        ren.Draw(RgbaFloat(0.0f, 0.1f, 0.2f, 1.0f))
    member c.Run() =
        // Create ECS container to hold game state and handle messages
        let container = Container.Create(Systems.register)
        container.RegisterInstance<ColorTextureQuadLayers>(layers)
        container.RegisterInstance<WorldSettings>(WorldSettings.defaults)
        // Start loop
        container.Run(Start())
        let timer = UpdateTimer(16L)
        while ren.Update() do
            let e = timer.Update()
            // Call systems to update/draw
            container.Run<Update>(e)
            container.Run(Draw())
            let mesh = layers.GetLayer(3)
            mesh.DrawSprite(
                center = Vector2(0.0f, 0.0f), 
                rotation = Vector2.UnitX,
                size = Vector2.One * 1.0f,
                t0 = Vector2.Zero,
                t1 = Vector2.One,
                fg = RgbaFloat.White,
                bg = RgbaFloat.Clear)
            mesh.Flush()
            // Draw to window
            ren.Invalidate()
            c.Draw()
            // Sleep to avoid spinning CPU (note sleep(1) typically takes ~15 ms)
            Thread.Sleep(1)
    interface IDisposable with
        member c.Dispose() =
            texture.Dispose()
            shaders.Dispose()
            ren.Dispose()
    static member Run(fs) =
        use game = new Game(fs)
        game.Run()
