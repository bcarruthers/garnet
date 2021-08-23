namespace Garnet.Samples.Flocking

open System
open System.Numerics
open System.Diagnostics
open System.Threading
open Veldrid
open Garnet.Composition
open Garnet.Numerics
open Garnet.Resources
open Garnet.Graphics
open Garnet.Input

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

type Game(fs : IReadOnlyFolder) =
    // Create window and graphics device
    let ren =
        new WindowRenderer( 
            title = "Flocking",
            width = 800,
            height = 600,
            Background = RgbaFloat(0.0f, 0.1f, 0.2f, 1.0f))
    // Initialize rendering
    let shaders = new ShaderSetCache()
    let textures = new TextureCache()
    let sprites = new SpriteRenderer(ren.Device, shaders, textures)
    do
        shaders.Load(ren.Device, fs, Resources.shaderSet)
        textures.Load(ren.Device, fs, Resources.atlas, 512, 512)
    member _.Run() =
        // Create ECS container to hold game state and handle messages
        let c = Container.Create <| fun c ->
            Disposable.Create [
                SimulationSystems.register c
                DrawingSystems.register c
                ]
        c.SetValue<TextureAtlas>(textures.[Resources.atlas])
        c.SetValue<SpriteRenderer>(sprites)
        c.SetValue<WorldSettings>(WorldSettings.defaults)
        // Initialize systems
        c.Run(Start())
        // Start timers
        let hud = FpsHud()
        let timer = UpdateTimer(16L)
        // Run loop
        let cameras = CameraSet()
        let inputs = InputCollection()
        while ren.Update(0.0f, inputs) do
            // Call systems to update
            let e = timer.Update()
            hud.OnUpdate()
            c.Run<Update>(e)
            // Draw if window is visible
            if ren.BeginDraw() then
                // Update transforms so origin is in the center of the screen and we use pixel coords
                // with +Y as up.
                let displayScale = 1.0f
                let size = ren.WindowSize.ToVector2() / displayScale
                let camera = cameras.[0]
                camera.ProjectionTransform <- Matrix4x4.CreateOrthographic(size.X, size.Y, -100.0f, 100.0f)
                // Call systems to draw
                c.Run(Draw())
                hud.Draw()
                sprites.Draw(ren.RenderContext, cameras)
                ren.EndDraw()
            // Sleep to avoid spinning CPU
            // Uncomment to test max FPS
            Thread.Sleep(1)
    interface IDisposable with
        member c.Dispose() =
            textures.Dispose()
            shaders.Dispose()
            sprites.Dispose()
            ren.Dispose()
    static member Run(fs) =
        use game = new Game(fs)
        game.Run()
