namespace Garnet.Composition

open System
open System.IO
open System.Runtime.CompilerServices
open System.Reflection
open Veldrid
open Garnet.Input
open Garnet.Graphics
open Garnet.Audio
open Garnet.Composition

type AssetSettings = {
    AssetPath : string
    }

[<Extension>]
type Systems =
    [<Extension>]
    static member AddAssetsFolder(c : Container) =
        let folder =
            let path = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
            let path = if String.IsNullOrEmpty(path) then "" else path
            new FileFolder(Path.Combine(path, "assets"))
        let cache = c.Get<ResourceCache>()
        cache.SetFolder(folder)
        Disposable.Create [
            folder :> IDisposable
            ]
        
    [<Extension>]
    static member AddTextLoaders(c : Container) =
        let cache = c.Get<ResourceCache>()
        cache.AddTextLoaders()
        Disposable.Null

    [<Extension>]
    static member AddGraphicsDevice(c : Container) =
        let settings = c.GetOrSetDefault(WindowSettings.Default)
        let ren = new WindowRenderer(settings)
        let shaders = new ShaderSetCache()
        c.Set<WindowRenderer>(ren)
        c.Set<GraphicsDevice>(ren.Device)
        c.Set<RenderContext>(ren.RenderContext)
        c.Set<ShaderSetCache>(shaders)
        Disposable.Create [
            ren :> IDisposable
            shaders :> IDisposable
            ]
        
    [<Extension>]
    static member AddGraphicsLoaders(c : Container) =
        let device = c.Get<GraphicsDevice>()
        let cache = c.Get<ResourceCache>()
        cache.AddShaderLoaders(device)
        cache.AddTextureLoaders(device)
        cache.AddFontLoaders()
        Disposable.Null

    [<Extension>]
    static member AddWindowRendering(c : Container) =
        Disposable.Create [
            c.On<PreUpdate> <| fun e ->
                let ren = c.Get<WindowRenderer>()
                let inputs = c.Get<InputCollection>()
                let deltaTime = float32 e.Update.DeltaTime / 1000.0f
                let isRunning = ren.Update(deltaTime, inputs)
                if not isRunning then
                    c.Send(Closing())
            c.On<PostUpdate> <| fun e ->
                let ren = c.Get<WindowRenderer>()
                c.Start <| seq {
                    let draw = {
                        Update = e.Update
                        ViewSize = ren.Size
                        }
                    yield c.Wait<Draw>(draw)
                    // This check must come after since the drawing stage may
                    // have invalidated renderer, causing check to pass.
                    if ren.BeginDraw() then
                        yield c.Wait(PushDrawCommands())
                        ren.EndDraw()
                    }
            ]
        
    [<Extension>]
    static member AddInputPublishing(c : Container) =
        Disposable.Create [
            c.On<HandleInput> <| fun _ ->
                let inputs = c.Get<InputCollection>()
                for e in inputs.KeyUpEvents do
                    c.Send<KeyUp> e
                for e in inputs.KeyDownEvents do
                    c.Send<KeyDown> e
                c.Send<MouseMoved> { 
                    devicePos = inputs.MousePosition
                    deviceDelta = inputs.MouseDelta
                    pos = inputs.NormalizedMousePosition
                    delta = inputs.NormalizedMouseDelta
                    modifiers = inputs.Modifiers
                    }
                if abs inputs.WheelDelta > 0.0f then
                    c.Send<MouseWheel> { 
                        modifiers = int inputs.Modifiers
                        wheel = int inputs.WheelDelta
                    }
                for e in inputs.MouseDownEvents do
                    c.Send<MouseDown> { 
                        devicePos = inputs.MousePosition
                        pos = inputs.NormalizedMousePosition
                        button = e
                        modifiers = inputs.Modifiers
                        }
                for e in inputs.MouseUpEvents do
                    c.Send<MouseUp> { 
                        devicePos = inputs.MousePosition
                        pos = inputs.NormalizedMousePosition
                        button = e
                        }
            ]
        
    [<Extension>]
    static member AddSpriteDrawing(c : Container) =
        let sprites =
            let device = c.Get<GraphicsDevice>()
            let shaders = c.Get<ShaderSetCache>()
            let cache = c.Get<ResourceCache>()
            new SpriteRenderer(device, shaders, cache)
        c.Set<SpriteRenderer>(sprites)
        Disposable.Create [
            sprites :> IDisposable
            c.On<PushDrawCommands> <| fun _ ->
                let context = c.Get<RenderContext>()
                let cameras = c.Get<CameraSet>()
                sprites.Draw(context, cameras)
            ]

    [<Extension>]
    static member AddAudioDevice(c : Container) =
        let device = new AudioDevice()
        c.Set<AudioDevice>(device)
        Disposable.Create [
            device :> IDisposable
            ]
        
    [<Extension>]
    static member AddAudioLoaders(c : Container) =
        let device = c.Get<AudioDevice>()
        let cache = c.Get<ResourceCache>()
        cache.AddAudioLoaders(device)
        Disposable.Null

    [<Extension>]
    static member AddTickUpdate(c : Container) =
        let settings = c.GetOrSetDefault(TimingSettings.Default)
        let timer = FixedUpdateTimer(settings)
        Disposable.Create [
            c.On<Start> <| fun _ ->
                // Send initial request for tick
                let settings = c.Get<TimingSettings>()
                if settings.ClockActorId.IsDefined then
                    c.Send<Schedule>(settings.ClockActorId, {
                        DueTime = settings.MinDeltaTime
                        })
            c.On<Tick> <| fun e ->
                c.Start <| seq {
                    let settings = c.Get<TimingSettings>()
                    timer.SetSettings(settings)
                    timer.SetTime(e.Time)
                    let update = timer.TryTakeUpdate()
                    match update with
                    | ValueNone -> ()
                    | ValueSome e ->
                        // Handle input
                        yield c.Wait<PreUpdate> { Update = e }
                        yield c.Wait<HandleInput> { Time = e.Time } 
                    // Run fixed updates
                    while timer.HasFixedUpdate do
                        let e = timer.TakeFixedUpdate()
                        yield c.Wait<FixedUpdate>(e)
                    match update with
                    | ValueNone -> ()
                    | ValueSome e ->
                        // Run variable update
                        c.Step(e.DeltaTime)
                        yield c.Wait<Update>(e)
                        yield c.Wait<PostUpdate> { Update = e }
                    // Request next update
                    if settings.ClockActorId.IsDefined then
                        c.Send<Schedule>(settings.ClockActorId, {
                            DueTime = e.Time + settings.MinDeltaTime
                            })
                    }
            ]

    [<Extension>]
    static member AddGraphics(c : Container) =
        Disposable.Create [
            c.AddGraphicsDevice()
            c.AddGraphicsLoaders()
            c.AddWindowRendering()
            c.AddSpriteDrawing()
            c.AddInputPublishing()
            ]
        
    [<Extension>]
    static member AddDefaultSystems(c : Container) =
        Disposable.Create [
            c.AddAssetsFolder()
            c.AddTextLoaders()
            c.AddGraphics()
            c.AddAudioDevice()
            c.AddAudioLoaders()
            c.AddTickUpdate()
            ]
