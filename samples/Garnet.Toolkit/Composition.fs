namespace Garnet.Composition

open System
open System.Diagnostics
open System.Threading
open System.Runtime.InteropServices
open Veldrid
open Garnet.Input
open Garnet.Graphics
open Garnet.Audio

[<AutoOpen>]
module DefaultSystem =
    type Container with
        member c.AddAssetsFolder() =
            let folder = new FileFolder("assets")
            let cache = c.GetValue<ResourceCache>()
            cache.SetFolder(folder)
            Disposable.Create [
                folder :> IDisposable
                ]
            
        member c.AddTextLoaders() =
            let cache = c.GetValue<ResourceCache>()
            cache.AddTextLoaders()
            Disposable.Null

        member c.AddGraphicsDevice() =
            let ren = new WindowRenderer()
            let shaders = new ShaderSetCache()
            c.SetValue<WindowRenderer>(ren)
            c.SetValue<GraphicsDevice>(ren.Device)
            c.SetValue<RenderContext>(ren.RenderContext)
            c.SetValue<ShaderSetCache>(shaders)
            Disposable.Create [
                ren :> IDisposable
                shaders :> IDisposable
                ]
            
        member c.AddGraphicsLoaders() =
            let device = c.GetValue<GraphicsDevice>()
            let cache = c.GetValue<ResourceCache>()
            cache.AddShaderLoaders(device)
            cache.AddTextureLoaders(device)
            cache.AddFontLoaders()
            Disposable.Null

        member c.AddWindowRendering() =
            Disposable.Create [
                c.On<PreUpdate> <| fun _ ->
                    let ren = c.GetValue<WindowRenderer>()
                    let inputs = c.GetValue<InputCollection>()
                    let isRunning = ren.Update(0.0f, inputs)
                    if not isRunning then
                        c.Send <| Close()
                c.On<PostUpdate> <| fun e ->
                    let ren = c.GetValue<WindowRenderer>()
                    c.Start <| seq {
                        let draw = {
                            Update = e.Update
                            ViewSize = ren.Size
                            }
                        yield c.Wait<Draw>(draw) 
                        if ren.BeginDraw() then
                            yield c.Wait <| PushDrawCommands()
                            ren.EndDraw()
                        }
                ]
            
        member c.AddInputPublishing() =
            Disposable.Create [
                c.On<HandleInput> <| fun _ ->
                    let inputs = c.GetValue<InputCollection>()
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
            
        member c.AddSpriteDrawing() =
            let sprites =
                let device = c.GetValue<GraphicsDevice>()
                let shaders = c.GetValue<ShaderSetCache>()
                let cache = c.GetValue<ResourceCache>()
                new SpriteRenderer(device, shaders, cache)
            c.SetValue<SpriteRenderer>(sprites)
            Disposable.Create [
                sprites :> IDisposable
                c.On<PushDrawCommands> <| fun _ ->
                    let context = c.GetValue<RenderContext>()
                    let cameras = c.GetValue<CameraSet>()
                    sprites.Draw(context, cameras)
                ]
            
        member c.AddAudioDevice() =
            let device = new AudioDevice()
            c.SetValue<AudioDevice>(device)
            Disposable.Create [
                device :> IDisposable
                ]
            
        member c.AddAudioLoaders() =
            let device = c.GetValue<AudioDevice>()
            let cache = c.GetValue<ResourceCache>()
            cache.AddAudioLoaders(device)
            Disposable.Null

        member c.AddTickUpdate() =
            let timer = c.GetValue<FixedUpdateTimer>()
            Disposable.Create [
                c.On<Tick> <| fun e ->
                    c.Start <| seq {
                        timer.SetTime(e.Time)
                        // Handle input
                        let e = timer.TakeUpdate()
                        yield c.Wait<PreUpdate> { Update = e }
                        yield c.Wait<HandleInput> { Time = e.Time } 
                        // Run fixed updates
                        while timer.HasFixedUpdate do
                            let e = timer.TakeFixedUpdate()
                            yield c.Wait<FixedUpdate>(e)
                        // Run variable update
                        c.Step(e.DeltaTime)
                        yield c.Wait<Update>(e)
                        yield c.Wait<PostUpdate> { Update = e }
                        }
                ]
            
        member c.AddDefaultSystems() =
            Disposable.Create [
                c.AddAssetsFolder()
                c.AddTextLoaders()
                c.AddGraphicsDevice()
                c.AddGraphicsLoaders()
                c.AddWindowRendering()
                c.AddSpriteDrawing()
                c.AddInputPublishing()
                c.AddAudioDevice()
                c.AddAudioLoaders()
                c.AddTickUpdate()
                ]
            
        member c.RunLoop([<Optional; DefaultParameterValue(-1)>] sleepDuration) =
            let mutable running = true
            use sub =
                c.On<Close> <| fun _ ->
                    running <- false
                    c.Send <| Closing()
            c.Run(Start())
            let sw = Stopwatch.StartNew()
            while running do
                c.Run<Tick> { Time = sw.ElapsedMilliseconds }
                // Sleep to avoid spinning CPU
                if sleepDuration >= 0 then
                    Thread.Sleep(sleepDuration)
