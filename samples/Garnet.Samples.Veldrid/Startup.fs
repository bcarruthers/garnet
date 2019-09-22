namespace Garnet.Samples.Veldrid

open System
open System.Numerics
open Veldrid
open Veldrid.Sdl2
open Veldrid.StartupUtilities
open Garnet.Composition
open Garnet.Samples.Resources
open Garnet.Samples.FrameworkTypes

type WindowController(device, window : Sdl2Window, root : IRenderer) =
    let controller = 
        new ImGuiRenderer(device, 
            device.MainSwapchain.Framebuffer.OutputDescription,
            window.Width, window.Height)
    let cmds = device.ResourceFactory.CreateCommandList()
    let onResize = Action(fun () -> 
        controller.WindowResized(window.Width, window.Height)
        device.MainSwapchain.Resize(uint32 window.Width, uint32 window.Height))
    do window.add_Resized onResize
    member c.Update(interval, snapshot) =
        // Feed the input events to our ImGui controller.
        controller.Update(interval, snapshot)
        // draw GUI after input but before rendering
        root.Update { 
            deltaTime = interval
            inputSnapshot = snapshot
            windowWidth = window.Width
            windowHeight = window.Height
            }
        // Begin() must be called before commands can be issued.
        cmds.Begin()        
        root.Draw(cmds)
        // Draw GUI (assume GUI draw calls all submitted before this point)
        controller.Render(device, cmds)
        // End() must be called before commands can be submitted for execution.
        cmds.End()
        device.SubmitCommands(cmds)
        // Once commands have been submitted, the rendered image can be presented to 
        // the application window.
        device.SwapBuffers()
    interface IDisposable with 
        member c.Dispose() =
            device.WaitForIdle()
            window.remove_Resized onResize
            controller.Dispose()
            cmds.Dispose()

module Window =
    let run title (assetPath : string) (c : Container) =
        // Register event handlers
        VeldridSystems.definition 
        |> Registration.registerTo c 
        |> ignore
        // Create device resources.
        let windowCI = 
            WindowCreateInfo(
                X = 100,
                Y = 100,
                WindowWidth = 960,
                WindowHeight = 540,
                WindowTitle = title
            )
        let window = VeldridStartup.CreateWindow(windowCI)
        window.LimitPollRate <- true
        use device = VeldridStartup.CreateGraphicsDevice(window)
        // Connect file system source to shared resources
        use fileSource = new FileStreamSource(assetPath)
        let resources = c.GetInstance<ResourceSet>()
        resources.RegisterGraphics(device)
        resources.AddSource fileSource
        // Create renderer pointing to shared canvas
        let canvas = c.GetInstance<Canvas>()
        use ren = new Renderer(device, resources, canvas)
        // Publish events at start and on updates.
        c.Run <| Reset()
        let sub = 
            ren.OnUpdate <| fun e ->
                canvas.Clear()
                c.Run<Resize> { viewWidth = e.windowWidth; viewHeight = e.windowHeight }
                c.Run<Update> { deltaTime = e.deltaTime }
                c.Run <| Draw()
        // Run game loop.
        use controller = new WindowController(device, window, ren)
        while window.Exists do
            let snapshot = window.PumpEvents()
            if window.Exists then 
                controller.Update(1.0f / 60.0f, snapshot)
