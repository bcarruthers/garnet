namespace Garnet.Graphics

open System
open System.IO
open Veldrid
open Veldrid.StartupUtilities
open Garnet.Numerics

type WindowDescriptor = {
    WindowX : int
    WindowY : int
    WindowWidth : int
    WindowHeight : int
    Title : string
    }

type WindowRenderer(param) =
    let addPathVariables() =
        // set path variable so native DLLs can be found when running in FSI
        let basePath = Path.GetDirectoryName(Directory.GetCurrentDirectory())
        let nativePath = Path.Combine(basePath, @"runtimes\win-x64\native")
        let ev = Environment.GetEnvironmentVariable("Path")
        if not (ev.Contains(nativePath)) then
            Environment.SetEnvironmentVariable("Path", $"{ev};{nativePath}")
    let window = 
        addPathVariables()
        // Create window initially hidden until background can be drawn
        let windowCI = 
            WindowCreateInfo(
                X = param.WindowX,
                Y = param.WindowY,
                WindowWidth = param.WindowWidth,
                WindowHeight = param.WindowHeight,
                WindowInitialState = WindowState.Hidden,
                WindowTitle = param.Title
            )
        VeldridStartup.CreateWindow(windowCI)
    let device = 
        let options = GraphicsDeviceOptions(false, Nullable<PixelFormat>(), false)
        VeldridStartup.CreateGraphicsDevice(window, options)
    let imGui =
        new ImGuiRenderer(device,
            device.MainSwapchain.Framebuffer.OutputDescription,
            window.Width, window.Height)
    let drawables = new DrawableCollection()
    let renderer = new Renderer(device)
    let inputs = InputCollection()
    do
        // Add ImGui after all other drawables
        renderer.Add(drawables)
        renderer.Add(new Drawable(fun context -> imGui.Render(device, context.Commands)))
    member val Background = RgbaFloat.Black with get, set
    member c.ImGui = imGui
    member c.Device = device
    member c.Inputs = inputs
    member c.WindowSize = Vector2i(window.Width, window.Height)
    member c.IsShown =
        match window.WindowState with
        | WindowState.Hidden
        | WindowState.Minimized -> false
        | _ -> window.Visible
    member c.Invalidate() =
        renderer.Invalidate()
    member c.Add(drawable) =
        drawables.Add(drawable)
    member c.Close() =
        window.Close()
    member c.ToggleFullScreen() =
        window.WindowState <- 
            match window.WindowState with
            | WindowState.BorderlessFullScreen -> WindowState.Normal
            | _ -> WindowState.BorderlessFullScreen
    member c.Draw() =
        window.Visible <- true
        if c.IsShown then
            imGui.WindowResized(window.Width, window.Height)
            renderer.Draw(c.WindowSize, c.Background)
    member c.Update(deltaSeconds) =
        let snapshot = window.PumpEvents()
        if not window.Exists then false
        else
            imGui.Update(deltaSeconds, snapshot)
            inputs.ClearEvents()
            inputs.UpdateKeysFromSnapshot(snapshot)
            inputs.UpdateMouseFromSnapshot(snapshot, c.WindowSize)                                
            true
    member c.Dispose() =
        renderer.Dispose()
        imGui.Dispose()
        device.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()
