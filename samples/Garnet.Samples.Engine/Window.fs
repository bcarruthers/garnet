namespace Garnet.Samples.Engine

open System
open System.IO
open Veldrid
open Veldrid.StartupUtilities

type CreateWindow = {
    windowX : int
    windowY : int
    windowWidth : int
    windowHeight : int
    title : string
    assetPath : string
    backgroundColor : RgbaFloat
    }

module CreateWindow =
    let defaults = {
        windowX = 100
        windowY = 100
        windowWidth = 960
        windowHeight = 540
        title = ""
        assetPath = ""
        backgroundColor = RgbaFloat.Black
        }

type WindowRenderer(param) =
    let addPathVariables() =
        // set path variable so native DLLs can be found when running in FSI
        let basePath = Path.GetDirectoryName(Directory.GetCurrentDirectory())
        let nativePath = Path.Combine(basePath, @"runtimes\win-x64\native")
        let ev = Environment.GetEnvironmentVariable("Path")
        if not (ev.Contains(nativePath)) then
            Environment.SetEnvironmentVariable("Path", $"{ev};{nativePath}")
            //printfn "Setting path to %s" nativePath
    let window = 
        addPathVariables()
        // Create window initially hidden until background can be drawn
        let windowCI = 
            WindowCreateInfo(
                X = param.windowX,
                Y = param.windowY,
                WindowWidth = param.windowWidth,
                WindowHeight = param.windowHeight,
                WindowInitialState = WindowState.Hidden,
                WindowTitle = param.title
            )
        VeldridStartup.CreateWindow(windowCI)
    let device = 
        let options = GraphicsDeviceOptions(false, Nullable<PixelFormat>(), false)
        VeldridStartup.CreateGraphicsDevice(window, options)
    let renderer = new Renderer(device)
    let inputs = InputCollection()
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
        renderer.Add(drawable)
    member c.Close() =
        window.Close()
    member c.ToggleFullScreen() =
        window.WindowState <- 
            match window.WindowState with
            | WindowState.BorderlessFullScreen -> WindowState.Normal
            | _ -> WindowState.BorderlessFullScreen
    member c.Draw(bgColor) =
        window.Visible <- true
        if c.IsShown then
            renderer.Draw(c.WindowSize, bgColor)
    member c.Update() =
        let snapshot = window.PumpEvents()
        if not window.Exists then false
        else
            inputs.ClearEvents()
            inputs.UpdateKeysFromSnapshot(snapshot)
            inputs.UpdateMouseFromSnapshot(snapshot, c.WindowSize)                                
            true
    member c.Dispose() =
        renderer.Dispose()
        device.Dispose()
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()




