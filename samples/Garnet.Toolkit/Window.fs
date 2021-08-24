﻿namespace Garnet.Graphics

open System
open System.IO
open System.Runtime.InteropServices
open Veldrid
open Veldrid.StartupUtilities
open ImGuiNET
open Garnet.Numerics
open Garnet.Input

module private Environment =
    let addPathVariables() =
        // Set path variable so native DLLs can be found when running in FSI
        let basePath = Path.GetDirectoryName(Directory.GetCurrentDirectory())
        let nativePath = Path.Combine(basePath, @"runtimes\win-x64\native")
        let ev = Environment.GetEnvironmentVariable("Path")
        if not (ev.Contains(nativePath)) then
            Environment.SetEnvironmentVariable("Path", $"{ev};{nativePath}")

type WindowRenderer([<Optional; DefaultParameterValue("Garnet")>] title : string,
        [<Optional; DefaultParameterValue(100)>] x : int,
        [<Optional; DefaultParameterValue(100)>] y : int,
        [<Optional; DefaultParameterValue(640)>] width : int,
        [<Optional; DefaultParameterValue(360)>] height : int,
        [<Optional; DefaultParameterValue(Redraw.Auto)>] redraw : Redraw
        ) =
    let window = 
        Environment.addPathVariables()
        // Create window initially hidden until background can be drawn
        let windowCI = 
            WindowCreateInfo(
                X = x,
                Y = y,
                WindowWidth = width,
                WindowHeight = height,
                WindowInitialState = WindowState.Hidden,
                WindowTitle = title
            )
        VeldridStartup.CreateWindow(windowCI)
    let device = 
        let options = GraphicsDeviceOptions(false, Nullable<PixelFormat>(), false)
        VeldridStartup.CreateGraphicsDevice(window, options)
    let imGui =
        new ImGuiRenderer(device,
            device.MainSwapchain.Framebuffer.OutputDescription,
            window.Width, window.Height)
    let renderer = new Renderer(device, redraw)
    let mutable isDrawn = false
    member c.Title 
        with get() = window.Title
        and set value = window.Title <- value
    member c.Position 
        with get() = Vector2i(window.X, window.Y) 
        and set(value : Vector2i) =
            window.X <- value.X
            window.Y <- value.Y
    member c.Size 
        with get() = Vector2i(window.Width, window.Height) 
        and set(value : Vector2i) =
            window.Width <- value.X
            window.Height <- value.Y
    member val Background = RgbaFloat.Black with get, set
    member c.ImGui = imGui
    member c.Device = device
    member c.RenderContext = renderer.Context
    member c.IsShown =
        match window.WindowState with
        | WindowState.Hidden
        | WindowState.Minimized -> false
        | _ -> window.Visible
    member c.Invalidate() =
        renderer.Invalidate()
    member c.Close() =
        window.Close()
    member c.ToggleFullScreen() =
        window.WindowState <- 
            match window.WindowState with
            | WindowState.BorderlessFullScreen -> WindowState.Normal
            | _ -> WindowState.BorderlessFullScreen
    member c.BeginDraw() =
        // Avoid drawing when we've drawn at least once but window isn't visible
        if isDrawn && not c.IsShown then false
        else
            // Proceed with drawing
            imGui.WindowResized(window.Width, window.Height)
            renderer.BeginDraw(window.Width, window.Height, c.Background)
    member c.EndDraw() =
        // Need to call this after beginning drawing but before ending. If we call
        // before any drawing, then there will be a brief white flicker as the window
        // is shown. If we call after all drawing, the initial render will not be
        // presented to the window, which is visible if doing manual redraw.
        window.Visible <- true
        isDrawn <- true
        // Complete rendering
        imGui.Render(device, renderer.Context.Commands)
        renderer.EndDraw()
    member c.Update(deltaSeconds, inputs : InputCollection) =
        let snapshot = window.PumpEvents()
        if not window.Exists then false
        else
            imGui.Update(deltaSeconds, snapshot)
            inputs.ClearEvents()
            if not (ImGui.GetIO().WantCaptureKeyboard) then
                inputs.UpdateKeysFromSnapshot(snapshot)
            if not (ImGui.GetIO().WantCaptureMouse) then
                inputs.UpdateMouseFromSnapshot(snapshot, c.Size)                                
            true
    member c.Dispose() =
        renderer.Dispose()
        imGui.Dispose()
        device.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()
