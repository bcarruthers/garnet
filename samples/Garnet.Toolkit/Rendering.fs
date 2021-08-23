namespace Garnet.Graphics

open System
open System.Collections.Generic
open Veldrid

// Allow auto invalidate to avoid requiring extra call when every frame
// is considered invalidated (for uses of manual mode, consider simple
// turn-based games without animation or tools that don't need to be
// constantly drawing.
type Redraw =
    | Manual = 0
    | Auto = 1

type RenderContext(commands : CommandList) =
    let frameBuffers = Stack<Framebuffer>()
    member c.Commands = commands
    member c.OutputDescription =
        frameBuffers.Peek().OutputDescription
    member c.PushFramebuffer(framebuffer) =
        frameBuffers.Push(framebuffer)
        commands.SetFramebuffer(framebuffer)
    member c.PopFramebuffer() =
        frameBuffers.Pop() |> ignore
        if frameBuffers.Count > 0 then
            commands.SetFramebuffer(frameBuffers.Peek())
        
type Renderer(device : GraphicsDevice, redraw) =
    let commands = device.ResourceFactory.CreateCommandList()
    let context = RenderContext(commands)
    let mutable width = 0
    let mutable height = 0
    let mutable valid = false
    member c.Context = context
    member c.Invalidate() =
        valid <- false
    member c.BeginDraw(newWidth : int, newHeight : int, bgColor) =
        let resized = newWidth <> width || newHeight <> height 
        if resized then
            device.ResizeMainWindow(uint32 newWidth, uint32 newHeight)
            width <- newWidth
            height <- newHeight
        // Only proceed if something to redraw
        if valid && not resized then false
        else
            commands.Begin()        
            // We want to render directly to the output window
            context.PushFramebuffer(device.SwapchainFramebuffer)
            // Clear viewports
            commands.ClearColorTarget(0u, bgColor)
            // Mark valid if using manual redraw
            match redraw with
            | Redraw.Auto -> ()
            | Redraw.Manual | _ -> valid <- true           
            true
    member c.EndDraw() =
        // End() must be called before commands can be submitted for execution.
        commands.End()
        device.SubmitCommands(commands)
        // Once commands have been submitted, the rendered image can be presented to 
        // the application window.
        device.SwapBuffers()
        context.PopFramebuffer()
    member c.Dispose() =
        device.WaitForIdle()
        commands.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()
