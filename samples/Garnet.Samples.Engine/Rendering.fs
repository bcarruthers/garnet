namespace Garnet.Samples.Engine

open System
open System.Collections.Generic
open Veldrid

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
   
type IDrawable =
    inherit IDisposable
    abstract Draw : RenderContext -> unit

type Drawable(draw) =
    interface IDrawable with
        member c.Draw(context) = draw context
        member c.Dispose() = ()

/// Adding drawables passes ownership and responsibility for disposal
type DrawableCollection() =
    let drawables = List<IDrawable>()
    member c.Add(drawable) =
        drawables.Add(drawable)
    member c.Draw(context) =
        for drawable in drawables do
            drawable.Draw(context)
    member c.Dispose() =
        for drawable in drawables do
            drawable.Dispose()
    interface IDrawable with
        member c.Draw(context) = c.Draw(context)
        member c.Dispose() = c.Dispose()
        
type Renderer(device : GraphicsDevice) =
    let drawables = new DrawableCollection()
    let cmds = device.ResourceFactory.CreateCommandList()
    let context = RenderContext(cmds)
    let mutable size = Vector2i.Zero
    let mutable valid = false
    member c.Invalidate() =
        valid <- false
    member c.Add(drawable) =
        drawables.Add(drawable)
    member c.Draw(newSize : Vector2i, bgColor) =
        if newSize <> size then
            device.ResizeMainWindow(uint32 newSize.X, uint32 newSize.Y)
            size <- newSize
            valid <- false
        // Require manual invalidation instead of constantly drawing
        if not valid then
            valid <- true
            cmds.Begin()        
            // We want to render directly to the output window
            context.PushFramebuffer(device.SwapchainFramebuffer)
            // Clear viewports
            cmds.ClearColorTarget(0u, bgColor)
            // Call drawing
            drawables.Draw(context)
            // End() must be called before commands can be submitted for execution.
            cmds.End()
            device.SubmitCommands(cmds)
            // Once commands have been submitted, the rendered image can be presented to 
            // the application window.
            device.SwapBuffers()
            context.PopFramebuffer()
    member c.Dispose() =
        drawables.Dispose()
        cmds.Dispose()
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()
