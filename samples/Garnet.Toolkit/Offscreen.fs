namespace Garnet.Graphics

open System
open System.Collections.Generic
open System.Numerics
open Veldrid

type PositionTextureColorQuadMesh(device : GraphicsDevice) =
    let vb =
        let size = uint32 (sizeof<PositionTextureDualColorVertex> * 4)
        let b = device.ResourceFactory.CreateBuffer(BufferDescription(size, BufferUsage.VertexBuffer))
        device.UpdateBuffer(b, 0u, [| 
            // flip Y tex coords
            { Position = Vector3(-1.0f, -1.0f, 0.0f); TexCoord = Vector2(0.0f, 1.0f); Foreground = RgbaFloat.White; Background = RgbaFloat.Clear }
            { Position = Vector3(+1.0f, -1.0f, 0.0f); TexCoord = Vector2(1.0f, 1.0f); Foreground = RgbaFloat.White; Background = RgbaFloat.Clear }
            { Position = Vector3(+1.0f, +1.0f, 0.0f); TexCoord = Vector2(1.0f, 0.0f); Foreground = RgbaFloat.White; Background = RgbaFloat.Clear }
            { Position = Vector3(-1.0f, +1.0f, 0.0f); TexCoord = Vector2(0.0f, 0.0f); Foreground = RgbaFloat.White; Background = RgbaFloat.Clear }
            |])
        b
    let ib = 
        let b = device.ResourceFactory.CreateBuffer(BufferDescription(uint32 (sizeof<uint32> * 6), BufferUsage.IndexBuffer))
        device.UpdateBuffer(b, 0u, [| 0u; 1u; 2u; 0u; 2u; 3u |])
        b
    member c.Draw(cmds : CommandList) =
        cmds.SetVertexBuffer(0u, vb)
        cmds.SetIndexBuffer(ib, IndexFormat.UInt32)
        cmds.DrawIndexed(
            indexCount = 6u,
            instanceCount = 1u,
            indexStart = 0u,
            vertexOffset = 0,
            instanceStart = 0u)
    member c.Dispose() =
        vb.Dispose()
        ib.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()

type ColorDepthRenderTarget(device : GraphicsDevice, width, height) =
    let renderTarget =
        device.ResourceFactory.CreateTexture(
            TextureDescription(
                uint32 width,
                uint32 height,
                1u,
                1u,
                1u,
                PixelFormat.R16_G16_B16_A16_Float,
                TextureUsage.RenderTarget ||| TextureUsage.Sampled,
                TextureType.Texture2D))
    let depthTexture = 
        device.ResourceFactory.CreateTexture(
            TextureDescription.Texture2D(
                uint32 width,
                uint32 height,
                1u, 
                1u, 
                PixelFormat.R32_Float, 
                TextureUsage.DepthStencil))
    let frameBuffer =
        device.ResourceFactory.CreateFramebuffer(
            FramebufferDescription(depthTexture, renderTarget))
    member c.Texture = renderTarget
    member c.OutputDescription =
        frameBuffer.OutputDescription
    member c.SetFramebuffer(cmds : CommandList) =
        cmds.SetFramebuffer(frameBuffer)
    member c.Dispose() =
        renderTarget.Dispose()
        depthTexture.Dispose()
        frameBuffer.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()

type ColorRenderTarget(device : GraphicsDevice, width, height) =
    let renderTarget =
        device.ResourceFactory.CreateTexture(
            TextureDescription(
                uint32 width,
                uint32 height,
                1u,
                1u,
                1u,
                PixelFormat.R16_G16_B16_A16_Float,
                TextureUsage.RenderTarget ||| TextureUsage.Sampled,
                TextureType.Texture2D))
    let frameBuffer =
        device.ResourceFactory.CreateFramebuffer(
            FramebufferDescription(null, renderTarget))
    member c.Texture = renderTarget
    member c.OutputDescription =
        frameBuffer.OutputDescription
    member c.PushFramebuffer(context : RenderContext) =
        context.PushFramebuffer(frameBuffer)
    member c.Dispose() =
        renderTarget.Dispose()
        frameBuffer.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()

type ColorRenderTargetPipeline(device : GraphicsDevice, width, height, shaders, sampler, blend) =
    let target = new ColorRenderTarget(device, width, height)
    let pipelines = Dictionary<_, TextureTrianglePipeline>()
    member c.Width = width
    member c.Height = height
    member c.PushFramebuffer(context) =
        target.PushFramebuffer(context)
    member c.SetPipeline(cmds : CommandList, worldTransform, outputDesc) =
        let pipeline =
            match pipelines.TryGetValue(outputDesc) with
            | true, x -> x
            | false, _ ->
                let pipeline = new TextureTrianglePipeline(device, shaders, target.Texture, sampler, blend, outputDesc)
                pipelines.Add(outputDesc, pipeline)
                pipeline
        pipeline.SetPipeline(cmds)
        pipeline.SetProjectionView(Matrix4x4.Identity, Matrix4x4.Identity, cmds)
        pipeline.SetWorldTexture(worldTransform, Matrix4x4.Identity, cmds)        
    member c.Dispose() =
        target.Dispose()
        for pipeline in pipelines.Values do
            pipeline.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()

/// Offscreen render target along with a quad mesh so render target can be drawn in
/// another viewport. Does not take ownership of shaders or drawables.
type OffscreenQuadDrawable(device : GraphicsDevice, width, height, shaders, filtering, blend) =
    let sampler = device.GetSampler(filtering)
    let drawables = new DrawableCollection()
    let mesh = new PositionTextureColorQuadMesh(device)
    let mutable pipeline = new ColorRenderTargetPipeline(device, width, height, shaders, sampler, blend)
    member val Background = RgbaFloat.Black with get, set
    member val WorldTransform = Matrix4x4.Identity with get, set
    member c.Resize(width, height) =
        if pipeline.Width <> width || pipeline.Height <> height then
            pipeline.Dispose()
            pipeline <- new ColorRenderTargetPipeline(device, width, height, shaders, sampler, blend)
    member c.Add(drawable) =
        drawables.Add(drawable)
    member c.Draw(context) =
        // Set target before making draw calls for render target
        pipeline.PushFramebuffer(context)
        context.Commands.ClearColorTarget(0u, c.Background)
        // Draw onto render target
        drawables.Draw(context)
        // Draw render target itself as a quad
        context.PopFramebuffer()
        pipeline.SetPipeline(context.Commands, c.WorldTransform, context.OutputDescription)
        mesh.Draw(context.Commands)
    member c.Dispose() =
        pipeline.Dispose()
        mesh.Dispose()
    interface IDrawable with
        member c.Draw(context) = c.Draw(context)
    interface IDisposable with
        member c.Dispose() = c.Dispose()

