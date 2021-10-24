namespace Garnet.Graphics

open System
open System.Collections.Generic
open System.Numerics
open Veldrid

type PositionTextureColorQuadMesh(device : GraphicsDevice) =
    let vb =
        let size = uint32 (sizeof<PositionTextureColorVertex> * 4)
        let b = device.ResourceFactory.CreateBuffer(BufferDescription(size, BufferUsage.VertexBuffer))
        device.UpdateBuffer(b, 0u, [| 
            // flip Y tex coords
            { Position = Vector3(-1.0f, -1.0f, 0.0f); TexCoord = Vector2(0.0f, 1.0f); Color = RgbaFloat.White }
            { Position = Vector3(+1.0f, -1.0f, 0.0f); TexCoord = Vector2(1.0f, 1.0f); Color = RgbaFloat.White }
            { Position = Vector3(+1.0f, +1.0f, 0.0f); TexCoord = Vector2(1.0f, 0.0f); Color = RgbaFloat.White }
            { Position = Vector3(-1.0f, +1.0f, 0.0f); TexCoord = Vector2(0.0f, 0.0f); Color = RgbaFloat.White }
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

type ColorRenderTargetBuffer(device : GraphicsDevice, width, height) =
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
    let target = new ColorRenderTargetBuffer(device, width, height)
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
type RenderTarget(device : GraphicsDevice, shaders, filtering, blend) =
    let sampler = device.GetSampler(filtering)
    let mesh = new PositionTextureColorQuadMesh(device)
    let mutable pipelineOpt = ValueNone
    member val Background = RgbaFloat.Black with get, set
    member val WorldTransform = Matrix4x4.Identity with get, set
    member val Width = 0 with get, set
    member val Height = 0 with get, set
    member private c.CreatePipeline() =
        let pipeline = new ColorRenderTargetPipeline(device, c.Width, c.Height, shaders, sampler, blend)
        pipelineOpt <- ValueSome pipeline
        pipeline
    member c.BeginDraw(context) =
        let pipeline =
            match pipelineOpt with
            | ValueNone -> c.CreatePipeline()
            | ValueSome pipeline ->
                if pipeline.Width = c.Width && pipeline.Height = c.Height then pipeline
                else 
                    pipeline.Dispose()
                    c.CreatePipeline()
        // Set target before making draw calls for render target
        pipeline.PushFramebuffer(context)
        context.Commands.ClearColorTarget(0u, c.Background)
    member c.EndDraw(context : RenderContext) =
        match pipelineOpt with
        | ValueNone -> () 
        | ValueSome pipeline ->
            // Draw render target itself as a quad
            context.PopFramebuffer()
            pipeline.SetPipeline(context.Commands, c.WorldTransform, context.OutputDescription)
            mesh.Draw(context.Commands)
    member c.Dispose() =
        match pipelineOpt with
        | ValueNone -> ()
        | ValueSome pipeline -> pipeline.Dispose()
        mesh.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()
