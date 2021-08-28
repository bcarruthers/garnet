namespace Garnet.Graphics

open System
open System.Collections.Generic
open System.Numerics
open Veldrid
open Garnet.Composition

[<Struct>]
type Blend =
    | Additive = 0
    | Alpha = 1
    | Override = 2

[<Struct>]
type Filtering =
    | Point = 0
    | Linear = 1

[<Struct>]
type TexturePipelineDescriptor = {
    Blend : Blend
    Filtering : Filtering
    ShaderSet : ShaderSetDescriptor
    Texture : string
    }

[<Struct>]
type TexturePipelineDescriptor<'v
                        when 'v : struct 
                        and 'v : (new : unit -> 'v) 
                        and 'v :> ValueType
                        and 'v :> IVertex> = {
    Blend : Blend
    Filtering : Filtering
    ShaderSet : ShaderSetDescriptor<'v>
    Texture : string
    } with
    member c.Untyped : TexturePipelineDescriptor = {
        Blend = c.Blend
        Filtering = c.Filtering
        ShaderSet = c.ShaderSet.Untyped
        Texture = c.Texture
        }

[<AutoOpen>]
module internal GraphicsDeviceExtensions =
    type GraphicsDevice with
        member c.GetSampler(filtering) =
            match filtering with
            | Filtering.Point -> c.PointSampler
            | Filtering.Linear -> c.LinearSampler
            | x -> failwith $"Invalid filtering {x}"

type ProjectionViewSet(device : GraphicsDevice, slot) =
    let factory = device.ResourceFactory
    let projBuffer =
        device.ResourceFactory.CreateBuffer(
            BufferDescription(uint32 sizeof<Matrix4x4>, BufferUsage.UniformBuffer))
    let viewBuffer =
        device.ResourceFactory.CreateBuffer(
            BufferDescription(uint32 sizeof<Matrix4x4>, BufferUsage.UniformBuffer))
    let projViewLayout = 
        factory.CreateResourceLayout(
            ResourceLayoutDescription(
                ResourceLayoutElementDescription(
                    "ProjectionBuffer", 
                    ResourceKind.UniformBuffer, 
                    ShaderStages.Vertex),
                ResourceLayoutElementDescription(
                    "ViewBuffer", 
                    ResourceKind.UniformBuffer, 
                    ShaderStages.Vertex)))
    let projViewSet = 
        factory.CreateResourceSet(
            ResourceSetDescription(projViewLayout, projBuffer, viewBuffer))
    member c.Layout = projViewLayout
    member c.Apply(proj : Matrix4x4, view : Matrix4x4, cmds : CommandList) =
        cmds.UpdateBuffer(projBuffer, 0u, proj)
        cmds.UpdateBuffer(viewBuffer, 0u, view)
        cmds.SetGraphicsResourceSet(uint32 slot, projViewSet)
    member c.Dispose() =
        projViewSet.Dispose()
        projViewLayout.Dispose()
        projBuffer.Dispose()
        viewBuffer.Dispose()
    interface IDisposable with 
        member c.Dispose() = c.Dispose()
   
type WorldTextureSet(device : GraphicsDevice, surfaceTexture : Texture, slot, sampler) =
    let factory = device.ResourceFactory
    let worldBuffer = factory.CreateBuffer(BufferDescription(64u, BufferUsage.UniformBuffer))
    let texTransformBuffer = factory.CreateBuffer(BufferDescription(64u, BufferUsage.UniformBuffer))
    let worldTextureLayout = 
        factory.CreateResourceLayout(
            ResourceLayoutDescription(
                ResourceLayoutElementDescription(
                    "WorldBuffer", 
                    ResourceKind.UniformBuffer, 
                    ShaderStages.Vertex),
                ResourceLayoutElementDescription(
                    "TexTransformBuffer", 
                    ResourceKind.UniformBuffer, 
                    ShaderStages.Vertex),
                ResourceLayoutElementDescription(
                    "SurfaceTexture", 
                    ResourceKind.TextureReadOnly, 
                    ShaderStages.Fragment),
                ResourceLayoutElementDescription(
                    "SurfaceSampler", 
                    ResourceKind.Sampler, 
                    ShaderStages.Fragment)))
    let surfaceTextureView = factory.CreateTextureView(surfaceTexture)
    let worldTextureSet =
        factory.CreateResourceSet(
            ResourceSetDescription(
                worldTextureLayout,
                worldBuffer,
                texTransformBuffer,
                surfaceTextureView,
                sampler))
    member c.Layout = worldTextureLayout
    member c.Apply(world : Matrix4x4, texTransform : Matrix4x4, cmds : CommandList) =
        cmds.UpdateBuffer(worldBuffer, 0u, world)
        cmds.UpdateBuffer(texTransformBuffer, 0u, texTransform)
        cmds.SetGraphicsResourceSet(uint32 slot, worldTextureSet)
    member c.Dispose() =
        worldTextureSet.Dispose()
        worldTextureLayout.Dispose()
        texTransformBuffer.Dispose()
        worldBuffer.Dispose()
        surfaceTextureView.Dispose()
    interface IDisposable with 
        member c.Dispose() = c.Dispose()

type TextureTrianglePipeline(device, shaders : ShaderSet, texture : Texture, sampler, blendState, outputDesc) =
    let projView = new ProjectionViewSet(device, 0)
    let worldTexture = new WorldTextureSet(device, texture, 1, sampler)
    let layouts = [| 
        projView.Layout
        worldTexture.Layout 
        |]
    let pipeline =
        let desc =
            GraphicsPipelineDescription(
                BlendState = blendState,
                DepthStencilState = DepthStencilStateDescription.Disabled,
                RasterizerState =
                    RasterizerStateDescription(
                        cullMode = FaceCullMode.None,
                        fillMode = PolygonFillMode.Solid,
                        frontFace = FrontFace.Clockwise,
                        depthClipEnabled = false,
                        scissorTestEnabled = false),
                PrimitiveTopology = PrimitiveTopology.TriangleList,
                ResourceLayouts = layouts,
                ShaderSet = shaders.Description,
                Outputs = outputDesc)
        device.ResourceFactory.CreateGraphicsPipeline(desc)
    member c.Dispose() =
        pipeline.Dispose()
        projView.Dispose()
        worldTexture.Dispose()
    member c.SetPipeline(cmds : CommandList) =
        cmds.SetPipeline(pipeline)
    member c.SetProjectionView(projection, view, cmds) =
        projView.Apply(projection, view, cmds)
    member c.SetWorldTexture(world, texTransform, cmds) =
        worldTexture.Apply(world, texTransform, cmds)
    interface IDisposable with
        member c.Dispose() = c.Dispose()

type TexturePipelineCache(device : GraphicsDevice,
        shaderCache : ShaderSetCache,
        cache : IResourceCache) =
    let solidTexture = device.CreateTextureRgba(1, 1, ReadOnlyMemory(Array.create 4 255uy))
    let pipelines = Dictionary<_, TextureTrianglePipeline>()
    member c.GetPipeline(desc : TexturePipelineDescriptor, outputDesc) =
        let key = struct(desc, outputDesc)
        match pipelines.TryGetValue(key) with
        | true, pipeline -> pipeline
        | false, _ ->
            let sampler = device.GetSampler(desc.Filtering)
            let blend =
                match desc.Blend with
                | Blend.Additive -> BlendStateDescription.SingleAdditiveBlend
                | Blend.Alpha -> BlendStateDescription.SingleAlphaBlend
                | Blend.Override -> BlendStateDescription.SingleOverrideBlend
                | x -> failwith $"Invalid blend {x}"
            let shaders = shaderCache.GetOrCreate(device, desc.ShaderSet, cache)
            let texture =
                // Use a solid white texture as a fallback when none specified
                if String.IsNullOrEmpty(desc.Texture) then solidTexture
                else cache.LoadResource<Texture>(desc.Texture)
            let pipeline = new TextureTrianglePipeline(device, shaders, texture, sampler, blend, outputDesc)
            pipelines.Add(key, pipeline)
            pipeline
    member c.Dispose() =
        solidTexture.Dispose()
        for pipeline in pipelines.Values do
            pipeline.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()
