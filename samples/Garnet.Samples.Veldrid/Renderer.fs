namespace Garnet.Samples.Veldrid

open System
open System.Numerics
open System.Collections.Generic
open Veldrid
open Garnet.Samples.Resources

[<Struct>]
type UpdateState = {
    windowWidth : int
    windowHeight : int
    inputSnapshot : InputSnapshot
    deltaTime : float32
    }

type IRenderer =
    inherit IDisposable
    abstract Update : UpdateState -> unit
    abstract Draw : CommandList -> unit

type DeviceLayer(device, blendState, shaders : Resource<ShaderSet>, texture : Resource<Texture>, mesh : DeviceMesh) =
    let projView = new ProjectionViewSet(device, 0)
    let worldTexture = new WorldTextureSet(device, texture, 1)
    let layouts = [| 
        projView.Layout
        worldTexture.Layout 
        |]
    let pipeline =
        shaders.DeriveDisposable(fun shaders ->
            let blendState = 
                match blendState with
                | BlendState.Additive -> BlendStateDescription.SingleAdditiveBlend
                | BlendState.Alpha -> BlendStateDescription.SingleAlphaBlend
                | BlendState.Override -> BlendStateDescription.SingleOverrideBlend
                | _ -> BlendStateDescription.Empty
            let desc =
                GraphicsPipelineDescription(
                    BlendState = blendState,
                    DepthStencilState =
                        DepthStencilStateDescription(
                            depthTestEnabled = false,
                            depthWriteEnabled = true,
                            comparisonKind = ComparisonKind.LessEqual),
                    RasterizerState =
                        RasterizerStateDescription(
                            cullMode = FaceCullMode.None, //
                            fillMode = PolygonFillMode.Solid,
                            frontFace = FrontFace.Clockwise,
                            depthClipEnabled = true,
                            scissorTestEnabled = false),
                    PrimitiveTopology = PrimitiveTopology.TriangleList,
                    ResourceLayouts = layouts,
                    ShaderSet = shaders.Description,
                    Outputs = device.SwapchainFramebuffer.OutputDescription)
            device.ResourceFactory.CreateGraphicsPipeline(desc))
    member c.Mesh = mesh
    member c.Dispose() =
        mesh.Dispose()
        pipeline.Dispose()
        projView.Dispose()
        worldTexture.Dispose()
    member c.Draw(context : LayerTransforms, cmds : CommandList) =
        // setup pipeline
        cmds.SetPipeline(pipeline.Load())
        // write uniform buffers
        projView.Apply(context.projection, context.view, cmds)
        worldTexture.Apply(context.world, cmds)
        // draw mesh
        mesh.Draw cmds
    member c.Invalidate() =
        mesh.Invalidate()
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()

type DeviceCanvas(device, resources : ResourceSet) =
    let layers = Dictionary<int, DeviceLayer>()
    /// Get or add a layer
    member c.GetLayer(layer : ILayer) =
        let desc = layer.Descriptor
        match layers.TryGetValue desc.layerId with
        | true, x -> x
        | false, _ ->
            // create resources
            let layout = ToVeldrid.getVertexLayout desc.vertexElements
            let shaders = 
                Resource.DeriveDisposableFrom(
                    resources.GetResource(desc.vertexShader),
                    resources.GetResource(desc.fragmentShader),
                    fun vert frag -> new ShaderSet(device, vert, frag, layout))
            let texture = resources.GetResource<Texture>(desc.texture)
            let mesh = new DeviceMesh(device, desc.vertexSize, layer.Vertices, layer.Indices)
            // create layer
            let deviceLayer = new DeviceLayer(device, desc.blendState, shaders, texture, mesh)
            layers.Add(desc.layerId, deviceLayer)
            deviceLayer
    member c.Invalidate() =
        for layer in layers.Values do
            layer.Invalidate()
    member c.Dispose() =
        for layer in layers.Values do
            layer.Dispose()

type Renderer(device, resources, canvas : Canvas) =
    let layers = DeviceCanvas(device, resources)
    let updates = List<_>()
    member c.OnUpdate action =
        updates.Add action
        new Subscription<_>(updates, action) :> IDisposable
    member c.Clear() =
        canvas.Clear()
        layers.Invalidate()
    interface IRenderer with
        member c.Update(state) =
            // invalidate any changed resources
            resources.InvalidateChanged()
            // update any subscribers
            for action in updates do
                action state
        member c.Draw(cmds) =
            for viewport in canvas.Viewports do
                // calculate transforms
                let transforms = {
                    world = Matrix4x4.Identity
                    projection = viewport.Projection
                    view = viewport.View
                    }
                // We want to render directly to the output window.
                cmds.SetFramebuffer(device.SwapchainFramebuffer)
                cmds.ClearColorTarget(0u, ToVeldrid.getRgbaFloat viewport.ClearColor)
                // draw layers
                for layer in viewport.Layers do
                    let deviceLayer =  layers.GetLayer layer
                    deviceLayer.Draw(transforms, cmds)
        member c.Dispose() =
            layers.Dispose()
            resources.Dispose()
