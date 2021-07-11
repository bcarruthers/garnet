namespace Garnet.Samples.Engine

open System
open System.Buffers
open System.Collections.Generic
open System.Numerics
open System.Text
open Veldrid

type Conversions() =
    static member ToRgbaByte(x : uint32) =
        RgbaByte(
            byte ((x >>> 24) &&& (uint32 0xff)),
            byte ((x >>> 16) &&& (uint32 0xff)),
            byte ((x >>> 8) &&& (uint32 0xff)),
            byte ((x >>> 0) &&& (uint32 0xff)))

    static member ToRgbaFloat(c : RgbaByte) =
        Veldrid.RgbaFloat(
            float32 c.R / 255.0f,
            float32 c.G / 255.0f,
            float32 c.B / 255.0f,
            float32 c.A / 255.0f)

    static member ToRgbaFloat(c : uint32) =
        Conversions.ToRgbaFloat(Conversions.ToRgbaByte(c))

type ProjectionViewSet(device : GraphicsDevice, slot) =
    let factory = device.ResourceFactory
    let projBuffer =
        device.ResourceFactory.CreateBuffer(
            BufferDescription(uint32 (sizeof<Matrix4x4>), BufferUsage.UniformBuffer))
    let viewBuffer =
        device.ResourceFactory.CreateBuffer(
            BufferDescription(uint32 (sizeof<Matrix4x4>), BufferUsage.UniformBuffer))
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

type ResizableDeviceBuffer(device : GraphicsDevice, elementSize, usage) =
    let mutable buffer = device.ResourceFactory.CreateBuffer(BufferDescription(uint32 (elementSize * 8), usage))
    let log2 x =
        let mutable log = 0
        let mutable y = x
        while y > 1 do
            y <- y >>> 1
            log <- log + 1;
        log
    let nextLog2 x =
        let log = log2 x
        if x - (1 <<< log) > 0 then 1 + log else log
    let getRequiredCount count =
        1 <<< nextLog2 count
    member c.Buffer = buffer
    member c.Write<'v
            when 'v : struct 
            and 'v : (new : unit -> 'v) 
            and 'v :> ValueType>(src : ReadOnlyMemory<'v>) =
        // ensure device buffer is large enough
        let size = src.Length * elementSize
        if buffer.SizeInBytes < uint32 size then
            // destroy old buffer
            buffer.Dispose()
            // round up to pow2 number of elements (not bytes)
            let requiredSize = getRequiredCount src.Length * elementSize
            let desc = new BufferDescription(uint32 requiredSize, usage)    
            buffer <- device.ResourceFactory.CreateBuffer(desc)
        // write data
        use handle = src.Pin()
        device.UpdateBuffer(buffer, 0u, IntPtr handle.Pointer, uint32 size)
    member c.Dispose() =
        buffer.Dispose()
    interface IDisposable with 
        member c.Dispose() = c.Dispose()

type DeviceMesh(device, vertexSize) =
    let vb = new ResizableDeviceBuffer(device, vertexSize, BufferUsage.Dynamic ||| BufferUsage.VertexBuffer)
    let ib = new ResizableDeviceBuffer(device, sizeof<uint32>, BufferUsage.Dynamic ||| BufferUsage.IndexBuffer)
    let mutable indexCount = 0
    member c.WriteVertices(src) =
        vb.Write(src)
    member c.WriteIndexes(src) =
        ib.Write(src)
        indexCount <- src.Length
    member c.Draw(cmds : CommandList) =
        if indexCount > 0 then
            cmds.SetVertexBuffer(0u, vb.Buffer)
            cmds.SetIndexBuffer(ib.Buffer, IndexFormat.UInt32)
            cmds.DrawIndexed(
                indexCount = uint32 indexCount,
                instanceCount = 1u,
                indexStart = 0u,
                vertexOffset = 0,
                instanceStart = 0u)
    member c.Dispose() =
        vb.Dispose()
        ib.Dispose()

type ShaderSet(device : GraphicsDevice, 
                vert : ShaderDescription, 
                frag : ShaderDescription,
                layout : VertexLayoutDescription) =
    let shaders =
        try
            let vsCode = vert.ShaderBytes
            let fsCode = frag.ShaderBytes
            let vertexShader = device.ResourceFactory.CreateShader(ShaderDescription(vert.Stage, vsCode, vert.EntryPoint))
            let fragmentShader = device.ResourceFactory.CreateShader(ShaderDescription(frag.Stage, fsCode, frag.EntryPoint))
            [| vertexShader; fragmentShader |]
        with ex ->
            let msg = 
                sprintf "Could not create shaders: %s\nVertex:\n%s\nFragment" 
                    (Encoding.UTF8.GetString(vert.ShaderBytes)) 
                    (Encoding.UTF8.GetString(frag.ShaderBytes))
            raise (Exception(msg, ex))
    member c.Description =
        ShaderSetDescription(
            vertexLayouts = [| layout |],
            shaders = shaders)
    member c.Dispose() =
        for shader in shaders do
            shader.Dispose()
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()

type ColorTextureTrianglePipeline(device, shaders : ShaderSet, texture : Texture, sampler) =
    let projView = new ProjectionViewSet(device, 0)
    let worldTexture = new WorldTextureSet(device, texture, 1, sampler)
    let layouts = [| 
        projView.Layout
        worldTexture.Layout 
        |]
    let pipeline =
        let desc =
            GraphicsPipelineDescription(
                BlendState = BlendStateDescription.SingleAlphaBlend,
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
                Outputs = device.SwapchainFramebuffer.OutputDescription)
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
        member c.Dispose() =
            c.Dispose()

[<Struct>]
type PositionTextureDualColorVertex = {
    Position : Vector3
    TexCoord : Vector2
    Foreground : RgbaFloat
    Background : RgbaFloat
} with
    static member Description = 
        VertexLayoutDescription([|
            VertexElementDescription("Position",
                VertexElementFormat.Float3,
                VertexElementSemantic.TextureCoordinate)
            VertexElementDescription("TexCoord",
                VertexElementFormat.Float2,
                VertexElementSemantic.TextureCoordinate)
            VertexElementDescription("Foreground",
                VertexElementFormat.Float4,
                VertexElementSemantic.TextureCoordinate)
            VertexElementDescription("Background",
                VertexElementFormat.Float4,
                VertexElementSemantic.TextureCoordinate)
            |])

type IDrawable =
    inherit IDisposable
    abstract Draw : CommandList -> unit

type BufferedQuadMesh<'v
                when 'v : struct 
                and 'v : (new : unit -> 'v) 
                and 'v :> ValueType>(device) =
    let vertices = ArrayBufferWriter<'v>()
    let indexes = ArrayBufferWriter<int>()
    let mesh = DeviceMesh(device, sizeof<'v>)
    member c.GetMemory(count) =
        vertices.GetMemory(count)
    member c.GetSpan(count) =
        vertices.GetSpan(count)
    member c.Advance(count) = 
        vertices.Advance(count)
    interface IBufferWriter<'v> with
        member c.GetSpan(count) =
            c.GetSpan(count)
        member c.GetMemory(count) =
            c.GetMemory(count)
        member c.Advance(count) = 
            c.Advance(count)        
    member c.Flush() =
        let quadCount = vertices.WrittenCount / 4
        let indexCount = quadCount * 6
        let indexSpan = indexes.GetSpan(indexCount).Slice(0, indexCount)
        for i = 0 to quadCount - 1 do
            let vi = i * 4
            let ii = i * 6
            indexSpan.[ii + 0] <- vi + 0
            indexSpan.[ii + 1] <- vi + 1
            indexSpan.[ii + 2] <- vi + 2
            indexSpan.[ii + 3] <- vi + 0
            indexSpan.[ii + 4] <- vi + 2
            indexSpan.[ii + 5] <- vi + 3
        indexes.Advance(indexCount)
        mesh.WriteIndexes(indexes.WrittenMemory)
        indexes.Clear()
        mesh.WriteVertices(vertices.WrittenMemory)
        vertices.Clear()
    member c.Draw(cmds) =
        mesh.Draw(cmds)
    member c.Dispose() =
        mesh.Dispose()
    interface IDrawable with
        member c.Draw(cmds) =
            c.Draw(cmds)
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()        

type ColorTextureQuadLayers(device, shaders, texture, sampler) =
    let meshes = List<IDrawable>()
    let pipeline = new ColorTextureTrianglePipeline(device, shaders, texture, sampler)
    member c.GetLayer<'v
            when 'v : struct 
            and 'v : (new : unit -> 'v) 
            and 'v :> ValueType>(z) =
        while meshes.Count <= z do
            meshes.Add(new BufferedQuadMesh<'v>(device))
        meshes.[z] :?> BufferedQuadMesh<'v>
    member val WorldTransform = Matrix4x4.Identity with get, set
    member val ViewTransform = Matrix4x4.Identity with get, set
    member val ProjectionTransform = Matrix4x4.Identity with get, set
    member val TextureTransform = Matrix4x4.Identity with get, set
    member c.Draw(cmds) =
        // Set shader params
        pipeline.SetPipeline(cmds)
        pipeline.SetProjectionView(c.ProjectionTransform, c.ViewTransform, cmds)
        pipeline.SetWorldTexture(c.WorldTransform, c.TextureTransform, cmds)
        // Draw primitives
        for mesh in meshes do
            mesh.Draw(cmds)
    member c.Dispose() =
        for mesh in meshes do
            mesh.Dispose()
        pipeline.Dispose()
    interface IDrawable with
        member c.Draw(cmds) =
            c.Draw(cmds)
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()

type Renderer(device : GraphicsDevice) =
    let drawables = List<IDrawable>()
    let cmds = device.ResourceFactory.CreateCommandList()
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
            // We want to render directly to the output window.
            cmds.SetFramebuffer(device.SwapchainFramebuffer)
            // Clear viewports
            cmds.ClearColorTarget(0u, bgColor)
            // Call drawing
            for drawable in drawables do
                drawable.Draw(cmds)
            // End() must be called before commands can be submitted for execution.
            cmds.End()
            device.SubmitCommands(cmds)
            // Once commands have been submitted, the rendered image can be presented to 
            // the application window.
            device.SwapBuffers()
    member c.Dispose() =
        for drawable in drawables do
            drawable.Dispose()
        cmds.Dispose()
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()
