namespace Garnet.Samples.Veldrid

open System
open System.Text
open System.Numerics
open Veldrid
open Veldrid.SPIRV
open Garnet.Resources

module VertexShaders =
    let toShader (code : string) =
        ShaderDescription(
            ShaderStages.Vertex,
            Encoding.UTF8.GetBytes(code), "main")

    let color = toShader @"
        #version 450

        layout(location = 0) in vec3 Position;
        layout(location = 1) in vec4 Color;

        layout(location = 0) out vec4 fsin_Color;

        void main()
        {
            gl_Position = vec4(Position, 1);
            fsin_Color = Color;
        }"

    let colorTransformed = toShader @"
        #version 450
        layout(set = 0, binding = 0) uniform ProjectionBuffer
        {
            mat4 Projection;
        };
        layout(set = 0, binding = 1) uniform ViewBuffer
        {
            mat4 View;
        };
        layout(set = 1, binding = 0) uniform WorldBuffer
        {
            mat4 World;
        };
        layout(location = 0) in vec3 Position;
        layout(location = 1) in vec4 Color;
        layout(location = 0) out vec4 fsin_color;
        void main()
        {
            vec4 worldPosition = World * vec4(Position, 1);
            vec4 viewPosition = View * worldPosition;
            vec4 clipPosition = Projection * viewPosition;
            gl_Position = clipPosition;
            fsin_color = Color;
        }"

    let textureTransformed = toShader @"
        #version 450
        layout(set = 0, binding = 0) uniform ProjectionBuffer
        {
            mat4 Projection;
        };
        layout(set = 0, binding = 1) uniform ViewBuffer
        {
            mat4 View;
        };
        layout(set = 1, binding = 0) uniform WorldBuffer
        {
            mat4 World;
        };
        layout(location = 0) in vec3 Position;
        layout(location = 1) in vec2 TexCoords;
        layout(location = 0) out vec2 fsin_texCoords;
        void main()
        {
            //vec4 worldPosition = World * vec4(Position, 1);
            //vec4 viewPosition = View * worldPosition;
            //vec4 clipPosition = Projection * viewPosition;
            //gl_Position = clipPosition;
            gl_Position = vec4(Position, 1);
            fsin_texCoords = TexCoords;
        }"

module FragmentShaders =
    let toShader (code : string) =
        ShaderDescription(
            ShaderStages.Fragment,
            Encoding.UTF8.GetBytes(code), "main")

    let color = toShader @"
        #version 450
        layout(location = 0) in vec4 fsin_color;
        layout(location = 0) out vec4 fsout_color;
        void main()
        {
            fsout_color = fsin_color;
        }"


    let texture = toShader @"
        #version 450
        layout(location = 0) in vec2 fsin_texCoords;
        layout(location = 0) out vec4 fsout_color;
        layout(set = 1, binding = 1) uniform texture2D SurfaceTexture;
        layout(set = 1, binding = 2) uniform sampler SurfaceSampler;
        void main()
        {
            fsout_color =  texture(sampler2D(SurfaceTexture, SurfaceSampler), fsin_texCoords);
        }"

type ShaderSet(device : GraphicsDevice, 
                vert : ShaderDescription, 
                frag : ShaderDescription,
                layout : VertexLayoutDescription) =
    let shaders =
        try
            printfn "Creating shaders"
            device.ResourceFactory.CreateFromSpirv(vert, frag)
        with :? SpirvCompilationException as ex ->
            printfn "Could not create shaders: %s\nVertex:\n%s\nFragment:\n%s" 
                ex.Message (Encoding.UTF8.GetString(vert.ShaderBytes)) 
                (Encoding.UTF8.GetString(frag.ShaderBytes))
            // use fallback shaders
            device.ResourceFactory.CreateFromSpirv(VertexShaders.color, FragmentShaders.color)
    member c.Description =
        ShaderSetDescription(
            vertexLayouts = [| layout |],
            shaders = shaders)
    interface IDisposable with
        member c.Dispose() =
            for shader in shaders do
                shader.Dispose()

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
   
type WorldTextureSet(device : GraphicsDevice, surfaceTexture : Resource<Texture>, slot) =
    let factory = device.ResourceFactory
    let worldBuffer = factory.CreateBuffer(BufferDescription(64u, BufferUsage.UniformBuffer))
    let worldTextureLayout = 
        factory.CreateResourceLayout(
            ResourceLayoutDescription(
                ResourceLayoutElementDescription(
                    "WorldBuffer", 
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
    let surfaceTextureView = surfaceTexture.DeriveDisposable(fun surfaceTexture ->
        factory.CreateTextureView(surfaceTexture)
        )
    let worldTextureSet = surfaceTextureView.DeriveDisposable(fun surfaceTextureView ->
        factory.CreateResourceSet(
            ResourceSetDescription(
                worldTextureLayout,
                worldBuffer,
                surfaceTextureView,
                device.Aniso4xSampler)))
    member c.Layout = worldTextureLayout
    member c.Apply(world : Matrix4x4, cmds : CommandList) =
        cmds.UpdateBuffer(worldBuffer, 0u, world)
        cmds.SetGraphicsResourceSet(uint32 slot, worldTextureSet.Load())
    member c.Dispose() =
        worldTextureSet.Dispose()
        worldTextureLayout.Dispose()
        worldBuffer.Dispose()
        surfaceTextureView.Dispose()
    interface IDisposable with 
        member c.Dispose() = c.Dispose()
