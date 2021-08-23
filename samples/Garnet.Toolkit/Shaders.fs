namespace Garnet.Graphics

open System
open System.Collections.Generic
open System.IO
open System.Text
open Veldrid
open Garnet.Resources

[<Struct>]
type ShaderSetDescriptor = {
    VertexShader : string
    FragmentShader : string
    Layout : VertexLayoutDescription
    }

type ShaderSet(device : GraphicsDevice, 
                vert : ShaderDescription, 
                frag : ShaderDescription,
                layout : VertexLayoutDescription) =
    let shaders =
        try
            //device.ResourceFactory.CreateFromSpirv(vert, frag)
            //let (vsCode, fsCode) = Shaders.compile vert.ShaderBytes frag.ShaderBytes device.BackendType
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

module internal Shaders =
    let getBytecodeExtension backend =
        match backend with
        | GraphicsBackend.Direct3D11 -> ".hlsl.bytes"
        | GraphicsBackend.Vulkan -> ".spv"
        | GraphicsBackend.OpenGL
        | GraphicsBackend.OpenGLES -> raise (InvalidOperationException("OpenGL and OpenGLES do not support shader bytecode."))
        | _ -> raise (Exception($"Invalid GraphicsBackend: {backend}"))    

    let getShaderBytes backend (code : string) =
        match backend with
        | GraphicsBackend.Direct3D11
        | GraphicsBackend.OpenGL
        | GraphicsBackend.OpenGLES -> Encoding.ASCII.GetBytes(code)
        | GraphicsBackend.Metal -> Encoding.UTF8.GetBytes(code)
        | _ -> raise (Exception($"Invalid GraphicsBackend: {backend}"))

    let getStage extension =
        match extension with
        | ".vert" -> ShaderStages.Vertex
        | ".tesc" -> ShaderStages.TessellationControl
        | ".tese" -> ShaderStages.TessellationEvaluation
        | ".geom" -> ShaderStages.Geometry
        | ".frag" -> ShaderStages.Fragment
        | ".comp" -> ShaderStages.Compute
        | _ -> raise (Exception($"Invalid extension: {extension}"))

[<AutoOpen>]
module ShaderLoaderExtensions =
    type IStreamSource with
        member c.LoadShader(key : string, backend : GraphicsBackend) =
            let stage = 
                let extension = Path.GetExtension(key)
                Shaders.getStage extension
            let bytecodePath = 
                let extension = Shaders.getBytecodeExtension backend
                key + extension
            use stream =
                match c.TryOpen(bytecodePath) with
                | ValueNone -> c.Open(key)
                | ValueSome x -> x
            let ms = new MemoryStream()
            stream.CopyTo(ms)
            ShaderDescription(stage, ms.ToArray(), "main")

        member c.LoadShaderSet(device : GraphicsDevice, desc) =
            let vert = c.LoadShader(desc.VertexShader, device.BackendType)
            let frag = c.LoadShader(desc.FragmentShader, device.BackendType)
            new ShaderSet(device, vert, frag, desc.Layout)
    
type ShaderSetCache() =
    let cache = Dictionary<ShaderSetDescriptor, ShaderSet>()
    member c.Item with get name = cache.[name]
    member c.Add(name, font) =
        cache.Add(name, font)
    member c.Dispose() =
        for set in cache.Values do
            set.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()

type ShaderSetCache with
    member c.Load(device, fs : IReadOnlyFolder, desc) =
        let set = fs.LoadShaderSet(device, desc)
        c.Add(desc, set)
    
