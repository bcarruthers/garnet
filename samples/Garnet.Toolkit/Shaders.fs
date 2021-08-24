namespace Garnet.Graphics

open System
open System.Collections.Generic
open System.IO
open System.Text
open Veldrid
open Garnet.Composition
open Veldrid.SPIRV

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

module private ShaderFileExtension =
    let stages = [|
        ".vert", ShaderStages.Vertex
        ".tesc", ShaderStages.TessellationControl
        ".tese", ShaderStages.TessellationEvaluation
        ".geom", ShaderStages.Geometry
        ".frag", ShaderStages.Fragment
        ".comp", ShaderStages.Compute
        |]
    
    let extensionFilter =
        stages
        |> Array.map (fun (extension, _) -> "*" + extension)
        |> String.concat ","

    let extensionToStage =
        let dict = Dictionary<string, ShaderStages>()
        for extension, stage in stages do
            dict.[extension] <- stage
        dict :> IReadOnlyDictionary<_,_>

[<AutoOpen>]
module ShaderExtensions =
    type Shader with
        static member GetCompilationTarget(backend) =
            match backend with
            | GraphicsBackend.Direct3D11 -> CrossCompileTarget.HLSL
            | GraphicsBackend.OpenGL -> CrossCompileTarget.GLSL
            | GraphicsBackend.Metal -> CrossCompileTarget.MSL
            | GraphicsBackend.OpenGLES -> CrossCompileTarget.ESSL
            | _ -> raise (SpirvCompilationException($"Invalid GraphicsBackend: {backend}"))

        static member GetBytecodeExtension(backend) =
            match backend with
            | GraphicsBackend.Direct3D11 -> ".hlsl.bytes"
            | GraphicsBackend.Vulkan -> ".spv"
            | GraphicsBackend.OpenGL
            | GraphicsBackend.OpenGLES -> raise (InvalidOperationException("OpenGL and OpenGLES do not support shader bytecode."))
            | _ -> raise (Exception($"Invalid GraphicsBackend: {backend}"))    

        static member GetShaderBytes(backend, code : string) =
            match backend with
            | GraphicsBackend.Direct3D11
            | GraphicsBackend.OpenGL
            | GraphicsBackend.OpenGLES -> Encoding.ASCII.GetBytes(code)
            | GraphicsBackend.Metal -> Encoding.UTF8.GetBytes(code)
            | _ -> raise (Exception($"Invalid GraphicsBackend: {backend}"))

        static member TryGetStage(extension) =
            match ShaderFileExtension.extensionToStage.TryGetValue(extension) with
            | true, stage -> ValueSome stage
            | false, _ -> ValueNone//raise (Exception($"Invalid extension: {extension}"))

        static member Compile(vert, frag, backend) =
            let target = Shader.GetCompilationTarget(backend)
            let result = SpirvCompilation.CompileVertexFragment(vert, frag, target)
            let vsCode = Shader.GetShaderBytes(backend, result.VertexShader)
            let fsCode = Shader.GetShaderBytes(backend, result.FragmentShader)
            (vsCode, fsCode)

type ShaderSetCache() =
    let cache = Dictionary<ShaderSetDescriptor, ShaderSet>()
    member c.TryGet(desc) =
        match cache.TryGetValue(desc) with
        | true, x -> ValueSome x
        | false, _ -> ValueNone
    member c.Add(name, shaderSet) =
        cache.Add(name, shaderSet)
    member c.Dispose() =
        for set in cache.Values do
            set.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()

[<AutoOpen>]
module ShaderLoadingExtensions =
    type IReadOnlyFolder with
        member c.LoadShader(key : string, backend : GraphicsBackend, cache : IResourceCache) =
            let extension = Path.GetExtension(key)
            match Shader.TryGetStage(extension) with
            | ValueNone -> ()
            | ValueSome stage ->
                let bytecodePath = 
                    let extension = Shader.GetBytecodeExtension(backend)
                    key + extension
                use stream =
                    match c.TryOpen(bytecodePath) with
                    | ValueNone -> c.Open(key)
                    | ValueSome x -> x
                let ms = new MemoryStream()
                stream.CopyTo(ms)
                let desc = ShaderDescription(stage, ms.ToArray(), "main")
                cache.AddResource(key, desc)
            
        member c.LoadShadersFromFolder(path, backend, cache : IResourceCache) =
            for key in c.GetFiles(path) do
                c.LoadShader(key, backend, cache)
    
type ShaderLoader(backend : GraphicsBackend, stage) =
    let extension = Shader.GetBytecodeExtension(backend)
    interface IResourceLoader with
        member c.Load(folder, cache, key) =
            let bytecodePath = key + extension
            use stream =
                match folder.TryOpen(bytecodePath) with
                | ValueNone -> folder.Open(key)
                | ValueSome x -> x
            let ms = new MemoryStream()
            stream.CopyTo(ms)
            let desc = ShaderDescription(stage, ms.ToArray(), "main")
            cache.AddResource(key, desc)

[<AutoOpen>]
module ShaderLoaderExtensions =
    type ResourceCache with
        member c.AddShaderLoaders(device : GraphicsDevice) =
            for extension, stage in ShaderFileExtension.stages do
                c.AddLoader(extension, ShaderLoader(device.BackendType, stage))
            
    type ShaderSetCache with
        member c.GetOrCreate(device, desc, cache : IResourceCache) =
            match c.TryGet(desc) with
            | ValueSome x -> x
            | ValueNone ->
                let vert = cache.LoadResource<ShaderDescription>(desc.VertexShader)
                let frag = cache.LoadResource<ShaderDescription>(desc.FragmentShader)
                let set = new ShaderSet(device, vert, frag, desc.Layout)
                c.Add(desc, set)
                set

