namespace Garnet.Graphics

open System
open System.Collections.Generic
open System.IO
open System.Text
open Veldrid
open Garnet.Composition
open Veldrid.SPIRV

type IVertex = 
    abstract Layout : VertexLayoutDescription

type Vertex<'v when 'v : struct and 'v :> IVertex> =
    static member Layout = Unchecked.defaultof<'v>.Layout

[<Struct>]
type ShaderSetDescriptor = {
    VertexShader : string
    FragmentShader : string
    Layout : VertexLayoutDescription
    }

[<Struct>]
type ShaderSetDescriptor<'v
                when 'v : struct 
                and 'v : (new : unit -> 'v) 
                and 'v :> ValueType
                and 'v :> IVertex> = {
    VertexShader : string
    FragmentShader : string
    } with
    member c.Untyped = {
        VertexShader = c.VertexShader
        FragmentShader = c.FragmentShader
        Layout = Vertex<'v>.Layout
        } 

type ShaderSet(device : GraphicsDevice, 
                vert : ShaderDescription, 
                frag : ShaderDescription,
                layout : VertexLayoutDescription,
                isCompiled) =
    let shaders =
        try
            if not isCompiled then device.ResourceFactory.CreateFromSpirv(vert, frag)
            else
                let vsCode = vert.ShaderBytes
                let fsCode = frag.ShaderBytes
                let vertexShader = device.ResourceFactory.CreateShader(ShaderDescription(vert.Stage, vsCode, vert.EntryPoint))
                let fragmentShader = device.ResourceFactory.CreateShader(ShaderDescription(frag.Stage, fsCode, frag.EntryPoint))
                [| vertexShader; fragmentShader |]
        with ex ->
            let msg =
                let vertStr = Encoding.UTF8.GetString(vert.ShaderBytes)
                let fragStr = Encoding.UTF8.GetString(frag.ShaderBytes) 
                "Could not create shaders:\n" +
                $"Vertex ({vert.ShaderBytes.Length} bytes):\n{vertStr}\n" +
                $"Fragment ({frag.ShaderBytes.Length} bytes):\n{fragStr}"
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
    
type ShaderResource = {
    Description : ShaderDescription
    IsCompiled : bool
    } with
    static member FromStream(stage, isCompiled, stream : Stream) =
        let ms = new MemoryStream()
        stream.CopyTo(ms)
        {
            Description = ShaderDescription(stage, ms.ToArray(), "main")
            IsCompiled = isCompiled
        }

type ShaderLoader(backend : GraphicsBackend, stage) =
    interface IResourceLoader with
        /// Key should be the base shader without backend-specific extension, e.g. shader.vert
        member c.Load(folder, cache, key) =
            // First look for a compiled shader with the backend-specific extension
            let extension = Shader.GetBytecodeExtension(backend)
            let bytecodePath = key + extension
            let result = folder.TryOpen(bytecodePath)
            use stream = 
                match result with
                | ValueSome x -> x
                | ValueNone ->
                    // If backend-specific file was not found, fallback to original file
                    folder.Open(key)
            let resource = ShaderResource.FromStream(stage, result.IsSome, stream)
            cache.AddResource<ShaderResource>(key, resource)
    
[<AutoOpen>]
module ShaderLoadingExtensions =
    type IReadOnlyFolder with
        /// Key should be the base shader without backend-specific extension, e.g. shader.vert
        member c.LoadShader(key : string, backend : GraphicsBackend, cache : IResourceCache) =
            let extension = Path.GetExtension(key)
            match Shader.TryGetStage(extension) with
            | ValueNone -> ()
            | ValueSome stage ->
                let loader = ShaderLoader(backend, stage) :> IResourceLoader
                loader.Load(c, cache, key)
            
        member c.LoadShadersFromFolder(path, backend, cache : IResourceCache) =
            for key in c.GetFiles(path) do
                c.LoadShader(key, backend, cache)

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
                let vert = cache.LoadResource<ShaderResource>(desc.VertexShader)
                let frag = cache.LoadResource<ShaderResource>(desc.FragmentShader)
                if vert.IsCompiled <> frag.IsCompiled then
                    failwith $"Shaders must both be GLSL or compiled for the same backend: {desc.VertexShader}, {desc.FragmentShader}"
                let set = new ShaderSet(device, vert.Description, frag.Description, desc.Layout, vert.IsCompiled)
                c.Add(desc, set)
                set

