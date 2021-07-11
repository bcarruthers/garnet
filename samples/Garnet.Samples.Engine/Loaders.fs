namespace Garnet.Samples.Engine

open System
open System.IO
open System.Runtime.InteropServices
open System.Text
open Newtonsoft.Json
open Veldrid
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

type IStreamSource =
    abstract TryOpen : string -> ValueOption<Stream>

[<AutoOpen>]
module StreamSource =
    type IStreamSource with
        member c.Open(key) =
            match c.TryOpen(key) with
            | ValueNone -> failwithf "Could not open %s" key
            | ValueSome x -> x

type FileStreamSource(dir) =
    interface IStreamSource with
        member c.TryOpen(key) =
            let path = Path.Combine(dir, key)
            if File.Exists(path) then ValueSome (File.OpenRead(path) :> Stream)
            else ValueNone

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
module LoaderExtensions =
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

        member c.LoadShaderSet(device : GraphicsDevice, vertexShader, fragmentShader, layout) =
            let vert = c.LoadShader(vertexShader, device.BackendType)
            let frag = c.LoadShader(fragmentShader, device.BackendType)
            new ShaderSet(device, vert, frag, layout)

        member c.LoadImage(key) =
            use stream = c.Open(key)
            Image.Load<Rgba32>(stream)

        member c.LoadTexture(device : GraphicsDevice, key) =
            let image = c.LoadImage(key)
            device.CreateTexture(image)

        member c.LoadTextureAtlas(device : GraphicsDevice, atlasWidth, atlasHeight, keys) =
            let images = keys |> Seq.map (fun key -> key, c.LoadImage(key))
            device.CreateTextureAtlas(atlasWidth, atlasHeight, images)

        member c.LoadWave(device : AudioDevice, key) =
            use stream = c.Open(key)
            // https://stackoverflow.com/questions/8754111/how-to-read-the-data-in-a-wav-file-to-an-array
            use reader = new BinaryReader(stream)
            // chunk 0
            let chunkId       = reader.ReadInt32()
            let fileSize      = reader.ReadInt32()
            let riffType      = reader.ReadInt32()
            // chunk 1
            let fmtID         = reader.ReadInt32()
            let fmtSize       = reader.ReadInt32() // bytes for this chunk (expect 16 or 18)
            // 16 bytes coming
            let fmtCode       = int (reader.ReadInt16())
            let channels      = int (reader.ReadInt16())
            let sampleRate    = reader.ReadInt32()
            let byteRate      = reader.ReadInt32()
            let fmtBlockAlign = int (reader.ReadInt16())
            let bitDepth      = int (reader.ReadInt16())
            if fmtSize = 18 then
                // Read any extra values
                let fmtExtraSize = int (reader.ReadInt16())
                stream.Seek(int64 fmtExtraSize, SeekOrigin.Current) |> ignore
            // chunk 2
            let dataId = reader.ReadInt32()
            let length = reader.ReadInt32()
            let data = reader.ReadBytes(length)
            let bytesForSample = bitDepth / 8
            let sampleCount = length / bytesForSample
            let desc = {
                channels = channels
                bitsPerSample = bitDepth
                sampleRate = sampleRate
                sampleCount = sampleCount
                }
            device.CreateSound(desc, ReadOnlyMemory(data))
                
        member c.LoadJson<'a>(key) =
            use stream = c.Open(key)
            use reader = new StreamReader(stream)
            let json = reader.ReadToEnd()
            JsonConvert.DeserializeObject<'a>(json)
            