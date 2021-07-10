namespace Garnet.Engine

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

module Shaders =
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

module TextureLoading =
    let getMipSize original mipLevel =
        original >>> mipLevel |> max 1
    
    let getFormatSize format =
        match format with
        | PixelFormat.R8_G8_B8_A8_UNorm -> 4
        | PixelFormat.BC3_UNorm -> 1
        | _ -> failwithf "Unsupported format %A" format

    let loadTo (texture : Texture) (device : GraphicsDevice) (desc : TextureDescription) (data : ReadOnlyMemory<byte>) =
        let factory = device.ResourceFactory
        // create staging texture
        use staging = 
            factory.CreateTexture(
                TextureDescription(
                    desc.Width, desc.Height, desc.Depth, desc.MipLevels, 
                    desc.ArrayLayers, desc.Format, TextureUsage.Staging, 
                    desc.Type))
        // copy from buffer to staging
        use handle = data.Pin()
        let formatSize = getFormatSize desc.Format
        let mutable offset = 0
        for level = 0 to int desc.MipLevels - 1 do
            let mipWidth = getMipSize (int desc.Width) level
            let mipHeight = getMipSize (int desc.Height) level
            let mipDepth = getMipSize (int desc.Depth) level
            let subresourceSize = mipWidth * mipHeight * mipDepth * formatSize
            for layer = 0 to int desc.ArrayLayers - 1 do
                device.UpdateTexture(
                    staging, IntPtr handle.Pointer + nativeint offset, uint32 subresourceSize,
                    0u, 0u, 0u, uint32 mipWidth, uint32 mipHeight, uint32 mipDepth,
                    uint32 level, 
                    uint32 layer)
                offset <- offset + subresourceSize
        // copy from staging to final
        use cl = factory.CreateCommandList()
        cl.Begin()
        cl.CopyTexture(staging, texture)
        cl.End()
        device.SubmitCommands(cl)
        texture

    let load (device : GraphicsDevice) (desc : TextureDescription) data =
        let texture = device.ResourceFactory.CreateTexture(desc)
        loadTo texture device desc data

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

        member c.LoadTexture(device, image : Image<Rgba32>) =
            let w = image.Width
            let h = image.Height
            let bytes = Array.zeroCreate<byte>(w * h * 4)
            for y = 0 to h - 1 do
                let row = image.GetPixelRowSpan(y)
                let src = MemoryMarshal.Cast<Rgba32, byte>(row)
                let dest = bytes.AsSpan().Slice(w * 4 * y, w * 4)
                src.CopyTo(dest)
            let desc = 
                TextureDescription(
                    Width = uint32 image.Width, 
                    Height = uint32 image.Height, 
                    Depth = 1u, 
                    MipLevels = 1u, 
                    ArrayLayers = 1u, 
                    Format = PixelFormat.R8_G8_B8_A8_UNorm,
                    Usage = TextureUsage.Sampled,
                    Type = TextureType.Texture2D)
            TextureLoading.load device desc (ReadOnlyMemory(bytes))

        member c.LoadTexture(device, key) =
            let image = c.LoadImage(key)
            c.LoadTexture(device, image)

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
            