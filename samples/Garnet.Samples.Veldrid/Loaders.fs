namespace Garnet.Samples.Veldrid

open System.IO
open System.Text
open Veldrid
open Veldrid.ImageSharp
open Garnet.Resources
//open Newtonsoft.Json

type ShaderLoader(stage) =
    interface IResourceLoader<ShaderDescription> with
        member c.Load(stream) =
            let r = new StreamReader(stream)
            let code = r.ReadToEnd()
            ShaderDescription(
                stage,
                Encoding.UTF8.GetBytes(code), "main")
        member c.Dispose x = ()

type TextureLoader(device : GraphicsDevice) =
    interface IResourceLoader<Texture> with
        member c.Load(stream) =
            let tex = ImageSharpTexture(stream)
            tex.CreateDeviceTexture(device, device.ResourceFactory)
        member c.Dispose x =
            x.Dispose()

//type JsonLoader<'a>() =
//    let serializer = new JsonSerializer()
//    member c.Load(stream : Stream) =
//        let reader = new StreamReader(stream)
//        let jsonReader = new JsonTextReader(reader)
//        serializer.Deserialize<'a>(jsonReader)
//    interface IResourceLoader<'a> with
//        member c.Load(stream) = c.Load(stream)
//        member c.Dispose x = ()

type TextLoader() =
    interface IResourceLoader<string> with
        member c.Load(stream) =
            let r = new StreamReader(stream)
            r.ReadToEnd()
        member c.Dispose x = ()

[<AutoOpen>]
module LoaderExtensions =
    type ResourceSet with
        member c.RegisterGraphics(device : GraphicsDevice) =
            c.Register<ShaderDescription>(".vert", ShaderLoader(ShaderStages.Vertex))
            c.Register<ShaderDescription>(".tesc", ShaderLoader(ShaderStages.TessellationControl))
            c.Register<ShaderDescription>(".tese", ShaderLoader(ShaderStages.TessellationEvaluation))
            c.Register<ShaderDescription>(".geom", ShaderLoader(ShaderStages.Geometry))
            c.Register<ShaderDescription>(".frag", ShaderLoader(ShaderStages.Fragment))
            c.Register<ShaderDescription>(".comp", ShaderLoader(ShaderStages.Compute))
            c.Register<Texture>(".png", TextureLoader(device))
            c.Register<Texture>(".jpg", TextureLoader(device))


