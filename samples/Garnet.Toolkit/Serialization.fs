namespace Garnet.Composition

open System
open System.IO
open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open Veldrid
open Garnet.Numerics

module private JsonSerialization =
    type RgbaFloatConverter() =
        inherit JsonConverter<RgbaFloat>()
        override _.WriteJson(writer : JsonWriter, value : RgbaFloat, _ : JsonSerializer) =
            writer.WriteValue(value.ToRgbaByte().ToString())
        override _.ReadJson(reader, objectType, existingValue, hasExistingValue, serializer) =
            let s = string reader.Value
            RgbaFloat.Parse s
            
    // http://gorodinski.com/blog/2013/01/05/json-dot-net-type-converters-for-f-option-list-tuple/
    type OptionConverter() =
        inherit JsonConverter() 
        override x.CanConvert(t) = 
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
        override x.WriteJson(writer, value, serializer) =
            let value = 
                if value = null then null
                else 
                    let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
                    fields.[0]  
            serializer.Serialize(writer, value)
        override x.ReadJson(reader, t, existingValue, serializer) =        
            let innerType = t.GetGenericArguments().[0]
            let innerType = 
                if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
                else innerType        
            let value = serializer.Deserialize(reader, innerType)
            let cases = FSharpType.GetUnionCases(t)
            if value = null then FSharpValue.MakeUnion(cases.[0], [||])
            else FSharpValue.MakeUnion(cases.[1], [|value|])
            
    let defaultSettings =
        let settings =
            JsonSerializerSettings(
                Formatting = Formatting.Indented,
                NullValueHandling = NullValueHandling.Ignore)
        settings.Converters.Add(OptionConverter())
        settings.Converters.Add(RgbaFloatConverter())
        settings
        
[<AutoOpen>]
module LoadingExtensions =
    type IStreamSource with
        member c.LoadText(key) =
            use stream = c.Open(key)
            use reader = new StreamReader(stream)
            reader.ReadToEnd()
                
        member c.LoadJson<'a>(key, settings : JsonSerializerSettings) =
            let json = c.LoadText(key)
            try JsonConvert.DeserializeObject<'a>(json, settings)
            with ex -> raise (exn($"Could not load JSON from {key} as {typeof<'a>.Name}", ex))

        member c.LoadJson<'a> key =
            c.LoadJson<'a>(key, JsonSerialization.defaultSettings)

        member c.LoadJson<'a>(key, cache : IResourceCache, settings) =
            let resource = c.LoadJson<'a>(key, settings)
            cache.AddResource(key, resource)

        member c.LoadJson<'a>(key, cache) =
            c.LoadJson<'a>(key, cache, JsonSerialization.defaultSettings)

type TextLoader() =
    interface IResourceLoader with
        member c.Load(folder, cache, key) =
            cache.AddResource(key, folder.LoadText(key))

type JsonLoader<'a>(settings) =
    new() = JsonLoader<'a>(JsonSerialization.defaultSettings)
    interface IResourceLoader with
        member c.Load(folder, cache, key) =
            folder.LoadJson<'a>(key, cache, settings)

[<AutoOpen>]
module LoaderExtensions =
    type ResourceCache with
        member c.AddTextLoaders() =
            c.AddLoader(".json", TextLoader())
            c.AddLoader(".txt", TextLoader())
