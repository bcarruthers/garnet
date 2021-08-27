namespace Garnet.Graphics

open System
open System.Collections.Generic
open System.Buffers
open System.Numerics
open System.Runtime.CompilerServices
open Veldrid
open Garnet.Numerics

[<Struct>]    
type Primitive =
    | Triangle
    | Quad
    
type Primitive with
    static member GetVertexCount(primitive) =
        match primitive with
        | Triangle -> 3
        | Quad -> 4

[<Struct>]    
type SpriteFlushMode =
    | NoFlush
    | FlushOnDraw

type SpriteLayerDescriptor = {
    LayerId : int
    CameraId : int
    Primitive : Primitive
    Pipeline : TexturePipelineDescriptor
    FlushMode : SpriteFlushMode
    }

type Camera() =
    member val WorldTransform = Matrix4x4.Identity with get, set
    member val TextureTransform = Matrix4x4.Identity with get, set
    member val ViewTransform = Matrix4x4.Identity with get, set
    member val ProjectionTransform = Matrix4x4.Identity with get, set
    member c.GetNormalizedToWorld() =
        let projView = c.ViewTransform * c.ProjectionTransform 
        projView.GetInverseOrIdentity()
    member c.NormalizedToWorld(normPos : Vector2) =
        let xf = c.GetNormalizedToWorld()
        Vector2.Transform(normPos, xf)
    member c.NormalizedToWorld(rect : Range2) =
        let xf = c.GetNormalizedToWorld()
        let p0 = Vector2.Transform(rect.Min, xf)
        let p1 = Vector2.Transform(rect.Max, xf)
        Range2.Union(Range2.Point(p0), Range2.Point(p1))
    
type CameraSet() =
    let cameras = List<Camera>()
    member c.Item with get i =
        while cameras.Count <= i do
            cameras.Add(Camera())
        cameras.[i]
        
[<Struct>]
type private SpriteLayer = {
    Descriptor : SpriteLayerDescriptor
    Vertices : IVertexBuffer
    }

type SpriteRenderer(device, shaders, cache) =
    let indexes = new QuadIndexBuffer(device)
    let pipelines = new TexturePipelineCache(device, shaders, cache)
    let layers = List<SpriteLayer voption>()
    member c.GetVertices<'v
                when 'v : struct 
                and 'v : (new : unit -> 'v) 
                and 'v :> ValueType> desc =
        while layers.Count <= desc.LayerId do
            layers.Add(ValueNone)
        match layers.[desc.LayerId] with
        | ValueNone ->
            let vertices = new VertexBuffer<'v>(device)
            let layer = {
                Descriptor = desc
                Vertices = vertices
                }
            layers.[desc.LayerId] <- ValueSome layer
            vertices
        | ValueSome layer -> layer.Vertices :?> VertexBuffer<'v>
    member c.Flush(layerId) =
        if layerId < layers.Count then
            match layers.[layerId] with
            | ValueSome layer -> layer.Vertices.Flush()
            | ValueNone -> ()
    member c.Draw(context : RenderContext, cameras : CameraSet) =
        for layer in layers do
            match layer with
            | ValueNone -> ()
            | ValueSome layer ->
                let desc = layer.Descriptor
                let vertices = layer.Vertices
                // Flush if needed
                match desc.FlushMode with
                | NoFlush -> ()
                | FlushOnDraw -> vertices.Flush()
                // Proceed if not empty
                let vertexCount = vertices.VertexCount
                if vertexCount > 0 then
                    // Set shader params
                    let camera = cameras.[desc.CameraId]
                    let pipeline = pipelines.GetPipeline(desc.Pipeline, context.OutputDescription)
                    pipeline.SetPipeline(context.Commands)
                    pipeline.SetProjectionView(camera.ProjectionTransform, camera.ViewTransform, context.Commands)
                    pipeline.SetWorldTexture(camera.WorldTransform, camera.TextureTransform, context.Commands)
                    // Draw primitives
                    vertices.SetVertexBuffer(context.Commands)
                    match desc.Primitive with
                    | Quad -> indexes.Draw(context.Commands, vertexCount / 4)
                    | Triangle ->
                        context.Commands.Draw(
                            vertexCount = uint32 vertexCount,
                            instanceCount = 1u,
                            vertexStart = 0u,
                            instanceStart = 0u)
    member c.Dispose() =
        for layer in layers do
            match layer with
            | ValueNone -> ()
            | ValueSome layer ->            
                layer.Vertices.Dispose()
        pipelines.Dispose()
        indexes.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()
