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

[<Struct>]
type internal ReadOnlyArray4<'a> = {
    Value0 : 'a
    Value1 : 'a
    Value2 : 'a
    Value3 : 'a
    } with
    member c.Item with get i =
        match i with
        | 0 -> c.Value0
        | 1 -> c.Value1
        | 2 -> c.Value2
        | 3 -> c.Value3
        | _ -> failwith $"Index out of range ({i})"
    static member Create(v0, v1, v2, v3) = {
        Value0 = v0
        Value1 = v1
        Value2 = v2
        Value3 = v3
        }
        
[<Extension>]
type SpriteVertexSpanExtensions =
    [<Extension>]
    static member DrawSprite(span : Span<PositionTextureDualColorVertex>, 
            center : Vector2, 
            rotation : Vector2, 
            size : Vector2, 
            texBounds : Range2, 
            fg : RgbaFloat, 
            bg : RgbaFloat) = 
        let dxDir = rotation
        let dyDir = dxDir.GetPerpendicular()
        let dx = dxDir * size.X
        let dy = dyDir * size.Y
        let p00 = center - (dx + dy) * 0.5f
        let p10 = p00 + dx
        let p11 = p10 + dy
        let p01 = p11 - dx
        let t00 = texBounds.Min
        let t11 = texBounds.Max
        span.[0] <- {
            Position = Vector3(p00.X, p00.Y, 0.0f)
            TexCoord = Vector2(t00.X, t00.Y)
            Foreground = fg
            Background = bg
            }
        span.[1] <- {
            Position = Vector3(p10.X, p10.Y, 0.0f)
            TexCoord = Vector2(t11.X, t00.Y)
            Foreground = fg
            Background = bg
            }
        span.[2] <- {
            Position = Vector3(p11.X, p11.Y, 0.0f)
            TexCoord = Vector2(t11.X, t11.Y)
            Foreground = fg
            Background = bg
            }
        span.[3] <- {
            Position = Vector3(p01.X, p01.Y, 0.0f)
            TexCoord = Vector2(t00.X, t11.Y)
            Foreground = fg
            Background = bg
            }

    [<Extension>]
    static member DrawRect(span : Span<PositionTextureDualColorVertex>, 
            rect : Range2,
            texBounds : Range2,
            fg : RgbaFloat,
            bg : RgbaFloat) = 
        span.DrawSprite(rect.Center, Vector2.UnitX, rect.Size, texBounds, fg, bg)

[<Extension>]
type SpriteVertexBufferWriterExtensions =
    [<Extension>]
    static member GetSpriteSpan(w : IBufferWriter<PositionTextureDualColorVertex>, spriteCount) = 
        let vertexCount = spriteCount * 4
        w.GetSpan(vertexCount).Slice(0, vertexCount)

    [<Extension>]
    static member DrawSprite(w : IBufferWriter<PositionTextureDualColorVertex>, 
            center : Vector2, 
            rotation : Vector2, 
            size : Vector2, 
            texBounds : Range2, 
            fg : RgbaFloat, 
            bg : RgbaFloat) = 
        let span = w.GetSpriteSpan(1)
        span.DrawSprite(center, rotation, size, texBounds, fg, bg)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawRect(w : IBufferWriter<PositionTextureDualColorVertex>, 
            rect : Range2,
            texBounds : Range2,
            fg : RgbaFloat,
            bg : RgbaFloat) = 
        let span = w.GetSpriteSpan(1)
        span.DrawSprite(rect.Center, Vector2.UnitX, rect.Size, texBounds, fg, bg)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawRect(w : IBufferWriter<PositionTextureDualColorVertex>, 
            rect : Range2,
            texBounds : Range2,
            fg : RgbaFloat) = 
        let span = w.GetSpriteSpan(1)
        span.DrawSprite(rect.Center, Vector2.UnitX, rect.Size, texBounds, fg, RgbaFloat.Clear)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawRect(w : IBufferWriter<PositionTextureDualColorVertex>, 
            rect : Range2, 
            color : RgbaFloat) =
        w.DrawRect(rect, Range2.Zero, color, color)

    [<Extension>]
    static member DrawNinePatchRect(w : IBufferWriter<PositionTextureDualColorVertex>, 
            rect : Range2i,
            atlasSize : Vector2i,
            texBounds : Range2i,
            margin0 : Vector2i,
            margin1 : Vector2i,
            fg : RgbaFloat,
            bg : RgbaFloat) = 
        let m0 = margin0.ToVector2()
        let m1 = margin1.ToVector2()
        let p0 = rect.Min.ToVector2()
        let p3 = rect.Max.ToVector2()
        let tc0 = texBounds.Min.ToVector2()
        let tc3 = texBounds.Max.ToVector2()
        let px = ReadOnlyArray4<_>.Create(p0.X, p0.X + m0.X, p3.X - m1.X, p3.X)        
        let py = ReadOnlyArray4<_>.Create(p0.Y, p0.Y + m0.Y, p3.Y - m1.Y, p3.Y)
        let tx = ReadOnlyArray4<_>.Create(tc0.X, tc0.X + m0.X, tc3.X - m1.X, tc3.X)        
        let ty = ReadOnlyArray4<_>.Create(tc0.Y, tc0.Y + m0.Y, tc3.Y - m1.Y, tc3.Y)
        let atlasScale = Vector2.One / atlasSize.ToVector2()
        let span = w.GetSpriteSpan(9)
        for y = 0 to 2 do
            for x = 0 to 2 do
                let t0 = Vector2(tx.[x], ty.[y]) * atlasScale
                let t1 = Vector2(tx.[x + 1], ty.[y + 1]) * atlasScale
                let p0 = Vector2(px.[x], py.[y])
                let p1 = Vector2(px.[x + 1], py.[y + 1])
                let i = y * 3 + x
                let span = span.Slice(i * 4)
                span.DrawRect(
                    Range2(p0, p1),
                    Range2(t0, t1),
                    fg,
                    bg)
        w.Advance(span.Length)
