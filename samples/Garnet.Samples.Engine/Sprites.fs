namespace Garnet.Samples.Engine

open System
open System.Collections.Generic
open System.Buffers
open System.Numerics
open System.Runtime.CompilerServices
open Veldrid

[<Struct>]
type Blend =
    | Additive = 0
    | Alpha = 1
    | Override = 2

[<Struct>]
type Filtering =
    | Point = 0
    | Linear = 1

[<Struct>]    
type Primitive =
    | Triangle
    | Quad

[<Struct>]
type SpriteLayerDescriptor = {
    Depth : int
    Blend : Blend
    Filtering : Filtering
    Primitive : Primitive
    ViewportId : int
    }

module SpriteLayerDescriptor =
    let init z blend filtering viewportId = {
        Depth = z
        Blend = blend
        Filtering = filtering
        Primitive = Quad
        ViewportId = viewportId
        }

    let depth z = init z Blend.Alpha Filtering.Linear 0
    let zero = depth 0
    
[<Struct>]
type SpritePipelineKey = {
    Blend : Blend
    Filtering : Filtering
    }
    
type SpritePipelineCache(device : GraphicsDevice, shaders, texture, outputDesc) =
    let pipelines = Dictionary<SpritePipelineKey, ColorTextureTrianglePipeline>()
    member c.GetPipeline(key) =
        match pipelines.TryGetValue(key) with
        | true, pipeline -> pipeline
        | false, _ ->
            let sampler =
                match key.Filtering with
                | Filtering.Point -> device.PointSampler
                | Filtering.Linear -> device.LinearSampler
                | x -> failwith $"Invalid filtering {x}"
            let blend =
                match key.Blend with
                | Blend.Additive -> BlendStateDescription.SingleAdditiveBlend
                | Blend.Alpha -> BlendStateDescription.SingleAlphaBlend
                | Blend.Override -> BlendStateDescription.SingleOverrideBlend
                | x -> failwith $"Invalid blend {x}"
            let pipeline = new ColorTextureTrianglePipeline(device, shaders, texture, sampler, blend, outputDesc)
            pipelines.Add(key, pipeline)
            pipeline
    member c.Dispose() =
        for pipeline in pipelines.Values do
            pipeline.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()

type SpriteViewport() =
    member val WorldTransform = Matrix4x4.Identity with get, set
    member val TextureTransform = Matrix4x4.Identity with get, set
    member val ViewTransform = Matrix4x4.Identity with get, set
    member val ProjectionTransform = Matrix4x4.Identity with get, set
    member c.GetNormalizedToWorld() =
        let projView = c.ViewTransform * c.ProjectionTransform 
        projView.GetInverseOrIdentity()
    
type SpriteRenderer(device, shaders, texture, outputDesc) =
    let indexes = new QuadIndexBuffer(device)
    let pipelines = new SpritePipelineCache(device, shaders, texture, outputDesc)
    let layers = List<SpriteLayerDescriptor>()
    let meshes = List<IVertexBuffer>()
    let viewports = List<SpriteViewport>()
    /// Layer depth must uniquely define a layer
    member c.GetLayer<'v
            when 'v : struct 
            and 'v : (new : unit -> 'v) 
            and 'v :> ValueType> desc =
        while meshes.Count <= desc.Depth do
            layers.Add(SpriteLayerDescriptor.depth meshes.Count)
            meshes.Add(new VertexBuffer<'v>(device))
        layers.[desc.Depth] <- desc
        meshes.[desc.Depth] :?> VertexBuffer<'v>
    member c.GetViewport(i) =
        while viewports.Count <= i do
            viewports.Add(SpriteViewport())
        viewports.[i]
    member c.Draw(cmds) =
        let mutable current = ValueNone
        for i = 0 to layers.Count - 1 do
            let layer = layers.[i]
            let key = {
                Blend = layer.Blend
                Filtering = layer.Filtering
                }
            let pipeline =
                match current with
                | ValueNone ->
                    let pipeline = pipelines.GetPipeline(key)
                    current <- ValueSome struct(key, pipeline)
                    pipeline
                | ValueSome struct(lastKey, pipeline) ->
                    if key = lastKey then pipeline
                    else
                        let pipeline = pipelines.GetPipeline(key)
                        current <- ValueSome struct(key, pipeline)
                        pipeline
            // Set shader params
            let viewport = c.GetViewport(layer.ViewportId)
            pipeline.SetPipeline(cmds)
            pipeline.SetProjectionView(viewport.ProjectionTransform, viewport.ViewTransform, cmds)
            pipeline.SetWorldTexture(viewport.WorldTransform, viewport.TextureTransform, cmds)
            // Draw primitives
            let vertexCount = meshes.[i].SetVertexBuffer(cmds)
            if vertexCount > 0 then
                match layers.[i].Primitive with
                | Quad -> indexes.Draw(cmds, vertexCount / 4)
                | Triangle ->
                    cmds.Draw(
                        vertexCount = uint32 vertexCount,
                        instanceCount = 1u,
                        vertexStart = 0u,
                        instanceStart = 0u)
    member c.Dispose() =
        for mesh in meshes do
            mesh.Dispose()
        pipelines.Dispose()
        indexes.Dispose()
    interface IDrawable with
        member c.Draw(cmds) = c.Draw(cmds)
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
        let p0 = center - (dx + dy) * 0.5f
        let p1 = p0 + dx
        let p2 = p1 + dy
        let p3 = p2 - dx
        let t0 = texBounds.Min
        let t2 = texBounds.Max
        span.[0] <- {
            Position = Vector3(p0.X, p0.Y, 0.0f)
            TexCoord = Vector2(t0.X, t0.Y)
            Foreground = fg
            Background = bg
            }
        span.[1] <- {
            Position = Vector3(p1.X, p1.Y, 0.0f)
            TexCoord = Vector2(t2.X, t0.Y)
            Foreground = fg
            Background = bg
            }
        span.[2] <- {
            Position = Vector3(p2.X, p2.Y, 0.0f)
            TexCoord = Vector2(t2.X, t2.Y)
            Foreground = fg
            Background = bg
            }
        span.[3] <- {
            Position = Vector3(p3.X, p3.Y, 0.0f)
            TexCoord = Vector2(t0.X, t2.Y)
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

    [<Extension>]
    static member DrawText(w : IBufferWriter<PositionTextureDualColorVertex>,
            text : string,
            pos : Vector2i,
            atlasSize : Vector2i,
            fontTexBounds : Range2i,
            charMargin : Vector2i,
            color : RgbaFloat) =
        let charSetSize = fontTexBounds.Size
        let charSize = charSetSize / Vector2i(16, 16)
        let displayCharSize = charSize + charMargin
        let atlasScale = Vector2.One / atlasSize.ToVector2()
        let span = w.GetSpriteSpan(text.Length)
        for i = 0 to text.Length - 1 do
            let ch = text.[i]
            let tileId = int ch
            let tx = tileId &&& 0xf
            let ty = tileId >>> 4
            let t0 = Vector2i(tx, ty) * charSize + fontTexBounds.Min
            let t1 = t0 + charSize
            let tb = Range2(t0.ToVector2() * atlasScale, t1.ToVector2() * atlasScale)
            let b = Range2i.Sized(pos + Vector2i(i * displayCharSize.X, 0), charSize)
            let span = span.Slice(i * 4)
            span.DrawRect(b.ToRange2(), tb, color, RgbaFloat.Clear)
        w.Advance(span.Length)
