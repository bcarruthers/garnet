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
    
[<Struct>]    
type SpriteFlushMode =
    | NoFlush
    | FlushOnDraw

[<Struct>]
type SpriteLayerDescriptor = {
    Depth : int
    CameraId : int
    Primitive : Primitive
    Pipeline : TexturePipelineDescriptor
    FlushMode : SpriteFlushMode
    }

type SpriteCamera() =
    member val WorldTransform = Matrix4x4.Identity with get, set
    member val TextureTransform = Matrix4x4.Identity with get, set
    member val ViewTransform = Matrix4x4.Identity with get, set
    member val ProjectionTransform = Matrix4x4.Identity with get, set
    member c.GetNormalizedToWorld() =
        let projView = c.ViewTransform * c.ProjectionTransform 
        projView.GetInverseOrIdentity()
    
type SpriteRenderer(device, shaders, texture) =
    let indexes = new QuadIndexBuffer(device)
    let pipelines = new TexturePipelineCache(device, shaders, texture)
    let layers = List<SpriteLayerDescriptor>()
    let meshes = List<IVertexBuffer>()
    let cameras = List<SpriteCamera>()
    /// Layer depth must uniquely define a layer
    member c.GetVertices<'v
                when 'v : struct 
                and 'v : (new : unit -> 'v) 
                and 'v :> ValueType> desc =
        while meshes.Count <= desc.Depth do
            layers.Add { Unchecked.defaultof<_> with Depth = -1 }
            meshes.Add(new VertexBuffer<'v>(device))
        layers.[desc.Depth] <- desc
        meshes.[desc.Depth] :?> VertexBuffer<'v>
    member c.GetCamera(i) =
        while cameras.Count <= i do
            cameras.Add(SpriteCamera())
        cameras.[i]
    member c.Draw(context : RenderContext) =
        for i = 0 to layers.Count - 1 do
            let layer = layers.[i]
            if layer.Depth >= 0 then
                let pipeline = pipelines.GetPipeline(layer.Pipeline, context.OutputDescription)
                // Set shader params
                let camera = c.GetCamera(layer.CameraId)
                pipeline.SetPipeline(context.Commands)
                pipeline.SetProjectionView(camera.ProjectionTransform, camera.ViewTransform, context.Commands)
                pipeline.SetWorldTexture(camera.WorldTransform, camera.TextureTransform, context.Commands)
                // Flush if needed
                let vertices = meshes.[i]
                match layer.FlushMode with
                | NoFlush -> ()
                | FlushOnDraw -> vertices.Flush()
                // Draw primitives
                let vertexCount = vertices.SetVertexBuffer(context.Commands)
                if vertexCount > 0 then
                    match layers.[i].Primitive with
                    | Quad -> indexes.Draw(context.Commands, vertexCount / 4)
                    | Triangle ->
                        context.Commands.Draw(
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
        member c.Draw(context) = c.Draw(context)
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
    /// Returns index of first vertex contained in rect
    [<Extension>]
    static member TryPickPoint(span : Span<PositionTextureDualColorVertex>, rect : Range2) = 
        let mutable found = false
        let mutable i = 0
        while not found && i < span.Length do
            let v = span.[i].Position.ToVector2()
            found <- rect.Contains(v)
            i <- i + 1
        if found then ValueSome i else ValueNone

    /// Returns index of first triangle containing point
    [<Extension>]
    static member TryPickTriangle(span : Span<PositionTextureDualColorVertex>, p : Vector2) = 
        let mutable found = false
        let mutable i = 0
        while not found && i < span.Length do
            let v0 = span.[i + 0].Position.ToVector2()
            let v1 = span.[i + 1].Position.ToVector2()
            let v2 = span.[i + 2].Position.ToVector2()
            found <- p.IsInTriangle(v0, v1, v2)
            i <- i + 3
        if found then ValueSome (i / 3) else ValueNone
        
    /// Returns index of first quad containing point
    [<Extension>]
    static member TryPickQuad(span : Span<PositionTextureDualColorVertex>, p : Vector2) = 
        let mutable found = false
        let mutable i = 0
        while not found && i < span.Length do
            let v0 = span.[i + 0].Position.ToVector2()
            let v1 = span.[i + 1].Position.ToVector2()
            let v2 = span.[i + 2].Position.ToVector2()
            let v3 = span.[i + 3].Position.ToVector2()
            found <-
                p.IsInTriangle(v0, v1, v2) ||
                p.IsInTriangle(v0, v2, v3)
            i <- i + 4
        if found then ValueSome (i / 4) else ValueNone
        
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
