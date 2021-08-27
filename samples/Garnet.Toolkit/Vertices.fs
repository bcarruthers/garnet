namespace Garnet.Graphics

open System
open System.Buffers
open System.Numerics
open System.Runtime.CompilerServices
open Veldrid
open Garnet.Numerics

[<Struct>]
type PositionColorVertex = {
    Position : Vector3
    Color : RgbaFloat
} with
    static member Description =
        VertexLayoutDescription([|
            VertexElementDescription("Position",
                VertexElementFormat.Float3,
                VertexElementSemantic.TextureCoordinate)
            VertexElementDescription("Color",
                VertexElementFormat.Float4,
                VertexElementSemantic.TextureCoordinate)
            |])

[<Struct>]
type PositionTextureVertex = {
    Position : Vector3
    TexCoord : Vector2
} with
    static member Description =
        VertexLayoutDescription([|
            VertexElementDescription("Position",
                VertexElementFormat.Float3,
                VertexElementSemantic.TextureCoordinate)
            VertexElementDescription("TexCoord",
                VertexElementFormat.Float2,
                VertexElementSemantic.TextureCoordinate)
            |])

[<Struct>]
type PositionTextureColorVertex = {
    Position : Vector3
    TexCoord : Vector2
    Color : RgbaFloat
} with
    static member Description =
        VertexLayoutDescription([|
            VertexElementDescription("Position",
                VertexElementFormat.Float3,
                VertexElementSemantic.TextureCoordinate)
            VertexElementDescription("TexCoord",
                VertexElementFormat.Float2,
                VertexElementSemantic.TextureCoordinate)
            VertexElementDescription("Color",
                VertexElementFormat.Float4,
                VertexElementSemantic.TextureCoordinate)
            |])

[<Struct>]
type PositionTextureDualColorVertex = {
    Position : Vector3
    TexCoord : Vector2
    Foreground : RgbaFloat
    Background : RgbaFloat
    } with
    static member Description = 
        VertexLayoutDescription([|
            VertexElementDescription("Position",
                VertexElementFormat.Float3,
                VertexElementSemantic.TextureCoordinate)
            VertexElementDescription("TexCoord",
                VertexElementFormat.Float2,
                VertexElementSemantic.TextureCoordinate)
            VertexElementDescription("Foreground",
                VertexElementFormat.Float4,
                VertexElementSemantic.TextureCoordinate)
            VertexElementDescription("Background",
                VertexElementFormat.Float4,
                VertexElementSemantic.TextureCoordinate)
            |])

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
type VertexSpanExtensions =        
    [<Extension>]
    static member GetTriangleSpan<'a>(w : IBufferWriter<'a>, spriteCount) = 
        let vertexCount = spriteCount * 3
        w.GetSpan(vertexCount).Slice(0, vertexCount)

    [<Extension>]
    static member GetQuadSpan<'a>(w : IBufferWriter<'a>, spriteCount) = 
        let vertexCount = spriteCount * 4
        w.GetSpan(vertexCount).Slice(0, vertexCount)

    // Vector2 triangles
    
    [<Extension>]
    static member DrawTriangle(span : Span<Vector2>, p0 : Vector2, p1 : Vector2, p2 : Vector2) = 
        span.[0] <- p0
        span.[1] <- p1
        span.[2] <- p2
    
    [<Extension>]
    static member DrawTriangle(w : IBufferWriter<Vector2>, p0 : Vector2, p1 : Vector2, p2 : Vector2) = 
        let span = w.GetTriangleSpan(1)
        span.DrawTriangle(p0, p1, p2)
        w.Advance(span.Length)

    // Vector2 quads

    [<Extension>]
    static member DrawQuad(span : Span<Vector2>, rect : Range2) = 
        span.[0] <- Vector2(rect.Min.X, rect.Min.Y)
        span.[1] <- Vector2(rect.Max.X, rect.Min.Y)
        span.[2] <- Vector2(rect.Max.X, rect.Max.Y)
        span.[3] <- Vector2(rect.Min.X, rect.Max.Y)

    [<Extension>]
    static member DrawQuad(w : IBufferWriter<Vector2>, rect : Range2) = 
        let span = w.GetQuadSpan(1)
        span.DrawQuad(rect)
        w.Advance(span.Length)

    // PositionColorVertex triangles

    [<Extension>]
    static member DrawTriangle(span : Span<PositionColorVertex>, p0 : Vector2, p1 : Vector2, p2 : Vector2, color) =
        span.[0] <- { Position = Vector3(p0.X, p0.Y, 0.0f); Color = color }
        span.[1] <- { Position = Vector3(p1.X, p1.Y, 0.0f); Color = color }
        span.[2] <- { Position = Vector3(p2.X, p2.Y, 0.0f); Color = color }

    [<Extension>]
    static member DrawTriangle(w : IBufferWriter<PositionColorVertex>, p0, p1, p2, color) = 
        let span = w.GetTriangleSpan(1)
        span.DrawTriangle(p0, p1, p2, color)
        w.Advance(span.Length)

    // PositionColorVertex quads

    [<Extension>]
    static member DrawQuad(span : Span<PositionColorVertex>, rect : Range2, color) =
        span.[0] <- { Position = Vector3(rect.Min.X, rect.Min.Y, 0.0f); Color = color }
        span.[1] <- { Position = Vector3(rect.Max.X, rect.Min.Y, 0.0f); Color = color }
        span.[2] <- { Position = Vector3(rect.Max.X, rect.Max.Y, 0.0f); Color = color }
        span.[3] <- { Position = Vector3(rect.Min.X, rect.Max.Y, 0.0f); Color = color }

    [<Extension>]
    static member DrawQuad(w : IBufferWriter<PositionColorVertex>, rect : Range2, color) = 
        let span = w.GetQuadSpan(1)
        span.DrawQuad(rect, color)
        w.Advance(span.Length)
    
    // PositionTextureColorVertex triangles

    [<Extension>]
    static member DrawTriangle(span : Span<PositionTextureColorVertex>, p0 : Vector2, p1 : Vector2, p2 : Vector2, tc0, tc1, tc2, color) =
        span.[0] <- { Position = Vector3(p0.X, p0.Y, 0.0f); TexCoord = tc0; Color = color }
        span.[1] <- { Position = Vector3(p1.X, p1.Y, 0.0f); TexCoord = tc1; Color = color }
        span.[2] <- { Position = Vector3(p2.X, p2.Y, 0.0f); TexCoord = tc2; Color = color }

    [<Extension>]
    static member DrawTriangle(w : IBufferWriter<PositionTextureColorVertex>, p0, p1, p2, tc0, tc1, tc2, color) = 
        let span = w.GetTriangleSpan(1)
        span.DrawTriangle(p0, p1, p2, tc0, tc1, tc2, color)
        w.Advance(span.Length)

    // PositionTextureColorVertex quads

    [<Extension>]
    static member DrawQuad(span : Span<PositionTextureColorVertex>, rect : Range2, texBounds : Range2, color) =
        span.[0] <- { Position = Vector3(rect.Min.X, rect.Min.Y, 0.0f); TexCoord = Vector2(texBounds.X.Min, texBounds.Y.Min); Color = color }
        span.[1] <- { Position = Vector3(rect.Max.X, rect.Min.Y, 0.0f); TexCoord = Vector2(texBounds.X.Max, texBounds.Y.Min); Color = color }
        span.[2] <- { Position = Vector3(rect.Max.X, rect.Max.Y, 0.0f); TexCoord = Vector2(texBounds.X.Max, texBounds.Y.Max); Color = color }
        span.[3] <- { Position = Vector3(rect.Min.X, rect.Max.Y, 0.0f); TexCoord = Vector2(texBounds.X.Min, texBounds.Y.Max); Color = color }
    
    [<Extension>]
    static member DrawQuad(span : Span<PositionTextureColorVertex>, 
            center : Vector2, 
            rotation : Vector2, 
            size : Vector2, 
            texBounds : Range2, 
            color : RgbaFloat) = 
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
            Color = color
            }
        span.[1] <- {
            Position = Vector3(p10.X, p10.Y, 0.0f)
            TexCoord = Vector2(t11.X, t00.Y)
            Color = color
            }
        span.[2] <- {
            Position = Vector3(p11.X, p11.Y, 0.0f)
            TexCoord = Vector2(t11.X, t11.Y)
            Color = color
            }
        span.[3] <- {
            Position = Vector3(p01.X, p01.Y, 0.0f)
            TexCoord = Vector2(t00.X, t11.Y)
            Color = color
            }

    [<Extension>]
    static member DrawQuad(w : IBufferWriter<PositionTextureColorVertex>, rect : Range2, texBounds : Range2, color) = 
        let span = w.GetQuadSpan(1)
        span.DrawQuad(rect, texBounds, color)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawQuad(w : IBufferWriter<PositionTextureColorVertex>, 
            center : Vector2, 
            rotation : Vector2, 
            size : Vector2, 
            texBounds : Range2, 
            color : RgbaFloat) = 
        let span = w.GetQuadSpan(1)
        span.DrawQuad(center, rotation, size, texBounds, color)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawNinePatchRect(w : IBufferWriter<PositionTextureColorVertex>, 
            rect : Range2i,
            atlasSize : Vector2i,
            texBounds : Range2i,
            margin0 : Vector2i,
            margin1 : Vector2i,
            color : RgbaFloat) = 
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
        let span = w.GetQuadSpan(9)
        for y = 0 to 2 do
            for x = 0 to 2 do
                let t0 = Vector2(tx.[x], ty.[y]) * atlasScale
                let t1 = Vector2(tx.[x + 1], ty.[y + 1]) * atlasScale
                let p0 = Vector2(px.[x], py.[y])
                let p1 = Vector2(px.[x + 1], py.[y + 1])
                let i = y * 3 + x
                let span = span.Slice(i * 4)
                span.DrawQuad(
                    Range2(p0, p1),
                    Range2(t0, t1),
                    color)
        w.Advance(span.Length)

    // PositionTextureDualColorVertex triangles

    [<Extension>]
    static member DrawTriangle(span : Span<PositionTextureDualColorVertex>, p0 : Vector2, p1 : Vector2, p2 : Vector2, tc0, tc1, tc2, fg, bg) =
        span.[0] <- { Position = Vector3(p0.X, p0.Y, 0.0f); TexCoord = tc0; Foreground = fg; Background = bg }
        span.[1] <- { Position = Vector3(p1.X, p1.Y, 0.0f); TexCoord = tc1; Foreground = fg; Background = bg }
        span.[2] <- { Position = Vector3(p2.X, p2.Y, 0.0f); TexCoord = tc2; Foreground = fg; Background = bg }

    [<Extension>]
    static member DrawTriangle(w : IBufferWriter<PositionTextureDualColorVertex>, p0, p1, p2, tc0, tc1, tc2, fg, bg) = 
        let span = w.GetTriangleSpan(1)
        span.DrawTriangle(p0, p1, p2, tc0, tc1, tc2, fg, bg)
        w.Advance(span.Length)

    // PositionTextureDualColorVertex quads
    
    [<Extension>]
    static member DrawQuad(span : Span<PositionTextureDualColorVertex>, rect : Range2, texBounds : Range2, fg, bg) =
        span.[0] <- { Position = Vector3(rect.Min.X, rect.Min.Y, 0.0f); TexCoord = Vector2(texBounds.X.Min, texBounds.Y.Min); Foreground = fg; Background = bg }
        span.[1] <- { Position = Vector3(rect.Max.X, rect.Min.Y, 0.0f); TexCoord = Vector2(texBounds.X.Max, texBounds.Y.Min); Foreground = fg; Background = bg }
        span.[2] <- { Position = Vector3(rect.Max.X, rect.Max.Y, 0.0f); TexCoord = Vector2(texBounds.X.Max, texBounds.Y.Max); Foreground = fg; Background = bg }
        span.[3] <- { Position = Vector3(rect.Min.X, rect.Max.Y, 0.0f); TexCoord = Vector2(texBounds.X.Min, texBounds.Y.Max); Foreground = fg; Background = bg }

    [<Extension>]
    static member DrawQuad(w : IBufferWriter<PositionTextureDualColorVertex>, rect : Range2, texBounds : Range2, fg, bg) = 
        let span = w.GetQuadSpan(1)
        span.DrawQuad(rect, texBounds, fg, bg)
        w.Advance(span.Length)
    