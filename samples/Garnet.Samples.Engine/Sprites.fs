namespace Garnet.Engine

open System
open System.Buffers
open System.Numerics
open System.Runtime.CompilerServices
open Veldrid

[<Extension>]
type SpriteVertexSpanExtensions =
    [<Extension>]
    static member DrawSprite(span : Span<PositionTextureDualColorVertex>, 
        center : Vector2, 
        rotation : Vector2, 
        size : Vector2, 
        t0 : Vector2, 
        t1 : Vector2, 
        fg : RgbaFloat, 
        bg : RgbaFloat) = 
        let dxDir = rotation
        let dyDir = dxDir.GetPerpendicular()
        let dx = dxDir * size
        let dy = dyDir * size
        let p0 = center - (dx + dy) * 0.5f
        let p1 = p0 + dx
        let p2 = p1 + dy
        let p3 = p2 - dx
        span.[0] <- {
            Position = Vector3(p0.X, p0.Y, 0.0f)
            TexCoord = Vector2(t0.X, t0.Y)
            Foreground = fg
            Background = bg
            }
        span.[1] <- {
            Position = Vector3(p1.X, p1.Y, 0.0f)
            TexCoord = Vector2(t1.X, t0.Y)
            Foreground = fg
            Background = bg
            }
        span.[2] <- {
            Position = Vector3(p2.X, p2.Y, 0.0f)
            TexCoord = Vector2(t1.X, t1.Y)
            Foreground = fg
            Background = bg
            }
        span.[3] <- {
            Position = Vector3(p3.X, p3.Y, 0.0f)
            TexCoord = Vector2(t0.X, t1.Y)
            Foreground = fg
            Background = bg
            }

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
        t0 : Vector2, 
        t1 : Vector2, 
        fg : RgbaFloat, 
        bg : RgbaFloat) = 
        let span = w.GetSpriteSpan(1)
        span.DrawSprite(center, rotation, size, t0, t1, fg, bg)
        w.Advance(span.Length)
