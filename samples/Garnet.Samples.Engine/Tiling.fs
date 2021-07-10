namespace Garnet.Engine

open System
open System.Buffers
open System.Numerics
open System.Runtime.CompilerServices

[<Extension>]
type TileVertexSpanExtensions =
    [<Extension>]
    static member DrawTile(span : Span<PositionTextureDualColorVertex>, x0, y0, x1, y1, tx, ty, fg, bg) = 
        let tx0 = float32 tx
        let ty0 = float32 ty
        let tx1 = tx0 + 1.0f
        let ty1 = ty0 + 1.0f
        let px0 = float32 x0
        let py0 = float32 y0
        let px1 = float32 x1
        let py1 = float32 y1
        span.[0] <- {
            Position = Vector3(px0, py0, 0.0f)
            TexCoord = Vector2(tx0, ty0)
            Foreground = fg
            Background = bg
            }
        span.[1] <- {
            Position = Vector3(px1, py0, 0.0f)
            TexCoord = Vector2(tx1, ty0)
            Foreground = fg
            Background = bg
            }
        span.[2] <- {
            Position = Vector3(px1, py1, 0.0f)
            TexCoord = Vector2(tx1, ty1)
            Foreground = fg
            Background = bg
            }
        span.[3] <- {
            Position = Vector3(px0, py1, 0.0f)
            TexCoord = Vector2(tx0, ty1)
            Foreground = fg
            Background = bg
            }

    [<Extension>]
    static member DrawTile(span : Span<PositionTextureDualColorVertex>, x, y, tx, ty, fg, bg) = 
        span.DrawTile(x, y, x + 1, y + 1, tx, ty, fg, bg)

    [<Extension>]
    static member DrawTile(span : Span<PositionTextureDualColorVertex>, x0, y0, x1, y1, ch : char, fg, bg) = 
        let tileId = int ch
        let tx = tileId &&& 0xf
        let ty = tileId >>> 4
        span.DrawTile(x0, y0, x1, y1, tx, ty, fg, bg)

    [<Extension>]
    static member DrawTile(span : Span<PositionTextureDualColorVertex>, x, y, ch : char, fg, bg) = 
        span.DrawTile(x, y, x + 1, y + 1, ch, fg, bg)

[<Extension>]
type TileVertexBufferWriterExtensions =
    [<Extension>]
    static member GetTileSpan(w : IBufferWriter<PositionTextureDualColorVertex>, tileCount) = 
        let vertexCount = tileCount * 4
        w.GetSpan(vertexCount).Slice(0, vertexCount)

    [<Extension>]
    static member DrawTile(w : IBufferWriter<PositionTextureDualColorVertex>, x, y, ch, fg, bg) = 
        let span = w.GetTileSpan(1)
        span.DrawTile(x, y, ch, fg, bg)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawTileText(w : IBufferWriter<PositionTextureDualColorVertex>, x0, y0, str : string, fg, bg) = 
        let span = w.GetTileSpan(str.Length)
        for i = 0 to str.Length - 1 do
            span.Slice(i * 4).DrawTile(x0 + i, y0, str.[i], fg, bg)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawTileRect(w : IBufferWriter<PositionTextureDualColorVertex>, x0, y0, x1, y1, fg) =
        let span = w.GetTileSpan(1)
        span.DrawTile(x0, y0, x1, y1, '\u00db', fg, fg)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawTileTextBorder(w : IBufferWriter<PositionTextureDualColorVertex>, x0, y0, x1, y1, fg, bg) = 
        let x1 = x1 - 1
        let y1 = y1 - 1
        w.DrawTile(x0, y0, '\u00da', fg, bg)
        w.DrawTile(x1, y0, '\u00bf', fg, bg)
        w.DrawTile(x0, y1, '\u00c0', fg, bg)
        w.DrawTile(x1, y1, '\u00d9', fg, bg)
        for x = x0 + 1 to x1 - 1 do
            w.DrawTile(x, y0, '\u00c4', fg, bg)
            w.DrawTile(x, y1, '\u00c4', fg, bg)
        for y = y0 + 1 to y1 - 1 do
            w.DrawTile(x0, y, '\u00b3', fg, bg)
            w.DrawTile(x1, y, '\u00b3', fg, bg)
