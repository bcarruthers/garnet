namespace Garnet.Samples.Trixel

open System
open System.Buffers
open System.Runtime.CompilerServices
open System.Numerics
open Veldrid
open Garnet.Numerics
open Garnet.Graphics

[<Extension>]
type VertexSpanExtensions =
    [<Extension>]
    static member DrawLine(span : Span<PositionTextureDualColorVertex>, 
            p0 : Vector2, 
            p1 : Vector2, 
            thickness : float32, 
            color : RgbaFloat) = 
        let delta = p1 - p0
        let length = delta.Length()
        let dir = if length < 1e-5f then Vector2.Zero else delta / length
        let rotation = dir.GetPerpendicular()
        let center = (p0 + p1) * 0.5f
        span.DrawSprite(center, rotation, Vector2(thickness, length), Range2.ZeroToOne, color, color)

    [<Extension>]
    static member DrawAxialLine(span : Span<PositionTextureDualColorVertex>, 
            p0 : Vector2i, 
            p1 : Vector2i, 
            thickness, 
            color : RgbaFloat) =
        let ep0 = TriCoords.vertexToEuc p0
        let ep1 = TriCoords.vertexToEuc p1
        span.DrawLine(ep0, ep1, thickness, color)

[<Extension>]
type VertexBufferWriterExtensions =
    [<Extension>]
    static member DrawGridLines(w : IBufferWriter<PositionTextureDualColorVertex>, spacing, thickness, color, r) = 
        let extent = r / spacing
        let count = extent * 2 + 1
        let span = w.GetSpriteSpan(count * 3)
        for i = -extent to extent do
            let p = i * spacing
            let di = (i + extent) * 3 * 4
            let p0 = Vector2i(-r, p)
            let p1 = Vector2i(r, p)
            span.Slice(di + 0).DrawAxialLine(p0, p1, thickness, color)
            let p0 = Vector2i(p, -r)
            let p1 = Vector2i(p, r)
            span.Slice(di + 4).DrawAxialLine(p0, p1, thickness, color)
            let p0 = Vector2i(-r, p + r)
            let p1 = Vector2i(r, p - r)
            span.Slice(di + 8).DrawAxialLine(p0, p1, thickness, color)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawGridLines(w : IBufferWriter<PositionTextureDualColorVertex>,
            majorSpacing,
            majorThickness,
            majorColor,
            minorThickness,
            minorColor,
            r) =
        w.DrawGridLines(majorSpacing, majorThickness, majorColor, r)
        w.DrawGridLines(1, minorThickness, minorColor, r)

    [<Extension>]
    static member DrawGridLines(w : IBufferWriter<PositionTextureDualColorVertex>) =
        let minorColor = RgbaFloat(0.8f, 0.6f, 0.1f, 0.2f)
        let majorColor = RgbaFloat(0.8f, 0.6f, 0.1f, 0.3f)
        let majorSpacing = 6
        let majorThickness = 0.1f
        let minorThickness = 0.05f
        w.DrawGridLines(majorSpacing, majorThickness, majorColor, minorThickness, minorColor, 100)
    
    [<Extension>]
    static member DrawGridCells(w : IBufferWriter<PositionTextureDualColorVertex>, state) =
        let cellMargin = 0.1f
        let vertexCount = state.Cells.Count * 3
        let span = w.GetSpan(vertexCount)
        let mutable i = 0
        for kvp in state.Cells do
            let p = Vector2i(kvp.Key.X, kvp.Key.Y)
            let tri = TriPositions.fromTriCell p
            let centroid = (tri.P0 + tri.P1 + tri.P2) / 3.0f
            let p0 = Vector2.Lerp(tri.P0, centroid, cellMargin)
            let p1 = Vector2.Lerp(tri.P1, centroid, cellMargin)
            let p2 = Vector2.Lerp(tri.P2, centroid, cellMargin)
            let color = kvp.Value.ToRgbaFloat()
            let verts = span.Slice(i * 3)
            verts.[0] <- {
                Position = Vector3(p0.X, p0.Y, 0.0f)
                TexCoord = Vector2(0.0f, 0.0f)
                Foreground = color
                Background = color
                }
            verts.[1] <- {
                Position = Vector3(p1.X, p1.Y, 0.0f)
                TexCoord = Vector2(1.0f, 0.0f)
                Foreground = color
                Background = color
                }
            verts.[2] <- {
                Position = Vector3(p2.X, p2.Y, 0.0f)
                TexCoord = Vector2(0.0f, 1.0f)
                Foreground = color
                Background = color
                }
            i <- i + 1
        w.Advance(vertexCount)
