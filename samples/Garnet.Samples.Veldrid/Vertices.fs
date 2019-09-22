namespace Garnet.Samples.Veldrid

open System.Numerics
open Veldrid

[<AutoOpen>]
module Vector2Extensions =
    type Vector2 with 
        member a.Rotate(b : Vector2) =
            Vector2(b.X * a.X - b.Y * a.Y, b.X * a.Y + b.Y * a.X)

[<Struct>]
type PositionColorVertex = {
    Position : Vector3
    Color : RgbaFloat
} with
    static member Descriptor : VertexDescriptor<PositionColorVertex> = {
        getPosition = fun v -> v.Position
        vertexElements = [
            VertexElement.Float3
            VertexElement.Float4
            ]
        }

[<Struct>]
type PositionTextureVertex = {
    Position : Vector3
    TexCoord : Vector2
} with
    static member Descriptor : VertexDescriptor<PositionTextureVertex> = {
        getPosition = fun v -> v.Position
        vertexElements = [
            VertexElement.Float3
            VertexElement.Float2
            ]
        }

[<Struct>]
type PositionTextureColorVertex = {
    Position : Vector3
    TexCoord : Vector2
    Color : Vector4
} with
    static member Descriptor : VertexDescriptor<PositionTextureColorVertex> = {
        getPosition = fun v -> v.Position
        vertexElements = [
            VertexElement.Float3
            VertexElement.Float2
            VertexElement.Float4
            ]
        }

type ColorMeshWriter(mesh : Mesh<_>) =
    let vertices = mesh.Vertices
    member c.Write(p0, p1, p2, color) =
        mesh.BeginTriangle()
        vertices.Add { Position = p0; Color = color }
        vertices.Add { Position = p1; Color = color }
        vertices.Add { Position = p2; Color = color }
    member c.Write(p0, p1, p2, p3, color) =
        mesh.BeginQuad()
        vertices.Add { Position = p0; Color = color }
        vertices.Add { Position = p1; Color = color }
        vertices.Add { Position = p2; Color = color }
        vertices.Add { Position = p3; Color = color }

type TextureColorMeshWriter(mesh : Mesh<_>) =
    let vertices = mesh.Vertices
    member c.Write(p0, p1, p2, tc0, tc1, tc2, color) =
        mesh.BeginTriangle()
        vertices.Add { Position = p0; TexCoord = tc0; Color = color }
        vertices.Add { Position = p1; TexCoord = tc1; Color = color }
        vertices.Add { Position = p2; TexCoord = tc2; Color = color }
    member c.Write(p0, p1, p2, p3, tc0, tc1, tc2, tc3, color) =
        mesh.BeginQuad()
        vertices.Add { Position = p0; TexCoord = tc0; Color = color }
        vertices.Add { Position = p1; TexCoord = tc1; Color = color }
        vertices.Add { Position = p2; TexCoord = tc2; Color = color }
        vertices.Add { Position = p3; TexCoord = tc3; Color = color }

[<AutoOpen>]
module CanvasExtensions =
    type Canvas with
        member c.Begin(layer : LayerDescriptor<_>) =
            ColorMeshWriter(c.GetLayer(layer).Mesh)

    type Canvas with
        member c.Begin(layer : LayerDescriptor<_>) =
            TextureColorMeshWriter(c.GetLayer(layer).Mesh)

    type TextureColorMeshWriter with
        member c.Write(p0 : Vector2, p1 : Vector2, p2 : Vector2, p3 : Vector2, tb : TextureBounds, color) =
            let p0 = Vector3(p0.X, p0.Y, 0.0f)
            let p1 = Vector3(p1.X, p1.Y, 0.0f)
            let p2 = Vector3(p2.X, p2.Y, 0.0f)
            let p3 = Vector3(p3.X, p3.Y, 0.0f)
            let tc0 = Vector2(tb.tx0, tb.ty0)
            let tc1 = Vector2(tb.tx1, tb.ty0)
            let tc2 = Vector2(tb.tx1, tb.ty1)
            let tc3 = Vector2(tb.tx0, tb.ty1)
            c.Write(p0, p1, p2, p3, tc0, tc1, tc2, tc3, color)

        member c.Write(center : Vector2, size : Vector2, tb : TextureBounds, color) =
            let rc0 = center - size * 0.5f
            let rc1 = center + size * 0.5f
            let p0 = Vector2(rc0.X, rc0.Y)
            let p1 = Vector2(rc1.X, rc0.Y)
            let p2 = Vector2(rc1.X, rc1.Y)
            let p3 = Vector2(rc0.X, rc1.Y)
            c.Write(p0, p1, p2, p3, tb, color)

        member c.Write(center : Vector2, size : Vector2, origin : Vector2, radians, tb : TextureBounds, color) =
            let s = size * 0.5f
            let offset = origin * s
            let p0 = Vector2(-s.X, -s.Y) - offset
            let p1 = Vector2(+s.X, -s.Y) - offset
            let p2 = Vector2(+s.X, +s.Y) - offset
            let p3 = Vector2(-s.X, +s.Y) - offset
            let r = Vector2(cos radians, sin radians)
            let offset = offset + center
            let p0 = p0.Rotate(r) + offset
            let p1 = p1.Rotate(r) + offset
            let p2 = p2.Rotate(r) + offset
            let p3 = p3.Rotate(r) + offset
            c.Write(p0, p1, p2, p3, tb, color)

        member c.Write(center : Vector2, size : Vector2, radians, tb : TextureBounds, color) =
            let dir = Vector2(cos radians, sin radians)
            let dx = dir * size
            let dy = Vector2(-dx.Y, dx.X)
            let p0 = center - dx * 0.5f - dy * 0.5f
            let p1 = p0 + dx
            let p2 = p1 + dy
            let p3 = p2 - dx
            c.Write(p0, p1, p2, p3, tb, color)
