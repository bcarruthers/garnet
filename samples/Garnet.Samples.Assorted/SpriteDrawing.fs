module Garnet.Samples.Assorted.SpriteDrawing

open System
open System.Numerics
open Garnet.Composition
open Garnet.Graphics
open Garnet.Numerics
open Veldrid

module Resources =
    let colorTextureShaderSet : ShaderSetDescriptor<PositionTextureColorVertex> = {
        VertexShader = "shaders/texture-color.vert"
        FragmentShader = "shaders/texture-color.frag"
        }

    let colorShaderSet : ShaderSetDescriptor<PositionColorVertex> = {
        VertexShader = "shaders/color.vert"
        FragmentShader = "shaders/color.frag"
        }

    let spritePipeline = {
        Blend = Blend.Alpha
        Filtering = Filtering.Linear
        ShaderSet = colorShaderSet
        Texture = ""
        }

    let spriteLayer = {
        LayerId = 0
        CameraId = 0
        Primitive = Quad
        FlushMode = FlushOnDraw
        Pipeline = spritePipeline
        }

let run() =
    Container.Run <| fun c ->
        c.SetValue {
            WindowSettings.Default with
                Background = RgbaFloat.Blue.MultiplyRgb(0.1f)
            }
        Disposable.Create [
            c.AddDefaultSystems()
            c.AddPixelCoordinateCamera(0)
            c.AddEscapeToClose()
            c.On<Draw> <| fun _ ->
                let sprites = c.GetValue<SpriteRenderer>()
                let verts = sprites.GetVertices(Resources.spriteLayer)
                let rect = Range2.Centered(Vector2(150.0f, 80.0f), Vector2(200.0f, 100.0f))
                verts.DrawQuad(rect, RgbaFloat.White)
            ]

