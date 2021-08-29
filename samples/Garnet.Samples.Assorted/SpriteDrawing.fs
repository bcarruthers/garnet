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

    let spritePipeline = {
        Blend = Blend.Alpha
        Filtering = Filtering.Point
        ShaderSet = colorTextureShaderSet
        Texture = "textures/multicolor-square.png"
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
                let size = Vector2(80.0f, 40.0f)
                // Identical quads
                verts.DrawQuad(
                    Range2.Centered(Vector2(50.0f, 50.0f), size),
                    Range2.ZeroToOne,
                    RgbaFloat.White)
                verts.DrawQuad {
                    ColorTextureSprite.Default with
                        Center = Vector2(150.0f, 50.0f)
                        Size = size
                        }
                // Rotated - Since we're using pixel coords, positive rotation is clockwise
                verts.DrawQuad {
                    ColorTextureSprite.Default with
                        Center = Vector2(250.0f, 50.0f)
                        Size = size
                        Rotation = Vector2.FromDegrees(90.0f)
                        }
            ]

