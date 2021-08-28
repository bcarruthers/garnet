module Garnet.Samples.Assorted.TextDrawing

open Garnet.Composition
open Garnet.Graphics
open Garnet.Numerics

module Resources =
    let font = "fonts/pixel-operator-regular-12.font.json"
    let fontTexture = "textures/pixel-operator-regular-12.png"
    
    let shaderSet : ShaderSetDescriptor<PositionTextureColorVertex> = {
        VertexShader = "shaders/texture-color.vert"
        FragmentShader = "shaders/texture-color.frag"
        }

    let textPipeline = {
        Blend = Blend.Alpha
        // Use point filtering since we plan to scale the font and want a pixelated appearance
        Filtering = Filtering.Point
        ShaderSet = shaderSet
        Texture = "textures/pixel-operator-regular-12.png"
        }

    let textLayer = {
        LayerId = 0
        CameraId = 0
        Primitive = Quad
        FlushMode = FlushOnDraw
        Pipeline = textPipeline
        }

let run() =
    Container.Run <| fun c -> Disposable.Create [
        c.AddDefaultSystems()
        c.AddPixelCoordinateCamera(0)
        c.AddEscapeToClose()
        c.On<Draw> <| fun e ->
            let font = c.LoadJsonFont(Resources.font, Resources.fontTexture)
            let verts = c.GetValue<SpriteRenderer>().GetVertices(Resources.textLayer)
            let block = {
                TextBlock.Default with
                    Scale = 4
                    Bounds = Range2i.Sized(Vector2i.Zero, e.ViewSize)
                    }
            verts.DrawText(font, {
                block with
                    Text = "Upper left"
                    Align = Align.Left
                    Valign = Valign.Top
                    })
            verts.DrawText(font, {
                block with
                    Text = "Multiline\nCenter"
                    Align = Align.Center
                    Valign = Valign.Center
                    })
            verts.DrawText(font, {
                block with
                    Text = "Bottom right"
                    Align = Align.Right
                    Valign = Valign.Bottom
                    })
        ]

