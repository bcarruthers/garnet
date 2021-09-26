module Garnet.Samples.Assorted.TextDrawing

open Veldrid
open Garnet.Composition
open Garnet.Graphics
open Garnet.Numerics

module Resources =
    let font = "fonts/pixel-operator-regular-12.font.json"
    let fontTexture = "textures/pixel-operator-regular-12.png"
    
    let textureShaderSet : ShaderSetDescriptor<PositionTextureColorVertex> = {
        VertexShader = "shaders/texture-color.vert"
        FragmentShader = "shaders/texture-color.frag"
        }

    let colorShaderSet : ShaderSetDescriptor<PositionColorVertex> = {
        VertexShader = "shaders/color.vert"
        FragmentShader = "shaders/color.frag"
        }

    let textPipeline = {
        Blend = Blend.Alpha
        // Use point filtering since we plan to scale the font and want a pixelated appearance
        Filtering = Filtering.Point
        ShaderSet = textureShaderSet
        Texture = "textures/pixel-operator-regular-12.png"
        }

    let panelPipeline = {
        Blend = Blend.Alpha
        Filtering = Filtering.Linear
        ShaderSet = colorShaderSet
        Texture = ""
        }

    let textLayer = {
        LayerId = 1
        CameraId = 0
        Primitive = Quad
        FlushMode = FlushOnDraw
        Pipeline = textPipeline
        }

    let panelLayer = {
        LayerId = 0
        CameraId = 0
        Primitive = Quad
        FlushMode = FlushOnDraw
        Pipeline = panelPipeline
        }

let run() =
    Container.Run <| fun c ->
        c.Set {
            WindowSettings.Default with Width = 640; Height = 480 
        }
        Disposable.Create [
            c.AddDefaultSystems()
            c.AddPixelCoordinateCamera(0)
            c.AddEscapeToClose()
            c.On<Draw> <| fun e ->
                let font = c.LoadJsonFont(Resources.font, Resources.fontTexture)
                let sprites = c.Get<SpriteRenderer>()
                let textVerts = sprites.GetVertices(Resources.textLayer)
                let panelVerts = sprites.GetVertices(Resources.panelLayer)
                let baseBlock = {
                    TextBlock.Default with
                        Scale = 3
                        Bounds = Range2i.Sized(Vector2i.Zero, e.ViewSize)
                        }
                // Draw text in the corners
                textVerts.DrawText(font, {
                    baseBlock with
                        Text = "Upper left"
                        Align = Align.Left
                        Valign = Valign.Top
                        })
                textVerts.DrawText(font, {
                    baseBlock with
                        Text = "Bottom right"
                        Align = Align.Right
                        Valign = Valign.Bottom
                        })
                // Draw text in the center within a panel
                let block = {
                    baseBlock with
                        Text = "Multiple Lines\nCenter"
                        Align = Align.Center
                        Valign = Valign.Center
                        }
                let bounds = font.Measure(block).ToRange2()
                textVerts.DrawText(font, block)
                panelVerts.DrawQuad(bounds, RgbaFloat.Red.MultiplyAlpha(0.5f))
                // Test wrapping
                let scale = 2
                let text = "Title\n\nLine 1\nLine 2 should wrap to next line\nLine 3"
                let size = font.Measure(text) * scale
                let bounds = Range2i.Sized(Vector2i(0, e.ViewSize.Y - size.Y), Vector2i(size.X - 200, size.Y))
                let block = {
                    baseBlock with
                        Scale = scale
                        Bounds = bounds
                        Text = text
                        Align = Align.Left
                        Valign = Valign.Center
                        Wrapping = TextWrapping.NoWrap
                        }
                textVerts.DrawText(font, block)
                panelVerts.DrawQuad(bounds.ToRange2(), RgbaFloat.Red.MultiplyAlpha(0.5f))
            ]

