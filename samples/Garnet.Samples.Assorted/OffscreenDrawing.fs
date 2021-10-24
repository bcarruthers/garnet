module Garnet.Samples.Assorted.OffscreenDrawing

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

    let lightPipeline = {
        Blend = Blend.Alpha
        Filtering = Filtering.Point
        ShaderSet = colorTextureShaderSet
        Texture = "textures/hex.png"
        }

    let lightLayer = {
        LayerId = 0
        CameraId = 0
        Primitive = Quad
        FlushMode = FlushOnDraw
        Pipeline = lightPipeline
        }

type MainScene(ren : SpriteRenderer) =
    member c.Renderer = ren
    
type LightScene(ren : SpriteRenderer) =
    member c.Renderer = ren
    
type Container with
    member c.AddSpriteLightingDrawing() =
        let device = c.Get<GraphicsDevice>()
        let shaders = c.Get<ShaderSetCache>()
        let cache = c.Get<ResourceCache>()
        let lightTarget =
            let blend = BlendStateDescription(AttachmentStates = [|
                // Multiply destination (main scene) by source (light map)
                BlendAttachmentDescription(
                    BlendEnabled = true,
                    SourceColorFactor = BlendFactor.Zero,
                    DestinationColorFactor = BlendFactor.SourceColor,
                    ColorFunction = BlendFunction.Add,
                    SourceAlphaFactor = BlendFactor.Zero,
                    DestinationAlphaFactor = BlendFactor.SourceAlpha,
                    AlphaFunction = BlendFunction.Add
                    )
                |])
            let shaderSet = shaders.GetOrCreate(device, Resources.colorTextureShaderSet.Untyped, cache)
            let target = new RenderTarget(device, shaderSet, Filtering.Linear, blend)
            target.Background <- RgbaFloat.Clear
            target
        let mainSprites = new SpriteRenderer(device, shaders, cache)
        let lightSprites = new SpriteRenderer(device, shaders, cache)
        c.Set(MainScene(mainSprites))
        c.Set(LightScene(lightSprites))
        Disposable.Create [
            mainSprites :> IDisposable
            lightSprites :> IDisposable
            lightTarget :> IDisposable
            c.On<Draw> <| fun e ->
                lightTarget.Width <- e.ViewSize.X
                lightTarget.Height <- e.ViewSize.Y
            c.On<PushDrawCommands> <| fun _ ->
                let context = c.Get<RenderContext>()
                let cameras = c.Get<CameraSet>()
                // First draw scene normally
                mainSprites.Draw(context, cameras)
                // Next draw lights to offscreen buffer, then draw to main buffer
                // (with multiply blending)
                lightTarget.BeginDraw(context)
                lightSprites.Draw(context, cameras)
                lightTarget.EndDraw(context)
            ]

let run() =
    Container.Run <| fun c ->
        c.Set {
            WindowSettings.Default with
                Background = RgbaFloat.Blue.MultiplyRgb(0.1f)
            }
        Disposable.Create [
            c.AddDefaultSystems()
            c.AddPixelCoordinateCamera(0)
            c.AddEscapeToClose()
            c.AddSpriteLightingDrawing()
            c.On<Draw> <| fun e ->
                let rect = Range2.Sized(Vector2.Zero, e.ViewSize.ToVector2()) 
                // Draw sprites
                let sprites = c.Get<MainScene>().Renderer
                let verts = sprites.GetVertices(Resources.spriteLayer)
                verts.DrawQuad(rect, Range2.ZeroToOne, RgbaFloat.White)
                // Draw lights
                let lights = c.Get<LightScene>().Renderer
                let verts = lights.GetVertices(Resources.lightLayer)
                verts.DrawQuad(rect, Range2.ZeroToOne, RgbaFloat.White)
            ]
