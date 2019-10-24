namespace Garnet.Samples.Veldrid

open System.Numerics
open Garnet.Composition
open Garnet.Samples.Performance
open Garnet.Samples.FrameworkTypes

[<Struct>]
type Resize = {
    viewWidth : int
    viewHeight : int 
    }

module SpriteSystems =
    let registerFpsMonitor (c : Container) =
        let fps = c.GetInstance<FpsMonitor>()
        Disposable.list [
            c.On<Update> <| fun e ->
                fps.Update()                
            ]

    let registerUpdateViewport (c : Container) =
        let canvas = c.GetInstance<Canvas>()
        let viewport = canvas.GetViewport(0)
        Disposable.list [
            c.On<Reset> <| fun e ->
                viewport.View <- Matrix4x4.CreateScale(1.0f)
                viewport.ClearColor <- Vector4(0.0f, 0.1f, 0.2f, 1.0f)
            c.On<Resize> <| fun e ->
                viewport.Projection <- Matrix4x4.CreateOrthographic(float32 e.viewWidth, float32 e.viewHeight, -100.0f, 100.0f)
            ]

    let registerDrawSprites (c : Container) =
        let spriteLayer = {
            layerId = 0
            viewportId = 0
            vertexDescriptor = PositionTextureColorVertex.Descriptor
            vertexShader = "texture-color-transformed.vert"
            fragmentShader = "texture-color.frag"
            texture = "triangle.png"
            blendState = BlendState.Additive
            pickMask = 0u
            }
        let origin = Vector2(-0.3333f, 0.0f)
        let canvas = c.GetInstance<Canvas>()
        c.OnAll<Sprite> <| fun list ->
            let w = canvas.Begin(spriteLayer)
            for sprite in list do
                let p = Vector2(sprite.center.x, sprite.center.y)
                let color = Vector4(sprite.color.red, sprite.color.green, sprite.color.blue, sprite.color.alpha)
                w.Write(p, Vector2.One * sprite.size * 120.0f, origin, sprite.radians, TextureBounds.zeroToOne, color)

    let definition = 
        Registration.list [ 
            registerFpsMonitor
            registerUpdateViewport
            registerDrawSprites
            ]

module VeldridSystems =
    let definition = 
        Registration.combine [
            SpriteSystems.definition
            ]
