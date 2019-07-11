module Garnet.Samples.MonoGame.MonoGameWrapper

open System
open System.IO
open System.Collections.Generic
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Garnet.Ecs
open Garnet.Samples.Comparisons
open Garnet.Samples.Performance
open Garnet.Samples.Numerics
open Garnet.Samples.FrameworkTypes

module Conversion =
    let toVector v =
        Vector2(float32 v.x, float32 v.y)

    let toColor c =
        Color(
            float32 c.red,
            float32 c.green,
            float32 c.blue,
            float32 c.alpha)

type TextureEntry = {
    origin : Vector2
    texture : Texture2D
}

type TextureSet() =
    let lookup = Dictionary<_,_>()
    member c.Add(graphicsDevice, spriteType, path, origin : Vector2) =
        use fs = File.OpenRead(path)
        let tex = Texture2D.FromStream(graphicsDevice, fs)
        lookup.Add(spriteType, { 
            origin = Vector2(float32 tex.Width * origin.X, float32 tex.Height * origin.Y)
            texture = tex })
    member c.Draw(sb : SpriteBatch, sprite) =
        let entry = lookup.[sprite.spriteType]
        sb.Draw(
            texture = entry.texture,
            position = Conversion.toVector sprite.center,
            sourceRectangle = Nullable (Rectangle(0, 0, entry.texture.Width, entry.texture.Height)),
            color = Conversion.toColor sprite.color, 
            rotation = float32 sprite.radians, 
            origin = entry.origin,
            scale = Vector2(1.0f, 1.0f) * float32 sprite.size, 
            effects = SpriteEffects.None,
            layerDepth = 0.0f)

module SpriteSystems =
    let registerSpriteBatch (c : Container) =
        c.Register<SpriteBatch> <| fun () ->
            let game = c.GetInstance<Game>()
            new SpriteBatch(game.GraphicsDevice)            
        Disposable.empty

    let registerTextureSet (c : Container) =
        c.Register<TextureSet> <| fun () ->
            let assetsPath = "assets/"
            let game = c.GetInstance<Game>()
            let ts = TextureSet()
            ts.Add(game.GraphicsDevice, Triangle, assetsPath + "triangle.png", Vector2(0.3333f, 0.5f))
            ts.Add(game.GraphicsDevice, Hex, assetsPath + "hex.png", Vector2(0.3333f, 0.5f))
            ts.Add(game.GraphicsDevice, Square, assetsPath + "square.png", Vector2(0.5f, 0.5f))
            ts
        Disposable.empty

    let registerDrawSprites (c : Container) =
        c.OnAll<Sprite> <| fun list ->
            let zoom = 1.0f
            let vs = c.GetInstance<ref<ViewSize>>()
            let sb = c.GetInstance<SpriteBatch>()
            let tileSet = c.GetInstance<TextureSet>()
            let xf = 
                Matrix.CreateScale(float32 zoom) *
                Matrix.CreateTranslation(Vector3(vs.Value.viewSize.x * 0.5f, vs.Value.viewSize.y * 0.5f, 0.0f))
            sb.Begin(
                samplerState = SamplerState.LinearClamp, 
                blendState = BlendState.Additive,
                transformMatrix = Nullable xf)
            for sprite in list do
                tileSet.Draw(sb, sprite)
            sb.End()

    let definition = 
        Registration.list [ 
            registerSpriteBatch
            registerTextureSet
            registerDrawSprites
            ]

module MonoGameSystems =
    let definition = 
        Registration.combine [
            SpriteSystems.definition
            ]

type SampleGame(registration) as c =
    inherit Game()
    do c.Content.RootDirectory <- "."
    let graphics = new GraphicsDeviceManager(c)
    let fps = FpsMonitor()
    let world = Container()
    override c.Initialize() =
        let w = 800
        let h = 600
        // init world
        world.RegisterInstance<Game> c
        world.Register(fun () -> ref { viewSize = Vec2f.init (float32 w) (float32 h) })
        Registration.register MonoGameSystems.definition world |> ignore
        Registration.register registration world |> ignore
        world.Run <| Reset()
        // init mg
        graphics.SynchronizeWithVerticalRetrace <- false
        graphics.PreferredBackBufferWidth <- w
        graphics.PreferredBackBufferHeight <- h
        graphics.ApplyChanges()
        c.IsMouseVisible <- true
        // using variable rate to test perf
        c.IsFixedTimeStep <- false
    override c.Update gameTime = 
        fps.Update()
        world.Run { deltaTime = float32 gameTime.ElapsedGameTime.TotalSeconds }
    override c.Draw gameTime = 
        c.GraphicsDevice.Clear(Color(0.0f, 0.1f, 0.2f, 1.0f))
        world.Run <| Draw()
