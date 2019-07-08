module Garnet.Samples.FrameworkTypes

open Garnet.Samples.Numerics

[<Struct>]
type SpriteType =
    | Triangle
    | Hex
    | Square

[<Struct>]
type Rgba = {
    red : float32
    green : float32
    blue : float32
    alpha : float32
}

[<Struct>]
type Sprite = {
    radians : float32
    spriteType : SpriteType
    center : Vec2f
    size : float32
    color : Rgba
}

type Draw = struct end
type Start = struct end
type Reset = struct end

[<Struct>]
type Update = {
    deltaTime : float32
    }

[<Struct>]
type ViewSize = {
    viewSize : Vec2f
}

[<Struct>]
type Zoom = {
    zoom : float32
}

module Rgba =
    let init r g b a = { 
        red = r
        green = g
        blue = b
        alpha = a 
        }

    let rgb r g b = init r g b 1.0f

    let multiplyAlpha c color =
        { color with alpha = color.alpha * c }
