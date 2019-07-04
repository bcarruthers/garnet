module Garnet.Samples.Common.Numerics

[<Struct>]
type Vec2f = {
    x : float32
    y : float32
}

module Scalar =
    let tolerance = 1e-9f

    let clamp (s0 : float32) (s1 : float32) (s : float32) =
        s |> max s0 |> min s1

    let linearStep s0 s1 s =
        let length = s1 - s0
        if abs length < tolerance then 0.0f
        else clamp 0.0f 1.0f ((s - s0) / length)

    let smoothStep s0 s1 s =
        let x = linearStep s0 s1 s
        x * x * (3.0f - 2.0f * x)

module Vec2f =
    let init x y = {
        x = x
        y = y
    }

    let zero = init 0.0f 0.0f

    let add a b = {
        x = a.x + b.x
        y = a.y + b.y
    }
    
    let subtract a b = {
        x = a.x - b.x
        y = a.y - b.y
    }

    let multiply c v = {
        x = v.x * c
        y = v.y * c
    }

    let lengthSquared v =
        v.x * v.x + v.y * v.y

    let length v = 
        sqrt (lengthSquared v)

    let divideOrZero c v =
        if abs c > Scalar.tolerance then multiply (1.0f / c) v else init 0.0f 0.0f

    let normalizeOrZero v =
        divideOrZero (length v) v

    let truncateOrZero maxLength v =
        let lengthSqr = lengthSquared v
        if lengthSqr <= maxLength * maxLength then v
        else normalizeOrZero v |> multiply maxLength

    let radians v =
        atan2 v.y v.x

    let fromRadians r =
        init (cos r) (sin r)

    let rotate a b =
        init (b.x * a.x - b.y * a.y) (b.x * a.y + b.y * a.x)
