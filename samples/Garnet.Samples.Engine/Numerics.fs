namespace Garnet.Samples.Engine

open System
open System.Numerics

[<AutoOpen>]
module MathF =
    type MathF with
        static member inline Lerp(min, max, t : float32) =
            min * (1.0f - t) + max * t

        static member inline Clamp(s0 : float32, s1 : float32, s : float32) =
            s |> max s0 |> min s1
            
        static member inline Clamp01(x) =
            MathF.Clamp(0.0f, 1.0f, x)

        static member inline LinearStep(s0, s1, s) =
            let length = s1 - s0
            if abs length < 1e-7f then 0.0f
            else MathF.Clamp01((s - s0) / length)

        static member inline SmoothStep(s0, s1, s) =
            let x = MathF.LinearStep(s0, s1, s)
            x * x * (3.0f - 2.0f * x)

[<AutoOpen>]
module Matrix4x4 =
    type Matrix4x4 with
        member m.GetInverseOrIdentity() =
            let mutable mInv = Matrix4x4.Identity
            if Matrix4x4.Invert(m, &mInv) then mInv else Matrix4x4.Identity
