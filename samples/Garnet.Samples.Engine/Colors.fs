namespace Garnet.Samples.Engine

open System
open Veldrid

[<Struct>]
type HsvaFloat =
    val H : float32
    val S : float32
    val V : float32
    val A : float32
    new(h, s, v, a) = { H = h; S = s; V = v; A = a; }
    new(c : RgbaFloat) =
        let m = min (min c.R c.G) c.B
        let v = max (max c.R c.G) c.B
        let s = if v > Single.Epsilon then 1.0f - m / v else 0.0f
        let l = (m + v) / 2.0f
        if l < Single.Epsilon || v < m 
        then HsvaFloat(0.0f, s, v, c.A)
        else
            let vm = v - m
            let r2 = (v - c.R) / vm
            let g2 = (v - c.G) / vm
            let b2 = (v - c.B) / vm
            let hx = 
                if c.R = v then if c.G = m then 5.0f + b2 else 1.0f - g2
                else if c.G = v then if c.B = m then 1.0f + r2 else 3.0f - b2
                else if c.R = m then 3.0f + g2 else 5.0f - r2
                / 6.0f
            let h = if hx >= 1.0f then hx - 1.0f else hx
            HsvaFloat(h, s, v, c.A)
    member c.ShiftHue(shift) =
        HsvaFloat(c.H + shift, c.S, c.V, c.A)
    member c.ToRgbaFloat() =
        // from: http://alvyray.com/Papers/CG/hsv2rgb.htm
        // H is given on [0, 6] or UNDEFINED. S and V are given on [0, 1].  
        // RGB are each returned on [0, 1].
        let f = c.H - floor c.H
        let h = f * 6.0f
        let v = c.V
        let i = int32(floor h)
        // if i is even
        let g = if (i &&& 1) = 0 then 1.0f - f else f
        let m = v * (1.0f - c.S)
        let n = v * (1.0f - c.S * g)
        match i with
        | 6 -> RgbaFloat(v, n, m, c.A)
        | 0 -> RgbaFloat(v, n, m, c.A)
        | 1 -> RgbaFloat(n, v, m, c.A)
        | 2 -> RgbaFloat(m, v, n, c.A)
        | 3 -> RgbaFloat(m, n, v, c.A)
        | 4 -> RgbaFloat(n, m, v, c.A)
        | _ -> RgbaFloat(v, m, n, c.A)
    static member Lerp(min : HsvaFloat, max : HsvaFloat, t) =
        HsvaFloat(
            MathF.Lerp(min.H, max.H, t),
            MathF.Lerp(min.S, max.S, t),
            MathF.Lerp(min.V, max.V, t),
            MathF.Lerp(min.A, max.A, t))

[<Struct>]
type HslaFloat =
    val H : float32
    val S : float32
    val L : float32
    val A : float32
    new(h, s, l, a) = { H = h; S = s; L = l; A = a; }
    new(c : RgbaFloat) =
        let c0 = min (min c.R c.G) c.B
        let c1 = max (max c.R c.G) c.B
        let l = (c0 + c1) / 2.0f
        if c0 = c1 then HslaFloat(0.0f, 0.0f, l, c.A)
        else
            let delta = c1 - c0
            let s =
                if l <= 0.5f
                    then (delta / (c1 + c0)) 
                    else (delta / (2.0f - (c1 + c0)))
            let h =
                if c.R = c1 then (c.G - c.B) / delta
                else if c.G = c1 then 2.0f + (c.B - c.R) / delta
                else if c.B = c1 then 4.0f + (c.R - c.G) / delta
                else 0.0f
            let x = h / 6.0f
            HslaFloat(x - floor x, s, l, c.A)
    member c.ShiftHue(shift) =
        HsvaFloat(c.H + shift, c.S, c.L, c.A)
    member c.ToRgbaFloat() =
        // Note: there is a typo in the 2nd International Edition of Foley and
        // van Dam's "Computer Graphics: Principles and Practice", section 13.3.5
        // (The HLS Color Model). This incorrectly replaces the 1f in the following
        // line with "l", giving confusing results.
        if c.S = 0.0f then RgbaFloat(c.L, c.L, c.L, c.A)
        else
            let m2 = 
                if c.L <= 0.5f 
                then c.L * (1.0f + c.S)
                else c.L + c.S - c.L * c.S
            let m1 = 2.0f * c.L - m2
            RgbaFloat(
                HslaFloat.GetChannelValue(m1, m2, (c.H + 1.0f / 3.0f)),
                HslaFloat.GetChannelValue(m1, m2, c.H),
                HslaFloat.GetChannelValue(m1, m2, (c.H - 1.0f / 3.0f)),
                c.A)
    static member Lerp(min : HslaFloat, max : HslaFloat, t) =
        HslaFloat(
            MathF.Lerp(min.H, max.H, t),
            MathF.Lerp(min.S, max.S, t),
            MathF.Lerp(min.L, max.L, t),
            MathF.Lerp(min.A, max.A, t))
    static member private GetChannelValue(n1, n2, t) =
        let hue =
            if t < 0.0f then t + 1.0f
            else if t > 1.0f then t - 1.0f
            else t
        if hue < 1.0f / 6.0f then n1 + (n2 - n1) * hue * 6.0f
        else if hue < 0.5f then n2
        else if (hue < 2.0f / 3.0f) then n1 + (n2 - n1) * (2.0f / 3.0f - hue) * 6.0f
        else n1                    

[<AutoOpen>]
module RgbaByte =
    type RgbaByte with
        member c.ToRgbaFloat() =
            RgbaFloat(
                float32 c.R / 255.0f,
                float32 c.G / 255.0f,
                float32 c.B / 255.0f,
                float32 c.A / 255.0f)
            
        member c.ToUInt32() =
            (int(c.R) <<< 24) |||
            (int(c.G) <<< 16) |||
            (int(c.B) <<< 8) |||
            (int(c.A) <<< 0)

        static member FromUInt32(x : uint) =
            RgbaByte(
                byte ((x >>> 24) &&& (uint32 0xff)),
                byte ((x >>> 16) &&& (uint32 0xff)),
                byte ((x >>> 8) &&& (uint32 0xff)),
                byte ((x >>> 0) &&& (uint32 0xff)))

[<AutoOpen>]
module RgbaFloat =
    type RgbaFloat with
        member c.Add(b : RgbaFloat) =
            RgbaFloat(c.R + b.R, c.G + b.G, c.B + b.B, c.A + b.A)

        member c.Multiply(b : RgbaFloat) =
            RgbaFloat(c.R * b.R, c.G * b.G, c.B * b.B, c.A * b.A)

        member c.Multiply(x) =
            RgbaFloat(c.R * x, c.G * x, c.B * x, c.A * x)

        member c.MultiplyRgb(x) =
            RgbaFloat(c.R * x, c.G * x, c.B * x, c.A)

        member c.MultiplyAlpha(a) =
            RgbaFloat(c.R, c.G, c.B, c.A * a)
            
        member c.WithAlpha(a) =
            RgbaFloat(c.R, c.G, c.B, a)
            
        member c.Clamp() =
            RgbaFloat(
                MathF.Clamp01(c.R),
                MathF.Clamp01(c.G),
                MathF.Clamp01(c.B),
                MathF.Clamp01(c.A))

        member c.ToRgbaByte() =
            RgbaByte(
                byte (c.R * 255.0f |> max 0.0f |> min 255.0f),
                byte (c.G * 255.0f |> max 0.0f |> min 255.0f),
                byte (c.B * 255.0f |> max 0.0f |> min 255.0f),
                byte (c.A * 255.0f |> max 0.0f |> min 255.0f))
            
        static member Lerp(min : RgbaFloat, max : RgbaFloat, t) =
            RgbaFloat(
                MathF.Lerp(min.R, max.R, t),
                MathF.Lerp(min.G, max.G, t),
                MathF.Lerp(min.B, max.B, t),
                MathF.Lerp(min.A, max.A, t))
        
        static member Luminance(x, a) =
            RgbaFloat(x, x, x, a)
            
        static member FromUInt32(x : uint) =
            RgbaByte.FromUInt32(x).ToRgbaFloat()