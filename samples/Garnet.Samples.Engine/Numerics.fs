namespace Garnet.Samples.Engine

open System
open System.Numerics
open Veldrid

module MathF =
    let inline lerp min max (t : float32) = 
        min * (1.0f - t) + max * t

[<Struct>]
type Vector2i = 
    val X : int
    val Y : int
    new(x, y) = { X = x; Y = y }
    member c.ToVector2() = Vector2(float32 c.X, float32 c.Y)
    override v.ToString() = sprintf "%A %A" v.X v.Y
    static member Zero = Vector2i(0, 0)
    static member One = Vector2i(1, 1)
    static member UnitX = Vector2i(1, 0)
    static member UnitY = Vector2i(0, 1)
    static member inline Dot(a: Vector2i, b: Vector2i) = a.X * b.X + a.Y * b.Y
    static member inline (~-) (v: Vector2i) = Vector2i(-v.X, -v.Y)
    static member inline (+) (a: Vector2i, c) = Vector2i(a.X + c, a.Y + c) 
    static member inline (-) (a: Vector2i, c) = Vector2i(a.X - c, a.Y - c)
    static member inline (*) (a: Vector2i, c) = Vector2i(a.X * c, a.Y * c) 
    static member inline (/) (a: Vector2i, c) = Vector2i(a.X / c, a.Y / c)
    static member inline (>>>) (a: Vector2i, c) = Vector2i(a.X >>> c, a.Y >>> c)
    static member inline (<<<) (a: Vector2i, c) = Vector2i(a.X <<< c, a.Y <<< c)
    static member inline (&&&) (a: Vector2i, c) = Vector2i(a.X &&& c, a.Y &&& c)
    static member inline (+) (a: Vector2i, b: Vector2i) = Vector2i(a.X + b.X, a.Y + b.Y)
    static member inline (-) (a: Vector2i, b: Vector2i) = Vector2i(a.X - b.X, a.Y - b.Y)
    static member inline (*) (a: Vector2i, b: Vector2i) = Vector2i(a.X * b.X, a.Y * b.Y) 
    static member inline (/) (a: Vector2i, b: Vector2i) = Vector2i(a.X / b.X, a.Y / b.Y) 
    static member inline Perpendicular(v : Vector2i) = Vector2i(-v.Y, v.X)    
    static member inline FromVector2(v : Vector2) = Vector2i(int v.X, int v.Y)

[<Struct>]
type Rangei =
    val Min : int
    val Max : int
    new(min, max) = { Min = min; Max = max }
    member c.Size = c.Max - c.Min
    member c.IsEmpty = c.Min >= c.Max
    member c.Contains(x) = x >= c.Min && x < c.Max
    member c.Expand(margin) = Rangei(c.Min - margin, c.Max + margin)
    member c.Clamp(x) = x |> max c.Min |> min c.Max
    override v.ToString() = sprintf "%A to %A" v.Min v.Max
    static member ZeroToOne = Rangei(0, 1)
    static member inline (+) (a: Rangei, c) = Rangei(a.Min + c, a.Max + c)
    static member inline (-) (a: Rangei, c) = Rangei(a.Min - c, a.Max - c)
    static member inline (*) (a: Rangei, c) = Rangei(a.Min * c, a.Max * c)
    static member inline (/) (a: Rangei, c) = Rangei(a.Min / c, a.Max / c)
    static member inline Sized(min, size) = 
        Rangei(min, min + size)
    static member inline Centered(center, size) = 
        Rangei.Sized(center - size / 2, size)
    static member inline Intersection(a : Rangei, b : Rangei) = 
        Rangei(max a.Min b.Min, min a.Max b.Max)
    static member inline Union(a : Rangei, b : Rangei) = 
        Rangei(min a.Min b.Min, max a.Max b.Max)

[<Struct>]
type Range =
    val Min : float32
    val Max : float32
    new(min, max) = { Min = min; Max = max }
    member c.Contains x = x >= c.Min && x < c.Max
    member c.Expand(margin) = Range(c.Min - margin, c.Max + margin)
    static member Lerp(r : Range, t) = r.Min * (1.0f - t) + r.Max * t
    static member inline Sized(min, size) = 
        Range(min, min + size)
    static member inline Centered(center, size) = 
        Range.Sized(center - size * 0.5f, size)
    static member inline Intersection(a : Range, b : Range) = 
        Range(max a.Min b.Min, min a.Max b.Max)
    static member inline Union(a : Range, b : Range) = 
        Range(min a.Min b.Min, max a.Max b.Max)

[<Struct>]
type Range2 =
    val Min : Vector2
    val Max : Vector2
    new(min, max) = { Min = min; Max = max }
    new(x : Range, y : Range) = { 
        Min = Vector2(x.Min, y.Min)
        Max = Vector2(x.Max, y.Max) 
        }
    member c.X = Range(c.Min.X, c.Max.X)
    member c.Y = Range(c.Min.Y, c.Max.Y)
    member c.Center = (c.Min + c.Max) * 0.5f
    member c.Size = c.Max - c.Min
    member c.Contains(p : Vector2) = 
        c.X.Contains p.X && c.Y.Contains p.Y
    member c.Expand(margin : Vector2) = 
        Range2(
            c.X.Expand(margin.X),
            c.Y.Expand(margin.Y))
    override i.ToString() = 
        sprintf "%A to %A" i.Min i.Max
    static member Zero = Range2(Vector2.Zero, Vector2.Zero)
    static member ZeroToOne = Range2(Vector2.Zero, Vector2.One)
    static member Lerp(r : Range2, v : Vector2) = 
        Vector2(Range.Lerp(r.X, v.X), Range.Lerp(r.Y, v.Y))
    static member inline Sized(min : Vector2, size : Vector2) = 
        Range2(min, min + size)
    static member inline Centered(center : Vector2, size : Vector2) = 
        Range2.Sized(center - size * 0.5f, size)
    static member inline Intersection(a : Range2, b : Range2) = 
        Range2(
            Range.Intersection(a.X, b.X), 
            Range.Intersection(a.Y, b.Y))
    static member inline Union(a : Range2, b : Range2) =         
        Range2(
            Range.Union(a.X, b.X), 
            Range.Union(a.Y, b.Y))

[<Struct>]
type Range2i =
    val Min : Vector2i
    val Max : Vector2i
    new(min, max) = { Min = min; Max = max }
    new(x : Rangei, y : Rangei) = { 
        Min = Vector2i(x.Min, y.Min)
        Max = Vector2i(x.Max, y.Max) 
        }
    member c.X = Rangei(c.Min.X, c.Max.X)
    member c.Y = Rangei(c.Min.Y, c.Max.Y)
    member c.Size = c.Max - c.Min
    member c.IsEmpty = c.X.IsEmpty || c.Y.IsEmpty
    member c.ToRange2() = Range2(c.Min.ToVector2(), c.Max.ToVector2())
    member c.GetCount() =
        let s = c.Size
        s.X * s.Y
    member c.Contains (p : Vector2i) = 
        c.X.Contains p.X && 
        c.Y.Contains p.Y
    member c.Expand(margin : Vector2i) = 
        Range2i(
            c.X.Expand(margin.X),
            c.Y.Expand(margin.Y))
    member c.Clamp(p : Vector2i) = 
        Vector2i(c.X.Clamp p.X, c.Y.Clamp p.Y)
    override i.ToString() = 
        sprintf "%A to %A" i.Min i.Max
    static member Zero = Range2i(Vector2i.Zero, Vector2i.Zero)
    static member ZeroToOne = Range2i(Vector2i.Zero, Vector2i.One)
    static member inline (+) (a: Range2i, c) = Range2i(a.X + c, a.Y + c)
    static member inline (-) (a: Range2i, c) = Range2i(a.X - c, a.Y - c)
    static member inline (*) (a: Range2i, c) = Range2i(a.X * c, a.Y * c)
    static member inline (/) (a: Range2i, c) = Range2i(a.X / c, a.Y / c)
    static member inline (+) (a: Range2i, v : Vector2i) = Range2i(a.X + v.X, a.Y + v.Y)
    static member inline (-) (a: Range2i, v : Vector2i) = Range2i(a.X - v.X, a.Y - v.Y)
    static member inline (*) (a: Range2i, v : Vector2i) = Range2i(a.X * v.X, a.Y * v.Y)
    static member inline (/) (a: Range2i, v : Vector2i) = Range2i(a.X / v.X, a.Y / v.Y)
    static member inline Sized(min : Vector2i, size : Vector2i) = 
        Range2i(min, min + size)
    static member inline Centered(center : Vector2i, size : Vector2i) = 
        Range2i.Sized(center - size / 2, size)
    static member inline Intersection(a : Range2i, b : Range2i) = 
        Range2i(
            Rangei.Intersection(a.X, b.X), 
            Rangei.Intersection(a.Y, b.Y))
    static member inline Union(a : Range2i, b : Range2i) =         
        Range2i(
            Rangei.Union(a.X, b.X), 
            Rangei.Union(a.Y, b.Y))

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
    member c.ToRgba() =
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

module Vector2 =
    let fromRadians a =
        Vector2(cos a, sin a)

    let fromDegrees a = 
        fromRadians (a * MathF.PI / 180.0f)

[<AutoOpen>]
module Vector2Extensions =
    type Vector2 with
        member v.GetRadians() =
            atan2 v.Y v.X

        member v.DivideOrZero(c) =
            if abs c > 1e-7f then v * (1.0f / c) else Vector2.Zero

        member v.NormalizeOrZero() =
            v.DivideOrZero(v.Length())

        member v.TruncateOrZero(maxLength) =
            let lengthSqr = v.LengthSquared()
            if lengthSqr <= maxLength * maxLength then v
            else v.NormalizeOrZero() * maxLength
        
        member v.GetPerpendicular() = 
            Vector2(-v.Y, v.X)    

        member v.Rotate(r : Vector2) = 
            Vector2(v.X * r.X - v.Y * r.Y, v.X * r.Y + v.Y * r.X)

        member v.InverseRotate(r : Vector2) = 
            Vector2(v.X * r.X + v.Y * r.Y, v.Y * r.X - v.X * r.Y)

        /// Rotates towards a target vector
        /// maxRotation is a unit-length direction vector relative to X axis
        member v.RotateTowards(target : Vector2, maxRotation : Vector2) =
            let dot = Vector2.Dot(v, target)
            let rotDot = Vector2.Dot(maxRotation, Vector2.UnitX)
            if dot >= rotDot then target
            else
                let cross = Vector3.Cross(Vector3(v, 0.0f), Vector3(target, 0.0f))
                if cross.Z > 0.0f then v.Rotate(maxRotation)
                else v.InverseRotate(maxRotation)

        member v.Round() =
            Vector2(floor (v.X + 0.5f), floor (v.Y + 0.5f))

        member v.RoundToInt() =
            let v = v.Round()
            Vector2i(int v.X, int v.Y)

[<AutoOpen>]
module Matrix4x4Extensions =
    type Matrix4x4 with
        member m.GetInverseOrIdentity() =
            let mutable mInv = Matrix4x4.Identity
            if Matrix4x4.Invert(m, &mInv) then mInv else Matrix4x4.Identity

[<AutoOpen>]
module RgbaByteExtensions =
    type RgbaByte with
        member c.ToRgbaFloat() =
            RgbaFloat(
                float32 c.R / 255.0f,
                float32 c.G / 255.0f,
                float32 c.B / 255.0f,
                float32 c.A / 255.0f)

[<AutoOpen>]
module RgbaFloatExtensions =
    type RgbaFloat with
        member c.Multiply(b : RgbaFloat) =
            RgbaFloat(c.R * b.R, c.G * b.G, c.B * b.B, c.A * b.A)

        member c.MultiplyAlpha(a) =
            RgbaFloat(c.R, c.G, c.B, c.A * a)

        member c.ToRgbaByte() =
            RgbaByte(
                byte (c.R * 255.0f |> max 0.0f |> min 255.0f),
                byte (c.G * 255.0f |> max 0.0f |> min 255.0f),
                byte (c.B * 255.0f |> max 0.0f |> min 255.0f),
                byte (c.A * 255.0f |> max 0.0f |> min 255.0f))

module RgbaFloat =        
    let lerp (min : RgbaFloat) (max : RgbaFloat) t =
        RgbaFloat(
            MathF.lerp min.R max.R t,
            MathF.lerp min.G max.G t,
            MathF.lerp min.B max.B t,
            MathF.lerp min.A max.A t)
