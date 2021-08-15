namespace Garnet.Samples.Engine

open System
open System.Numerics

// Int32 vector types

[<Struct>]
type Vector2i = 
    val X : int
    val Y : int
    new(x, y) = { X = x; Y = y }
    member c.ToVector2() = Vector2(float32 c.X, float32 c.Y)
    override v.ToString() = $"%A{v.X} %A{v.Y}"
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
type Vector3i = 
    val X : int
    val Y : int
    val Z : int
    new(x, y, z) = { X = x; Y = y; Z = z }
    member c.ToVector3() = Vector3(float32 c.X, float32 c.Y, float32 c.Z)
    override v.ToString() = $"%A{v.X} %A{v.Y} %A{v.Z}"
    static member Zero = Vector3i(0, 0, 0)
    static member One = Vector3i(1, 1, 1)
    static member UnitX = Vector3i(1, 0, 0)
    static member UnitY = Vector3i(0, 1, 0)
    static member UnitZ = Vector3i(0, 0, 1)
    static member inline Dot(a: Vector3i, b: Vector3i) = a.X * b.X + a.Y * b.Y + a.Z * b.Z
    static member inline (~-) (v: Vector3i) = Vector3i(-v.X, -v.Y, -v.Z)
    static member inline (+) (a: Vector3i, c) = Vector3i(a.X + c, a.Y + c, a.Z + c) 
    static member inline (-) (a: Vector3i, c) = Vector3i(a.X - c, a.Y - c, a.Z - c)
    static member inline (*) (a: Vector3i, c) = Vector3i(a.X * c, a.Y * c, a.Z * c) 
    static member inline (/) (a: Vector3i, c) = Vector3i(a.X / c, a.Y / c, a.Z / c)
    static member inline (>>>) (a: Vector3i, c) = Vector3i(a.X >>> c, a.Y >>> c, a.Z >>> c)
    static member inline (<<<) (a: Vector3i, c) = Vector3i(a.X <<< c, a.Y <<< c, a.Z <<< c)
    static member inline (&&&) (a: Vector3i, c) = Vector3i(a.X &&& c, a.Y &&& c, a.Z &&& c)
    static member inline (+) (a: Vector3i, b: Vector3i) = Vector3i(a.X + b.X, a.Y + b.Y, a.Z + b.Z)
    static member inline (-) (a: Vector3i, b: Vector3i) = Vector3i(a.X - b.X, a.Y - b.Y, a.Z - b.Z)
    static member inline (*) (a: Vector3i, b: Vector3i) = Vector3i(a.X * b.X, a.Y * b.Y, a.Z * b.Z) 
    static member inline (/) (a: Vector3i, b: Vector3i) = Vector3i(a.X / b.X, a.Y / b.Y, a.Z / b.Z) 
    static member inline FromVector3(v : Vector3) = Vector3i(int v.X, int v.Y, int v.Z)

// Float vector types

[<AutoOpen>]
module Vector2 =
    type Vector2 with
        static member FromRadians(a) =
            Vector2(cos a, sin a)

        static member FromDegrees(a) = 
            Vector2.FromRadians(a * MathF.PI / 180.0f)
        
        static member inline Rotate(r : Vector2, a : Vector2) = 
            Vector2(a.X * r.X - a.Y * r.Y, a.X * r.Y + a.Y * r.X)

        static member inline Perpendicular(v : Vector2) =
            Vector2(-v.Y, v.X)

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

        member v.IsInTriangle(p0 : Vector2, p1 : Vector2, p2 : Vector2) =
            let a = 0.5f * (-p1.Y * p2.X + p0.Y * (-p1.X + p2.X) + p0.X * (p1.Y - p2.Y) + p1.X * p2.Y)
            let sign = if a < 0.0f then -1.0f else 1.0f;
            let s = (p0.Y * p2.X - p0.X * p2.Y + (p2.Y - p0.Y) * v.X + (p0.X - p2.X) * v.Y) * sign
            let t = (p0.X * p1.Y - p0.Y * p1.X + (p0.Y - p1.Y) * v.X + (p1.X - p0.X) * v.Y) * sign
            //s > 0.0f && t > 0.0f && (s + t) < 2.0f * A * sign
            s >= 0.0f && t >= 0.0f && (s + t) <= 2.0f * a * sign

[<AutoOpen>]
module Vector3 =
    type Vector3 with
        member v.ToVector2() =
            Vector2(v.X, v.Y)
