namespace Garnet.Numerics

open System.Numerics

// Float range types

[<Struct>]
type Range =
    val Min : float32
    val Max : float32
    new(min, max) = { Min = min; Max = max }
    member inline c.Size = c.Max - c.Min
    member inline c.IsEmpty = c.Min >= c.Max
    member inline c.Contains(x) = x >= c.Min && x < c.Max
    member inline c.Expand(margin) = Range(c.Min - margin, c.Max + margin)
    member inline c.Clamp(x) = x |> max c.Min |> min c.Max
    override v.ToString() = $"%A{v.Min} to %A{v.Max}"
    static member Zero = Range(0.0f, 0.0f)
    static member ZeroToOne = Range(0.0f, 1.0f)
    static member inline Lerp(r : Range, t) = r.Min * (1.0f - t) + r.Max * t
    static member inline (+) (a: Range, c) = Range(a.Min + c, a.Max + c)
    static member inline (-) (a: Range, c) = Range(a.Min - c, a.Max - c)
    static member inline (*) (a: Range, c) = Range(a.Min * c, a.Max * c)
    static member inline (/) (a: Range, c) = Range(a.Min / c, a.Max / c)
    static member inline Point(point) = Range(point, point)
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
    member inline c.X = Range(c.Min.X, c.Max.X)
    member inline c.Y = Range(c.Min.Y, c.Max.Y)
    member inline c.Center = (c.Min + c.Max) * 0.5f
    member inline c.Size = c.Max - c.Min
    member inline c.GetArea() =
        let s = c.Size
        s.X * s.Y
    member inline c.Contains(p : Vector2) = 
        c.X.Contains p.X && c.Y.Contains p.Y
    member inline c.Expand(margin : Vector2) = 
        Range2(
            c.X.Expand(margin.X),
            c.Y.Expand(margin.Y))
    override i.ToString() = $"%A{i.Min} to %A{i.Max}"
    static member Zero = Range2(Vector2.Zero, Vector2.Zero)
    static member ZeroToOne = Range2(Vector2.Zero, Vector2.One)
    static member inline Lerp(r : Range2, t : Vector2) =
        Vector2(
            Range.Lerp(r.X, t.X),
            Range.Lerp(r.Y, t.Y))
    static member inline (+) (a: Range2, c : float32) = Range2(a.X + c, a.Y + c)
    static member inline (-) (a: Range2, c : float32) = Range2(a.X - c, a.Y - c)
    static member inline (*) (a: Range2, c : float32) = Range2(a.X * c, a.Y * c)
    static member inline (/) (a: Range2, c : float32) = Range2(a.X / c, a.Y / c)
    static member inline (+) (a: Range2, v : Vector2) = Range2(a.X + v.X, a.Y + v.Y)
    static member inline (-) (a: Range2, v : Vector2) = Range2(a.X - v.X, a.Y - v.Y)
    static member inline (*) (a: Range2, v : Vector2) = Range2(a.X * v.X, a.Y * v.Y)
    static member inline (/) (a: Range2, v : Vector2) = Range2(a.X / v.X, a.Y / v.Y)
    static member inline Point(point : Vector2) = Range2(point, point)
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
type Range3 =
    val Min : Vector3
    val Max : Vector3
    new(min, max) = { Min = min; Max = max }
    new(x : Range, y : Range, z : Range) = { 
        Min = Vector3(x.Min, y.Min, z.Min)
        Max = Vector3(x.Max, y.Max, z.Max) 
        }
    member inline c.X = Range(c.Min.X, c.Max.X)
    member inline c.Y = Range(c.Min.Y, c.Max.Y)
    member inline c.Z = Range(c.Min.Z, c.Max.Z)
    member inline c.Size = c.Max - c.Min
    member inline c.GetVolume() =
        let s = c.Size
        s.X * s.Y * s.Z
    member inline c.Contains(p : Vector3) = 
        c.X.Contains p.X && 
        c.Y.Contains p.Y && 
        c.Z.Contains p.Z
    member inline c.Expand(margin : Vector3) = 
        Range3(
            c.X.Expand(margin.X),
            c.Y.Expand(margin.Y),
            c.Z.Expand(margin.Z))
    member inline c.Clamp(p : Vector3) = 
        Vector3(
            c.X.Clamp p.X,
            c.Y.Clamp p.Y,
            c.Z.Clamp p.Z)
    override i.ToString() = $"%A{i.Min} to %A{i.Max}"
    static member inline Lerp(r : Range3, t : Vector3) =
        Vector3(
            Range.Lerp(r.X, t.X),
            Range.Lerp(r.Y, t.Y),
            Range.Lerp(r.Z, t.Z))
    static member Zero = Range3(Vector3.Zero, Vector3.Zero)
    static member ZeroToOne = Range3(Vector3.Zero, Vector3.One)
    static member inline (+) (a: Range3, c) = Range3(a.X + c, a.Y + c, a.Z + c)
    static member inline (-) (a: Range3, c) = Range3(a.X - c, a.Y - c, a.Z - c)
    static member inline (*) (a: Range3, c) = Range3(a.X * c, a.Y * c, a.Z * c)
    static member inline (/) (a: Range3, c) = Range3(a.X / c, a.Y / c, a.Z / c)
    static member inline (+) (a: Range3, v : Vector3) = Range3(a.X + v.X, a.Y + v.Y, a.Z + v.Z)
    static member inline (-) (a: Range3, v : Vector3) = Range3(a.X - v.X, a.Y - v.Y, a.Z - v.Z)
    static member inline (*) (a: Range3, v : Vector3) = Range3(a.X * v.X, a.Y * v.Y, a.Z * v.Z)
    static member inline (/) (a: Range3, v : Vector3) = Range3(a.X / v.X, a.Y / v.Y, a.Z / v.Z)
    static member inline Point(point : Vector3) = Range3(point, point)
    static member inline Sized(min : Vector3, size : Vector3) = 
        Range3(min, min + size)
    static member inline Centered(center : Vector3, size : Vector3) = 
        Range3.Sized(center - size * 0.5f, size)
    static member inline Intersection(a : Range3, b : Range3) = 
        Range3(
            Range.Intersection(a.X, b.X), 
            Range.Intersection(a.Y, b.Y),
            Range.Intersection(a.Z, b.Z))
    static member inline Union(a : Range3, b : Range3) =         
        Range3(
            Range.Union(a.X, b.X), 
            Range.Union(a.Y, b.Y),
            Range.Union(a.Z, b.Z))

// Int32 range types

[<Struct>]
type Rangei =
    val Min : int
    val Max : int
    new(min, max) = { Min = min; Max = max }
    member inline c.Size = c.Max - c.Min
    member inline c.IsEmpty = c.Min >= c.Max
    member inline c.Contains(x) = x >= c.Min && x < c.Max
    member inline c.Expand(margin) = Rangei(c.Min - margin, c.Max + margin)
    member inline c.Clamp(x) = x |> max c.Min |> min c.Max
    member inline c.ToRange() = Range(float32 c.Min, float32 c.Max)
    override v.ToString() = $"%A{v.Min} to %A{v.Max}"
    static member Zero = Rangei(0, 0)
    static member ZeroToOne = Rangei(0, 1)
    static member inline (+) (a: Rangei, c) = Rangei(a.Min + c, a.Max + c)
    static member inline (-) (a: Rangei, c) = Rangei(a.Min - c, a.Max - c)
    static member inline (*) (a: Rangei, c) = Rangei(a.Min * c, a.Max * c)
    static member inline (/) (a: Rangei, c) = Rangei(a.Min / c, a.Max / c)
    static member inline Point(point) = Rangei(point, point)
    static member inline Sized(min, size) = 
        Rangei(min, min + size)
    static member inline Centered(center, size) = 
        Rangei.Sized(center - size / 2, size)
    static member inline Intersection(a : Rangei, b : Rangei) = 
        Rangei(max a.Min b.Min, min a.Max b.Max)
    static member inline Union(a : Rangei, b : Rangei) = 
        Rangei(min a.Min b.Min, max a.Max b.Max)

[<Struct>]
type Range2i =
    val Min : Vector2i
    val Max : Vector2i
    new(min, max) = { Min = min; Max = max }
    new(x : Rangei, y : Rangei) = { 
        Min = Vector2i(x.Min, y.Min)
        Max = Vector2i(x.Max, y.Max) 
        }
    member inline c.X = Rangei(c.Min.X, c.Max.X)
    member inline c.Y = Rangei(c.Min.Y, c.Max.Y)
    member inline c.Size = c.Max - c.Min
    member inline c.IsEmpty = c.X.IsEmpty || c.Y.IsEmpty
    member inline c.ToRange2() = Range2(c.Min.ToVector2(), c.Max.ToVector2())
    member inline c.GetArea() =
        let s = c.Size
        s.X * s.Y
    member inline c.Contains (p : Vector2i) = 
        c.X.Contains p.X && 
        c.Y.Contains p.Y
    member inline c.Expand(margin : Vector2i) = 
        Range2i(
            c.X.Expand(margin.X),
            c.Y.Expand(margin.Y))
    member inline c.Clamp(p : Vector2i) = 
        Vector2i(c.X.Clamp p.X, c.Y.Clamp p.Y)
    override i.ToString() = $"%A{i.Min} to %A{i.Max}"
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
    static member inline Point(point : Vector2i) = Range2i(point, point)
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
type Range3i =
    val Min : Vector3i
    val Max : Vector3i
    new(min, max) = { Min = min; Max = max }
    new(x : Rangei, y : Rangei, z : Rangei) = { 
        Min = Vector3i(x.Min, y.Min, z.Min)
        Max = Vector3i(x.Max, y.Max, z.Max) 
        }
    member inline c.X = Rangei(c.Min.X, c.Max.X)
    member inline c.Y = Rangei(c.Min.Y, c.Max.Y)
    member inline c.Z = Rangei(c.Min.Z, c.Max.Z)
    member inline c.Size = c.Max - c.Min
    member inline c.IsEmpty = c.X.IsEmpty || c.Y.IsEmpty || c.Z.IsEmpty
    member inline c.ToRange3() = Range3(c.Min.ToVector3(), c.Max.ToVector3())
    member inline c.GetVolume() =
        let s = c.Size
        s.X * s.Y * s.Z
    member inline c.Contains(p : Vector3i) = 
        c.X.Contains p.X && 
        c.Y.Contains p.Y && 
        c.Z.Contains p.Z
    member inline c.Expand(margin : Vector3i) = 
        Range3i(
            c.X.Expand(margin.X),
            c.Y.Expand(margin.Y),
            c.Z.Expand(margin.Z))
    member inline c.Clamp(p : Vector3i) = 
        Vector3i(
            c.X.Clamp p.X,
            c.Y.Clamp p.Y,
            c.Z.Clamp p.Z)
    override i.ToString() = $"%A{i.Min} to %A{i.Max}"
    static member Zero = Range3i(Vector3i.Zero, Vector3i.Zero)
    static member ZeroToOne = Range3i(Vector3i.Zero, Vector3i.One)
    static member inline (+) (a: Range3i, c) = Range3i(a.X + c, a.Y + c, a.Z + c)
    static member inline (-) (a: Range3i, c) = Range3i(a.X - c, a.Y - c, a.Z - c)
    static member inline (*) (a: Range3i, c) = Range3i(a.X * c, a.Y * c, a.Z * c)
    static member inline (/) (a: Range3i, c) = Range3i(a.X / c, a.Y / c, a.Z / c)
    static member inline (+) (a: Range3i, v : Vector3i) = Range3i(a.X + v.X, a.Y + v.Y, a.Z + v.Z)
    static member inline (-) (a: Range3i, v : Vector3i) = Range3i(a.X - v.X, a.Y - v.Y, a.Z - v.Z)
    static member inline (*) (a: Range3i, v : Vector3i) = Range3i(a.X * v.X, a.Y * v.Y, a.Z * v.Z)
    static member inline (/) (a: Range3i, v : Vector3i) = Range3i(a.X / v.X, a.Y / v.Y, a.Z / v.Z)
    static member inline Point(point : Vector3i) = Range3i(point, point)
    static member inline Sized(min : Vector3i, size : Vector3i) = 
        Range3i(min, min + size)
    static member inline Centered(center : Vector3i, size : Vector3i) = 
        Range3i.Sized(center - size / 2, size)
    static member inline Intersection(a : Range3i, b : Range3i) = 
        Range3i(
            Rangei.Intersection(a.X, b.X), 
            Rangei.Intersection(a.Y, b.Y),
            Rangei.Intersection(a.Z, b.Z))
    static member inline Union(a : Range3i, b : Range3i) =         
        Range3i(
            Rangei.Union(a.X, b.X), 
            Rangei.Union(a.Y, b.Y),
            Rangei.Union(a.Z, b.Z))
