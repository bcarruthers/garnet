namespace Garnet.Samples.Engine

open System
open System.Numerics

type PcgRandom(initState, initSeq) =
    let inc = PcgRandom.GetIncrement(initSeq)
    let mutable state = PcgRandom.GetState(initState, inc)
    new() = PcgRandom(0x853c49e6748fea9bUL, 0xda3e39cb94b95bdbUL)
    
    member c.NextUInt32() =
        let result = PcgRandom.GetUInt32(state)
        state <- PcgRandom.GetNextState(state, inc)
        result
    
    static member GetUInt32(state) =
        let xorShifted = uint32 (((state >>> 18) ^^^ state) >>> 27)
        let rot = int (state >>> 59)
        (xorShifted >>> rot) ||| (xorShifted <<< ((-rot) &&& 31))
    
    static member GetNextState(state, inc) =        
        state * 6364136223846793005UL + inc
    
    static member GetIncrement(initSeq) =
        (initSeq <<< 1) ||| 1UL
    
    static member GetState(seed, inc) =
        let s = PcgRandom.GetNextState(0UL, inc) + seed
        PcgRandom.GetNextState(s, inc)

type PcgRandom with
    member c.NextInt32() =
        c.NextUInt32() |> int

    member c.NextUInt32Unbiased(exclusiveBound : uint) =
        let threshold = uint32 ((0x100000000UL - uint64 exclusiveBound) % uint64 exclusiveBound)
        let mutable r = c.NextUInt32()
        while r < threshold do
            r <- c.NextUInt32()
        r % exclusiveBound

    member c.NextInt32Unbiased(exclusiveBound : int) =
        let threshold = int32 ((0x100000000UL - uint64 exclusiveBound) % uint64 exclusiveBound)
        let mutable r = c.NextInt32()
        while r < threshold do
            r <- c.NextInt32()
        r % exclusiveBound

    member c.NextInt32(max : int) =
        if max = 0 then 0 else c.NextUInt32() % (uint32 max) |> int

    member c.NextUInt32(max : uint) =
        if max = 0u then 0u else c.NextUInt32() % max

    member c.NextUInt32(min, max) =
        c.NextUInt32(max - min) + min

    member c.NextInt32(min, max) =
        c.NextInt32(max - min) + min

    /// Closed [0, 1]
    member c.NextDouble() =
        let x = c.NextUInt32()
        double x * (1.0 / 4294967295.0)

    /// Half open [0, 1)
    member c.NextDouble2() = 
        let x = c.NextUInt32()
        double x * (1.0 / 4294967296.0)

    /// Open (0, 1)
    member c.NextDouble3() = 
        let x = c.NextUInt32()
        (double x + 0.5) * (1.0 / 4294967296.0)

    member c.NextSingle() = float32 (c.NextDouble())
    member c.NextSingle2() = float32 (c.NextDouble2())
    member c.NextSingle3() = float32 (c.NextDouble3())

    member c.NextSingle(min, max) =
        c.NextSingle() * (max - min) + min

    member c.NextRotation(radiansRange) =
        Vector2.FromRadians((c.NextSingle() - 0.5f) * radiansRange) 

    member c.NextRotationDegrees(degreesRange) =
        c.NextRotation(degreesRange * MathF.PI / 180.0f)

    member c.NextVector2() =
        Vector2(c.NextSingle(), c.NextSingle())
        
    member c.NextVector2(range : Range2) =
        Range2.Lerp(range, c.NextVector2())

    member c.NextVector3() =
        Vector3(c.NextSingle(), c.NextSingle(), c.NextSingle())
        
    member c.NextVector3(range : Range3) =
        Range3.Lerp(range, c.NextVector3())

    member c.NextVector2i(r : Range2i) =
        Vector2i(
            c.NextInt32(r.Min.X, r.Max.X),
            c.NextInt32(r.Min.Y, r.Max.Y))

    member c.NextVector2i(min, max) =
        let r = Rangei(min, max)
        c.NextVector2i(Range2i(r, r))

    member c.NextVector3i(r : Range3i) =
        Vector3i(
            c.NextInt32(r.Min.X, r.Max.X),
            c.NextInt32(r.Min.Y, r.Max.Y),
            c.NextInt32(r.Min.Z, r.Max.Z))

    member c.NextVector3i(min, max) =
        let r = Rangei(min, max)
        c.NextVector3i(Range3i(r, r, r))

    member c.Next(dest : Span<int>) =
        for i = 0 to dest.Length - 1 do
            dest.[i] <- c.NextInt32()
        
    member c.Shuffle(items) =
        let r = items |> Seq.toArray
        for i = 0 to r.Length - 2 do
            let j = c.NextInt32(i, r.Length)
            let temp = r.[i]
            r.[i] <- r.[j]
            r.[j] <- temp    
        r
        