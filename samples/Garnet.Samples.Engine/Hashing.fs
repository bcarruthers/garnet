namespace Garnet.Samples.Engine

open System

module Fnv1a =
    [<Literal>]
    let Prime = 0x1000193u

    [<Literal>]
    let Seed = 0x811c9dc5u

type Fnv1a() =
    static member inline Combine(hash, value) =
        (hash ^^^ value) * Fnv1a.Prime

    static member inline Hash(x1 : uint32) =
        let h = Fnv1a.Seed
        let h = Fnv1a.Combine(h, x1)
        h

    static member inline Hash(x1 : uint32, x2 : uint32) =
        let h = Fnv1a.Seed
        let h = Fnv1a.Combine(h, x1)
        let h = Fnv1a.Combine(h, x2)
        h
        
    static member inline Hash(x1 : uint32, x2 : uint32, x3 : uint32) =
        let h = Fnv1a.Seed
        let h = Fnv1a.Combine(h, x1)
        let h = Fnv1a.Combine(h, x2)
        let h = Fnv1a.Combine(h, x3)
        h

    static member inline Hash(key : string) =
        let mutable hash = Fnv1a.Seed
        for i = 0 to key.Length - 1 do
            let value = uint32 key.[i]
            hash <- Fnv1a.Combine(hash, value)
        hash
        
module Fnv1a64 =
    [<Literal>]
    let Prime = 0x100000001b3UL

    [<Literal>]
    let Seed = 0xcbf29ce484222325UL

type Fnv1a64() =
    static member inline Combine(hash, value) =
        (hash ^^^ value) * Fnv1a64.Prime

    static member inline Hash(value : uint64) =
        let h = Fnv1a64.Seed
        let h = Fnv1a64.Combine(h, value)
        h

    static member inline Hash(x1 : uint64, x2 : uint64) =
        let h = Fnv1a64.Seed
        let h = Fnv1a64.Combine(h, x1)
        let h = Fnv1a64.Combine(h, x2)
        h
        
    static member inline Hash(x1 : uint64, x2 : uint64, x3 : uint64) =
        let h = Fnv1a64.Seed
        let h = Fnv1a64.Combine(h, x1)
        let h = Fnv1a64.Combine(h, x2)
        let h = Fnv1a64.Combine(h, x3)
        h

    static member inline Hash(key : string) =
        let mutable hash = Fnv1a64.Seed
        for i = 0 to key.Length - 1 do
            let value = uint64 key.[i]
            hash <- Fnv1a64.Combine(hash, value)
        hash

module XXHash =
    [<Literal>]
    let Prime32_1 = 2654435761u

    [<Literal>]
    let Prime32_2 = 2246822519u

    [<Literal>]
    let Prime32_3 = 3266489917u

    [<Literal>]
    let Prime32_4 = 668265263u

    [<Literal>]
    let Prime32_5 = 374761393u

type XXHash() =    
    static member inline RotateLeft(value : uint32, count) =
        (value <<< count) ||| (value >>> (32 - count))

    static member inline Finalize(hash : uint32) =
        let h = hash ^^^ (hash >>> 15)
        let h = h * XXHash.Prime32_2
        let h = h ^^^ (h >>> 13)
        let h = h * XXHash.Prime32_3
        let h = h ^^^ (h >>> 16)
        h

    static member inline Combine(hash : uint32, value : uint32) = 
        let h = hash + value * XXHash.Prime32_3
        let h = XXHash.RotateLeft(h, 17) * XXHash.Prime32_4
        h

    static member inline Initialize(seed : uint32) =
        seed + XXHash.Prime32_5

    static member inline Initialize(seed : uint32, size : uint32) =
        let h = seed + XXHash.Prime32_5
        let h = h + size
        h

    static member inline Hash(seed : uint32, value : uint32) =        
        let h = XXHash.Initialize(seed, 4u)
        let h = XXHash.Combine(h, value)
        XXHash.Finalize(h)

    static member inline Hash(seed : uint32, x1 : uint32, x2 : uint32) =
        let h = XXHash.Initialize(seed, 8u)
        let h = XXHash.Combine(h, x1)
        let h = XXHash.Combine(h, x2)
        XXHash.Finalize(h)
        
    static member inline Hash(seed : uint32, x1 : uint32, x2 : uint32, x3 : uint32) =
        let h = XXHash.Initialize(seed, 12u)
        let h = XXHash.Combine(h, x1)
        let h = XXHash.Combine(h, x2)
        let h = XXHash.Combine(h, x3)
        XXHash.Finalize(h)

    static member inline Hash(seed : uint32, span : ReadOnlySpan<int>) =
        let mutable h = XXHash.Initialize(seed, uint32 span.Length)
        for i = 0 to span.Length - 1 do
            h <- XXHash.Combine(h, uint32 span.[i])
        XXHash.Finalize(h)

    static member inline Hash(seed : uint32, span : ReadOnlySpan<uint64>) =
        let mutable h = XXHash.Initialize(seed, uint32 (span.Length * 2))
        for i = 0 to span.Length - 1 do
            h <- XXHash.Combine(h, uint32 (span.[i] &&& 0xffffffffUL))
            h <- XXHash.Combine(h, uint32 (span.[i] >>> 32))
        XXHash.Finalize(h)

    static member inline FinalizeToRange(hash, index, min, max) =
        let h = XXHash.Combine(hash, uint32 index)
        let h = XXHash.Finalize(h)
        int (h % uint32 (max - min)) + min
