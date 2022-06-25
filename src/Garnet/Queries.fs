namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open Garnet.Composition.Comparisons

[<Struct>]
type MaskEnumerator = 
    val mutable private mask : uint64
    val mutable private i : int
    new(mask) = {
        mask = mask
        i = -1
    }
    member c.Current = c.i
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() =
        // Alternative for .NET 3.0 onward
        // let skip = System.Numerics.BitOperations.TrailingZeroCount(c.mask) + 1
        // c.mask <- c.mask >>> skip
        // c.i <- c.i + skip
        // c.i < 64
        if c.mask = 0UL then false
        else
            while c.mask &&& 1UL = 0UL do
                c.mask <- c.mask >>> 1
                c.i <- c.i + 1
            c.mask <- c.mask >>> 1
            c.i <- c.i + 1
            true
    member c.Reset() =
        raise (NotSupportedException())
    member c.Dispose() = ()
    interface IEnumerator<int> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

[<Struct>]
type MaskEnumerable =
    val Mask : uint64
    new(mask) = { Mask = mask }
    member inline c.GetEnumerator() = new MaskEnumerator(c.Mask)
    interface IEnumerable<int> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<int> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator

[<Struct>]
type SegmentDescriptor<'k> =
    val Id : 'k
    val Mask : uint64
    new(id, mask) = { Id = id; Mask = mask }
    member inline c.GetEnumerator() = new MaskEnumerator(c.Mask)
    interface IEnumerable<int> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<int> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator

[<Struct>]
type SegmentData<'a> =
    val Array : 'a[]
    val Offset : int
    new(array, offset) = { Array = array; Offset = offset }
    member inline c.Item with get i : byref<'a> =
        &c.Array.[c.Offset + i]
    member c.AsSpan() =
        Span(c.Array, c.Offset, Segment.SegmentSize)
    member c.AsReadOnlySpan() =
        ReadOnlySpan(c.Array, c.Offset, Segment.SegmentSize)

type internal SD<'a> = SegmentData<'a>

// ComponentBatchEnumerator

// These are for iterating individual Items (read-only) in a batch.

[<Struct>]
type ComponentBatchEnumerator<'k, 's1 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : SD<'s1>
    val mutable private m : MaskEnumerator
    new(struct(desc : SegmentDescriptor<'k>, s1)) = {
        m = new MaskEnumerator(desc.Mask); s1 = s1
    }
    member c.Current =
        let i = c.m.Current
        c.s1.[i]
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() = c.m.MoveNext()
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<'s1> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

[<Struct>]
type ComponentBatchEnumerator<'k, 's1, 's2 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : SD<'s1>
    val private s2 : SD<'s2>
    val mutable private m : MaskEnumerator
    new(struct(desc : SegmentDescriptor<'k>, s1, s2)) = {
        m = new MaskEnumerator(desc.Mask); s1 = s1; s2 = s2
    }
    member c.Current =
        let i = c.m.Current
        struct(c.s1.[i], c.s2.[i])
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() = c.m.MoveNext()
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<struct('s1 * 's2)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

[<Struct>]
type ComponentBatchEnumerator<'k, 's1, 's2, 's3 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : SD<'s1>
    val private s2 : SD<'s2>
    val private s3 : SD<'s3>
    val mutable private m : MaskEnumerator
    new(struct(desc : SegmentDescriptor<'k>, s1, s2, s3)) = {
        m = new MaskEnumerator(desc.Mask); s1 = s1; s2 = s2; s3 = s3
    }
    member c.Current =
        let i = c.m.Current
        struct(c.s1.[i], c.s2.[i], c.s3.[i])
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() = c.m.MoveNext()
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<struct('s1 * 's2 * 's3)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

[<Struct>]
type ComponentBatchEnumerator<'k, 's1, 's2, 's3, 's4 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : SD<'s1>
    val private s2 : SD<'s2>
    val private s3 : SD<'s3>
    val private s4 : SD<'s4>
    val mutable private m : MaskEnumerator
    new(struct(desc : SegmentDescriptor<'k>, s1, s2, s3, s4)) = {
        m = new MaskEnumerator(desc.Mask); s1 = s1; s2 = s2; s3 = s3; s4 = s4
    }
    member c.Current =
        let i = c.m.Current
        struct(c.s1.[i], c.s2.[i], c.s3.[i], c.s4.[i])
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() = c.m.MoveNext()
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<struct('s1 * 's2 * 's3 * 's4)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

[<Struct>]
type ComponentBatchEnumerator<'k, 's1, 's2, 's3, 's4, 's5 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : SD<'s1>
    val private s2 : SD<'s2>
    val private s3 : SD<'s3>
    val private s4 : SD<'s4>
    val private s5 : SD<'s5>
    val mutable private m : MaskEnumerator
    new(struct(desc : SegmentDescriptor<'k>, s1, s2, s3, s4, s5)) = {
        m = new MaskEnumerator(desc.Mask); s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5
    }
    member c.Current =
        let i = c.m.Current
        struct(c.s1.[i], c.s2.[i], c.s3.[i], c.s4.[i], c.s5.[i])
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() = c.m.MoveNext()
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<struct('s1 * 's2 * 's3 * 's4 * 's5)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

[<Struct>]
type ComponentBatchEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : SD<'s1>
    val private s2 : SD<'s2>
    val private s3 : SD<'s3>
    val private s4 : SD<'s4>
    val private s5 : SD<'s5>
    val private s6 : SD<'s6>
    val mutable private m : MaskEnumerator
    new(struct(desc : SegmentDescriptor<'k>, s1, s2, s3, s4, s5, s6)) = {
        m = new MaskEnumerator(desc.Mask); s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5; s6 = s6
    }
    member c.Current =
        let i = c.m.Current
        struct(c.s1.[i], c.s2.[i], c.s3.[i], c.s4.[i], c.s5.[i], c.s6.[i])
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() = c.m.MoveNext()
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<struct('s1 * 's2 * 's3 * 's4 * 's5 * 's6)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

[<Struct>]
type ComponentBatchEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : SD<'s1>
    val private s2 : SD<'s2>
    val private s3 : SD<'s3>
    val private s4 : SD<'s4>
    val private s5 : SD<'s5>
    val private s6 : SD<'s6>
    val private s7 : SD<'s7>
    val mutable private m : MaskEnumerator
    new(struct(desc : SegmentDescriptor<'k>, s1, s2, s3, s4, s5, s6, s7)) = {
        m = new MaskEnumerator(desc.Mask); s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5; s6 = s6; s7 = s7
    }
    member c.Current =
        let i = c.m.Current
        struct(c.s1.[i], c.s2.[i], c.s3.[i], c.s4.[i], c.s5.[i], c.s6.[i], c.s7.[i])
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() = c.m.MoveNext()
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<struct('s1 * 's2 * 's3 * 's4 * 's5 * 's6 * 's7)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

// SegmentQueryResult

[<Struct>]
type SegmentQueryResult<'k, 's1 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val Id : 'k
    val Mask : uint64
    val Segment : SD<'s1>
    new(id, mask, s1) = { Id = id; Mask = mask; Segment = s1 }
    member c.Indices = MaskEnumerable(c.Mask)

[<Struct>]
type SegmentQueryResult<'k, 's1, 's2 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val Id : 'k
    val Mask : uint64
    val Segment1 : SD<'s1> 
    val Segment2 : SD<'s2> 
    new(id, mask, s1, s2) = {
        Id = id; Mask = mask
        Segment1 = s1; Segment2 = s2
    }
    member c.Indices = MaskEnumerable(c.Mask)
    member c.Segments = struct(c.Segment1, c.Segment2)

[<Struct>]
type SegmentQueryResult<'k, 's1, 's2, 's3 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val Id : 'k
    val Mask : uint64
    val Segment1 : SD<'s1> 
    val Segment2 : SD<'s2> 
    val Segment3 : SD<'s3> 
    new(id, mask, s1, s2, s3) = {
        Id = id; Mask = mask
        Segment1 = s1; Segment2 = s2; Segment3 = s3
    }
    member c.Indices = MaskEnumerable(c.Mask)
    member c.Segments = struct(c.Segment1, c.Segment2, c.Segment3)

[<Struct>]
type SegmentQueryResult<'k, 's1, 's2, 's3, 's4 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val Id : 'k
    val Mask : uint64
    val Segment1 : SD<'s1> 
    val Segment2 : SD<'s2> 
    val Segment3 : SD<'s3> 
    val Segment4 : SD<'s4> 
    new(id, mask, s1, s2, s3, s4) = {
        Id = id; Mask = mask
        Segment1 = s1; Segment2 = s2; Segment3 = s3; Segment4 = s4
    }
    member c.Indices = MaskEnumerable(c.Mask)
    member c.Segments = struct(c.Segment1, c.Segment2, c.Segment3, c.Segment4)

[<Struct>]
type SegmentQueryResult<'k, 's1, 's2, 's3, 's4, 's5 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val Id : 'k
    val Mask : uint64
    val Segment1 : SD<'s1> 
    val Segment2 : SD<'s2> 
    val Segment3 : SD<'s3> 
    val Segment4 : SD<'s4> 
    val Segment5 : SD<'s5> 
    new(id, mask, s1, s2, s3, s4, s5) = {
        Id = id; Mask = mask
        Segment1 = s1; Segment2 = s2; Segment3 = s3; Segment4 = s4; Segment5 = s5
    }
    member c.Indices = MaskEnumerable(c.Mask)
    member c.Segments = struct(c.Segment1, c.Segment2, c.Segment3, c.Segment4, c.Segment5)

[<Struct>]
type SegmentQueryResult<'k, 's1, 's2, 's3, 's4, 's5, 's6 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val Id : 'k
    val Mask : uint64
    val Segment1 : SD<'s1> 
    val Segment2 : SD<'s2> 
    val Segment3 : SD<'s3> 
    val Segment4 : SD<'s4> 
    val Segment5 : SD<'s5> 
    val Segment6 : SD<'s6> 
    new(id, mask, s1, s2, s3, s4, s5, s6) = {
        Id = id; Mask = mask
        Segment1 = s1; Segment2 = s2; Segment3 = s3; Segment4 = s4; Segment5 = s5; Segment6 = s6
    }
    member c.Indices = MaskEnumerable(c.Mask)
    member c.Segments = struct(c.Segment1, c.Segment2, c.Segment3, c.Segment4, c.Segment5, c.Segment6)

[<Struct>]
type SegmentQueryResult<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val Id : 'k
    val Mask : uint64
    val Segment1 : SD<'s1> 
    val Segment2 : SD<'s2> 
    val Segment3 : SD<'s3> 
    val Segment4 : SD<'s4> 
    val Segment5 : SD<'s5> 
    val Segment6 : SD<'s6> 
    val Segment7 : SD<'s7> 
    new(id, mask, s1, s2, s3, s4, s5, s6, s7) = {
        Id = id; Mask = mask
        Segment1 = s1; Segment2 = s2; Segment3 = s3; Segment4 = s4; Segment5 = s5; Segment6 = s6; Segment7 = s7
    }
    member c.Indices = MaskEnumerable(c.Mask)
    member c.Segments = struct(c.Segment1, c.Segment2, c.Segment3, c.Segment4, c.Segment5, c.Segment6, c.Segment7)

// SegmentQueryEnumerator

// These implement segment intersections (inner joins) for iterating over segments.

[<Struct>]
type SegmentQueryEnumerator<'k, 's1
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> = 
    val private s1 : Segments<'k, 's1> 
    val mutable private i1 : int
    val mutable private descriptor : SegmentDescriptor<'k>
    val mutable private data1 : SD<'s1>
    new(s1) = {
        s1 = s1
        i1 = 0
        descriptor = SegmentDescriptor<'k>(Unchecked.defaultof<'k>, 0UL)
        data1 = Unchecked.defaultof<_>
        }
    member c.Mask = c.descriptor.Mask
    member c.Item with get i =
        &c.data1.[i]
    member c.Current =
        struct(c.descriptor, c.data1)
    member c.MoveNext() =
        let mutable found = false
        while not found &&
              c.i1 < c.s1.Count do
            let seg1 = c.s1.[c.i1]
            let n1 = seg1.Id
            let mask = seg1.Mask
            if mask <> 0UL then
                c.descriptor <- SegmentDescriptor<'k>(n1, mask)
                c.data1 <- SD(seg1.Data, 0)
                found <- true
            c.i1 <- c.i1 + 1
        found
    member c.Reset() =
        c.i1 <- 0
    member c.Dispose() = ()
    interface IEnumerator<struct(SegmentDescriptor<'k> * SD<'s1>)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.Dispose() = ()
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()

[<Struct>]
type SegmentQueryEnumerator<'k, 's1, 's2
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> = 
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val mutable private i1 : int
    val mutable private i2 : int
    val mutable private descriptor : SegmentDescriptor<'k>
    val mutable private data1 : SD<'s1>
    val mutable private data2 : SD<'s2>
    new(s1, s2) = {
        s1 = s1; s2 = s2
        i1 = 0;  i2 = 0
        descriptor = SegmentDescriptor<'k>(Unchecked.defaultof<'k>, 0UL)
        data1 = Unchecked.defaultof<_>
        data2 = Unchecked.defaultof<_>
        }
    member c.GetValue1(i) = &c.data1.[i] 
    member c.GetValue2(i) = &c.data2.[i] 
    member c.Mask = c.descriptor.Mask
    member c.Item with get i =
        struct(c.data1.[i], c.data2.[i])
    member c.Current =
        struct(c.descriptor, c.data1, c.data2)
    member c.MoveNext() =
        let mutable found = false
        while not found &&
              c.i1 < c.s1.Count &&
              c.i2 < c.s2.Count do
            let seg1 = c.s1.[c.i1]
            let seg2 = c.s2.[c.i2]
            let n1 = seg1.Id
            let n2 = seg2.Id
            if   n1 < n2 then c.i1 <- c.i1 + 1
            elif n2 < n1 then c.i2 <- c.i2 + 1
            else
                let mask = seg1.Mask &&& seg2.Mask
                if mask <> 0UL then
                    c.descriptor <- SegmentDescriptor<'k>(n1, mask)
                    c.data1 <- SD(seg1.Data, 0)
                    c.data2 <- SD(seg2.Data, 0)
                    found <- true
                c.i1 <- c.i1 + 1
                c.i2 <- c.i2 + 1
        found
    member c.Reset() =
        c.i1 <- 0; c.i2 <- 0
    member c.Dispose() = ()
    interface IEnumerator<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2>)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.Dispose() = ()
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()

[<Struct>]
type SegmentQueryEnumerator<'k, 's1, 's2, 's3
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> = 
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val mutable private i1 : int
    val mutable private i2 : int
    val mutable private i3 : int
    val mutable private descriptor : SegmentDescriptor<'k>
    val mutable private data1 : SD<'s1>
    val mutable private data2 : SD<'s2>
    val mutable private data3 : SD<'s3>
    new(s1, s2, s3) = {
        s1 = s1; s2 = s2; s3 = s3
        i1 = 0;  i2 = 0;  i3 = 0
        descriptor = SegmentDescriptor<'k>(Unchecked.defaultof<'k>, 0UL)
        data1 = Unchecked.defaultof<_>
        data2 = Unchecked.defaultof<_>
        data3 = Unchecked.defaultof<_>
        }
    member c.GetValue1(i) = &c.data1.[i] 
    member c.GetValue2(i) = &c.data2.[i] 
    member c.GetValue3(i) = &c.data3.[i] 
    member c.Mask = c.descriptor.Mask
    member c.Item with get i =
        struct(c.data1.[i], c.data2.[i], c.data3.[i])
    member c.Current =
        struct(c.descriptor, c.data1, c.data2, c.data3)
    member c.MoveNext() =
        let mutable found = false
        while not found &&
              c.i1 < c.s1.Count &&
              c.i2 < c.s2.Count &&
              c.i3 < c.s3.Count do
            let seg1 = c.s1.[c.i1]
            let seg2 = c.s2.[c.i2]
            let seg3 = c.s3.[c.i3]
            let n1 = seg1.Id
            let n2 = seg2.Id
            let n3 = seg3.Id
            if   n1 < n2 || n1 < n3 then c.i1 <- c.i1 + 1
            elif n2 < n1 || n2 < n3 then c.i2 <- c.i2 + 1
            elif n3 < n1 || n3 < n2 then c.i3 <- c.i3 + 1
            else
                let mask = seg1.Mask &&& seg2.Mask &&& seg3.Mask
                if mask <> 0UL then
                    c.descriptor <- SegmentDescriptor<'k>(n1, mask)
                    c.data1 <- SD(seg1.Data, 0)
                    c.data2 <- SD(seg2.Data, 0)
                    c.data3 <- SD(seg3.Data, 0)
                    found <- true
                c.i1 <- c.i1 + 1
                c.i2 <- c.i2 + 1
                c.i3 <- c.i3 + 1
        found
    member c.Reset() =
        c.i1 <- 0; c.i2 <- 0; c.i3 <- 0
    member c.Dispose() = ()
    interface IEnumerator<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3>)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.Dispose() = ()
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()

[<Struct>]
type SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> = 
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    val mutable private i1 : int
    val mutable private i2 : int
    val mutable private i3 : int
    val mutable private i4 : int
    val mutable private descriptor : SegmentDescriptor<'k>
    val mutable private data1 : SD<'s1>
    val mutable private data2 : SD<'s2>
    val mutable private data3 : SD<'s3>
    val mutable private data4 : SD<'s4>
    new(s1, s2, s3, s4) = {
        s1 = s1; s2 = s2; s3 = s3; s4 = s4
        i1 = 0;  i2 = 0;  i3 = 0;  i4 = 0
        descriptor = SegmentDescriptor<'k>(Unchecked.defaultof<'k>, 0UL)
        data1 = Unchecked.defaultof<_>
        data2 = Unchecked.defaultof<_>
        data3 = Unchecked.defaultof<_>
        data4 = Unchecked.defaultof<_>
        }
    member c.GetValue1(i) = &c.data1.[i] 
    member c.GetValue2(i) = &c.data2.[i] 
    member c.GetValue3(i) = &c.data3.[i] 
    member c.GetValue4(i) = &c.data4.[i] 
    member c.Mask = c.descriptor.Mask
    member c.Item with get i =
        struct(c.data1.[i], c.data2.[i], c.data3.[i], c.data4.[i])
    member c.Current =
        struct(c.descriptor, c.data1, c.data2, c.data3, c.data4)
    member c.MoveNext() =
        let mutable found = false
        while not found &&
              c.i1 < c.s1.Count &&
              c.i2 < c.s2.Count &&
              c.i3 < c.s3.Count &&
              c.i4 < c.s4.Count do
            let seg1 = c.s1.[c.i1]
            let seg2 = c.s2.[c.i2]
            let seg3 = c.s3.[c.i3]
            let seg4 = c.s4.[c.i4]
            let n1 = seg1.Id
            let n2 = seg2.Id
            let n3 = seg3.Id
            let n4 = seg4.Id
            if   n1 < n2 || n1 < n3 || n1 < n4 then c.i1 <- c.i1 + 1
            elif n2 < n1 || n2 < n3 || n2 < n4 then c.i2 <- c.i2 + 1
            elif n3 < n1 || n3 < n2 || n3 < n4 then c.i3 <- c.i3 + 1
            elif n4 < n1 || n4 < n2 || n4 < n3 then c.i4 <- c.i4 + 1
            else
                let mask = seg1.Mask &&& seg2.Mask &&& seg3.Mask &&& seg4.Mask
                if mask <> 0UL then
                    c.descriptor <- SegmentDescriptor<'k>(n1, mask)
                    c.data1 <- SD(seg1.Data, 0)
                    c.data2 <- SD(seg2.Data, 0)
                    c.data3 <- SD(seg3.Data, 0)
                    c.data4 <- SD(seg4.Data, 0)
                    found <- true
                c.i1 <- c.i1 + 1
                c.i2 <- c.i2 + 1
                c.i3 <- c.i3 + 1
                c.i4 <- c.i4 + 1
        found
    member c.Reset() =
        c.i1 <- 0; c.i2 <- 0; c.i3 <- 0; c.i4 <- 0
    member c.Dispose() = ()
    interface IEnumerator<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3> * SD<'s4>)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.Dispose() = ()
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()

[<Struct>]
type SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> = 
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    val private s5 : Segments<'k, 's5> 
    val mutable private i1 : int
    val mutable private i2 : int
    val mutable private i3 : int
    val mutable private i4 : int
    val mutable private i5 : int
    val mutable private descriptor : SegmentDescriptor<'k>
    val mutable private data1 : SD<'s1>
    val mutable private data2 : SD<'s2>
    val mutable private data3 : SD<'s3>
    val mutable private data4 : SD<'s4>
    val mutable private data5 : SD<'s5>
    new(s1, s2, s3, s4, s5) = {
        s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5
        i1 = 0;  i2 = 0;  i3 = 0;  i4 = 0;  i5 = 0
        descriptor = SegmentDescriptor<'k>(Unchecked.defaultof<'k>, 0UL)
        data1 = Unchecked.defaultof<_>
        data2 = Unchecked.defaultof<_>
        data3 = Unchecked.defaultof<_>
        data4 = Unchecked.defaultof<_>
        data5 = Unchecked.defaultof<_>
        }
    member c.GetValue1(i) = &c.data1.[i] 
    member c.GetValue2(i) = &c.data2.[i] 
    member c.GetValue3(i) = &c.data3.[i] 
    member c.GetValue4(i) = &c.data4.[i] 
    member c.GetValue5(i) = &c.data5.[i] 
    member c.Mask = c.descriptor.Mask
    member c.Item with get i =
        struct(c.data1.[i], c.data2.[i], c.data3.[i], c.data4.[i], c.data5.[i])
    member c.Current =
        struct(c.descriptor, c.data1, c.data2, c.data3, c.data4, c.data5)
    member c.MoveNext() =
        let mutable found = false
        while not found &&
              c.i1 < c.s1.Count &&
              c.i2 < c.s2.Count &&
              c.i3 < c.s3.Count &&
              c.i4 < c.s4.Count &&
              c.i5 < c.s5.Count do
            let seg1 = c.s1.[c.i1]
            let seg2 = c.s2.[c.i2]
            let seg3 = c.s3.[c.i3]
            let seg4 = c.s4.[c.i4]
            let seg5 = c.s5.[c.i5]
            let n1 = seg1.Id
            let n2 = seg2.Id
            let n3 = seg3.Id
            let n4 = seg4.Id
            let n5 = seg5.Id
            if   n1 < n2 || n1 < n3 || n1 < n4 || n1 < n5 then c.i1 <- c.i1 + 1
            elif n2 < n1 || n2 < n3 || n2 < n4 || n2 < n5 then c.i2 <- c.i2 + 1
            elif n3 < n1 || n3 < n2 || n3 < n4 || n3 < n5 then c.i3 <- c.i3 + 1
            elif n4 < n1 || n4 < n2 || n4 < n3 || n4 < n5 then c.i4 <- c.i4 + 1
            elif n5 < n1 || n5 < n2 || n5 < n3 || n5 < n4 then c.i5 <- c.i5 + 1
            else
                let mask = seg1.Mask &&& seg2.Mask &&& seg3.Mask &&& seg4.Mask &&& seg5.Mask
                if mask <> 0UL then
                    c.descriptor <- SegmentDescriptor<'k>(n1, mask)
                    c.data1 <- SD(seg1.Data, 0)
                    c.data2 <- SD(seg2.Data, 0)
                    c.data3 <- SD(seg3.Data, 0)
                    c.data4 <- SD(seg4.Data, 0)
                    c.data5 <- SD(seg5.Data, 0)
                    found <- true
                c.i1 <- c.i1 + 1
                c.i2 <- c.i2 + 1
                c.i3 <- c.i3 + 1
                c.i4 <- c.i4 + 1
                c.i5 <- c.i5 + 1
        found
    member c.Reset() =
        c.i1 <- 0; c.i2 <- 0; c.i3 <- 0; c.i4 <- 0; c.i5 <- 0
    member c.Dispose() = ()
    interface IEnumerator<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3> * SD<'s4> * SD<'s5>)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.Dispose() = ()
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()

[<Struct>]
type SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> = 
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    val private s5 : Segments<'k, 's5> 
    val private s6 : Segments<'k, 's6> 
    val mutable private i1 : int
    val mutable private i2 : int
    val mutable private i3 : int
    val mutable private i4 : int
    val mutable private i5 : int
    val mutable private i6 : int
    val mutable private descriptor : SegmentDescriptor<'k>
    val mutable private data1 : SD<'s1>
    val mutable private data2 : SD<'s2>
    val mutable private data3 : SD<'s3>
    val mutable private data4 : SD<'s4>
    val mutable private data5 : SD<'s5>
    val mutable private data6 : SD<'s6>
    new(s1, s2, s3, s4, s5, s6) = {
        s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5; s6 = s6
        i1 = 0;  i2 = 0;  i3 = 0;  i4 = 0;  i5 = 0;  i6 = 0
        descriptor = SegmentDescriptor<'k>(Unchecked.defaultof<'k>, 0UL)
        data1 = Unchecked.defaultof<_>
        data2 = Unchecked.defaultof<_>
        data3 = Unchecked.defaultof<_>
        data4 = Unchecked.defaultof<_>
        data5 = Unchecked.defaultof<_>
        data6 = Unchecked.defaultof<_>
        }
    member c.GetValue1(i) = &c.data1.[i] 
    member c.GetValue2(i) = &c.data2.[i] 
    member c.GetValue3(i) = &c.data3.[i] 
    member c.GetValue4(i) = &c.data4.[i] 
    member c.GetValue5(i) = &c.data5.[i] 
    member c.GetValue6(i) = &c.data6.[i] 
    member c.Mask = c.descriptor.Mask
    member c.Item with get i =
        struct(c.data1.[i], c.data2.[i], c.data3.[i], c.data4.[i], c.data5.[i], c.data6.[i])
    member c.Current =
        struct(c.descriptor, c.data1, c.data2, c.data3, c.data4, c.data5, c.data6)
    member c.MoveNext() =
        let mutable found = false
        while not found &&
              c.i1 < c.s1.Count &&
              c.i2 < c.s2.Count &&
              c.i3 < c.s3.Count &&
              c.i4 < c.s4.Count &&
              c.i5 < c.s5.Count &&
              c.i6 < c.s6.Count do
            let seg1 = c.s1.[c.i1]
            let seg2 = c.s2.[c.i2]
            let seg3 = c.s3.[c.i3]
            let seg4 = c.s4.[c.i4]
            let seg5 = c.s5.[c.i5]
            let seg6 = c.s6.[c.i6]
            let n1 = seg1.Id
            let n2 = seg2.Id
            let n3 = seg3.Id
            let n4 = seg4.Id
            let n5 = seg5.Id
            let n6 = seg6.Id
            if   n1 < n2 || n1 < n3 || n1 < n4 || n1 < n5 || n1 < n6 then c.i1 <- c.i1 + 1
            elif n2 < n1 || n2 < n3 || n2 < n4 || n2 < n5 || n2 < n6 then c.i2 <- c.i2 + 1
            elif n3 < n1 || n3 < n2 || n3 < n4 || n3 < n5 || n3 < n6 then c.i3 <- c.i3 + 1
            elif n4 < n1 || n4 < n2 || n4 < n3 || n4 < n5 || n4 < n6 then c.i4 <- c.i4 + 1
            elif n5 < n1 || n5 < n2 || n5 < n3 || n5 < n4 || n5 < n6 then c.i5 <- c.i5 + 1
            elif n6 < n1 || n6 < n2 || n6 < n3 || n6 < n4 || n6 < n5 then c.i6 <- c.i6 + 1
            else
                let mask = seg1.Mask &&& seg2.Mask &&& seg3.Mask &&& seg4.Mask &&& seg5.Mask &&& seg6.Mask
                if mask <> 0UL then
                    c.descriptor <- SegmentDescriptor<'k>(n1, mask)
                    c.data1 <- SD(seg1.Data, 0)
                    c.data2 <- SD(seg2.Data, 0)
                    c.data3 <- SD(seg3.Data, 0)
                    c.data4 <- SD(seg4.Data, 0)
                    c.data5 <- SD(seg5.Data, 0)
                    c.data6 <- SD(seg6.Data, 0)
                    found <- true
                c.i1 <- c.i1 + 1
                c.i2 <- c.i2 + 1
                c.i3 <- c.i3 + 1
                c.i4 <- c.i4 + 1
                c.i5 <- c.i5 + 1
                c.i6 <- c.i6 + 1
        found
    member c.Reset() =
        c.i1 <- 0; c.i2 <- 0; c.i3 <- 0; c.i4 <- 0; c.i5 <- 0; c.i6 <- 0
    member c.Dispose() = ()
    interface IEnumerator<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3> * SD<'s4> * SD<'s5> * SD<'s6>)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.Dispose() = ()
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()

[<Struct>]
type SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> = 
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    val private s5 : Segments<'k, 's5> 
    val private s6 : Segments<'k, 's6> 
    val private s7 : Segments<'k, 's7> 
    val mutable private i1 : int
    val mutable private i2 : int
    val mutable private i3 : int
    val mutable private i4 : int
    val mutable private i5 : int
    val mutable private i6 : int
    val mutable private i7 : int
    val mutable private descriptor : SegmentDescriptor<'k>
    val mutable private data1 : SD<'s1>
    val mutable private data2 : SD<'s2>
    val mutable private data3 : SD<'s3>
    val mutable private data4 : SD<'s4>
    val mutable private data5 : SD<'s5>
    val mutable private data6 : SD<'s6>
    val mutable private data7 : SD<'s7>
    new(s1, s2, s3, s4, s5, s6, s7) = {
        s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5; s6 = s6; s7 = s7
        i1 = 0;  i2 = 0;  i3 = 0;  i4 = 0;  i5 = 0;  i6 = 0;  i7 = 0
        descriptor = SegmentDescriptor<'k>(Unchecked.defaultof<'k>, 0UL)
        data1 = Unchecked.defaultof<_>
        data2 = Unchecked.defaultof<_>
        data3 = Unchecked.defaultof<_>
        data4 = Unchecked.defaultof<_>
        data5 = Unchecked.defaultof<_>
        data6 = Unchecked.defaultof<_>
        data7 = Unchecked.defaultof<_>
        }
    member c.GetValue1(i) = &c.data1.[i] 
    member c.GetValue2(i) = &c.data2.[i] 
    member c.GetValue3(i) = &c.data3.[i] 
    member c.GetValue4(i) = &c.data4.[i] 
    member c.GetValue5(i) = &c.data5.[i] 
    member c.GetValue6(i) = &c.data6.[i] 
    member c.GetValue7(i) = &c.data7.[i] 
    member c.Mask = c.descriptor.Mask
    member c.Item with get i =
        struct(c.data1.[i], c.data2.[i], c.data3.[i], c.data4.[i], c.data5.[i], c.data6.[i], c.data7.[i])
    member c.Current =
        struct(c.descriptor, c.data1, c.data2, c.data3, c.data4, c.data5, c.data6, c.data7)
    member c.MoveNext() =
        let mutable found = false
        while not found &&
              c.i1 < c.s1.Count &&
              c.i2 < c.s2.Count &&
              c.i3 < c.s3.Count &&
              c.i4 < c.s4.Count &&
              c.i5 < c.s5.Count &&
              c.i6 < c.s6.Count &&
              c.i7 < c.s7.Count do
            let seg1 = c.s1.[c.i1]
            let seg2 = c.s2.[c.i2]
            let seg3 = c.s3.[c.i3]
            let seg4 = c.s4.[c.i4]
            let seg5 = c.s5.[c.i5]
            let seg6 = c.s6.[c.i6]
            let seg7 = c.s7.[c.i7]
            let n1 = seg1.Id
            let n2 = seg2.Id
            let n3 = seg3.Id
            let n4 = seg4.Id
            let n5 = seg5.Id
            let n6 = seg6.Id
            let n7 = seg7.Id
            if   n1 < n2 || n1 < n3 || n1 < n4 || n1 < n5 || n1 < n6 || n1 < n7 then c.i1 <- c.i1 + 1
            elif n2 < n1 || n2 < n3 || n2 < n4 || n2 < n5 || n2 < n6 || n2 < n7 then c.i2 <- c.i2 + 1
            elif n3 < n1 || n3 < n2 || n3 < n4 || n3 < n5 || n3 < n6 || n3 < n7 then c.i3 <- c.i3 + 1
            elif n4 < n1 || n4 < n2 || n4 < n3 || n4 < n5 || n4 < n6 || n4 < n7 then c.i4 <- c.i4 + 1
            elif n5 < n1 || n5 < n2 || n5 < n3 || n5 < n4 || n5 < n6 || n5 < n7 then c.i5 <- c.i5 + 1
            elif n6 < n1 || n6 < n2 || n6 < n3 || n6 < n4 || n6 < n5 || n6 < n7 then c.i6 <- c.i6 + 1
            elif n7 < n1 || n7 < n2 || n7 < n3 || n7 < n4 || n7 < n5 || n7 < n6 then c.i7 <- c.i7 + 1
            else
                let mask = seg1.Mask &&& seg2.Mask &&& seg3.Mask &&& seg4.Mask &&& seg5.Mask &&& seg6.Mask &&& seg7.Mask 
                if mask <> 0UL then
                    c.descriptor <- SegmentDescriptor<'k>(n1, mask)
                    c.data1 <- SD(seg1.Data, 0)
                    c.data2 <- SD(seg2.Data, 0)
                    c.data3 <- SD(seg3.Data, 0)
                    c.data4 <- SD(seg4.Data, 0)
                    c.data5 <- SD(seg5.Data, 0)
                    c.data6 <- SD(seg6.Data, 0)
                    c.data7 <- SD(seg7.Data, 0)
                    found <- true
                c.i1 <- c.i1 + 1
                c.i2 <- c.i2 + 1
                c.i3 <- c.i3 + 1
                c.i4 <- c.i4 + 1
                c.i5 <- c.i5 + 1
                c.i6 <- c.i6 + 1
                c.i7 <- c.i7 + 1
        found
    member c.Reset() =
        c.i1 <- 0; c.i2 <- 0; c.i3 <- 0; c.i4 <- 0; c.i5 <- 0; c.i6 <- 0; c.i7 <- 0
    member c.Dispose() = ()
    interface IEnumerator<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3> * SD<'s4> * SD<'s5> * SD<'s6> * SD<'s7>)> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.Dispose() = ()
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()

// ComponentQueryEnumerator

// These enumerate over all Items over all segments, so they combine
// two enumerators (or for loops) into one.

// Note they are reference types and actually return themself during
// enumeration. This is so we can access byref members directly and
// avoid overhead from returning a large struct type on each iteration.
// Although GC impact appears low, we can consider pooling these objects
// to avoid GC overhead if needed.

// Unfortunately, this approach requires access components by numbered
// fields like Value1, Value2, etc. There's an option to read values as
// tuples using 'Values', but this is significantly slower. 

type ComponentQueryEnumerator<'k, 's1 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val mutable private si : SegmentQueryEnumerator<'k, 's1>
    val mutable private m : MaskEnumerator
    new(s1) = {
        si = new SegmentQueryEnumerator<_,_>(s1)
        m = new MaskEnumerator(0UL)
        }
    member c.Value = &c.si.[c.m.Current]
    member c.Current = c
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() =
        if c.m.MoveNext() then true
        else
            let mutable found = c.si.MoveNext() 
            while found && c.si.Mask = 0UL do
                found <- c.si.MoveNext() 
            if found then
                c.m <- new MaskEnumerator(c.si.Mask)
                c.m.MoveNext() |> ignore
            found
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<ComponentQueryEnumerator<'k, 's1>> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

type ComponentQueryEnumerator<'k, 's1, 's2 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val mutable private si : SegmentQueryEnumerator<'k, 's1, 's2>
    val mutable private m : MaskEnumerator
    new(s1, s2) = {
        si = new SegmentQueryEnumerator<_,_,_>(s1, s2)
        m = new MaskEnumerator(0UL)
        }
    member c.Value1 = &c.si.GetValue1(c.m.Current) 
    member c.Value2 = &c.si.GetValue2(c.m.Current)
    member c.Values = c.si.[c.m.Current]
    member c.Current = c
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() =
        if c.m.MoveNext() then true
        else
            let mutable found = c.si.MoveNext() 
            while found && c.si.Mask = 0UL do
                found <- c.si.MoveNext() 
            if found then
                c.m <- new MaskEnumerator(c.si.Mask)
                c.m.MoveNext() |> ignore
            found
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<ComponentQueryEnumerator<'k, 's1, 's2>> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

type ComponentQueryEnumerator<'k, 's1, 's2, 's3 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val mutable private si : SegmentQueryEnumerator<'k, 's1, 's2, 's3>
    val mutable private m : MaskEnumerator
    new(s1, s2, s3) = {
        si = new SegmentQueryEnumerator<_,_,_,_>(s1, s2, s3)
        m = new MaskEnumerator(0UL)
        }
    member c.Value1 = &c.si.GetValue1(c.m.Current) 
    member c.Value2 = &c.si.GetValue2(c.m.Current) 
    member c.Value3 = &c.si.GetValue3(c.m.Current) 
    member c.Values = struct(c.Value1, c.Value2, c.Value3)
    member c.Current = c
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() =
        if c.m.MoveNext() then true
        else
            let mutable found = c.si.MoveNext() 
            while found && c.si.Mask = 0UL do
                found <- c.si.MoveNext() 
            if found then
                c.m <- new MaskEnumerator(c.si.Mask)
                c.m.MoveNext() |> ignore
            found
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<ComponentQueryEnumerator<'k, 's1, 's2, 's3>> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

type ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val mutable private si : SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4>
    val mutable private m : MaskEnumerator
    new(s1, s2, s3, s4) = {
        si = new SegmentQueryEnumerator<_,_,_,_,_>(s1, s2, s3, s4)
        m = new MaskEnumerator(0UL)
        }
    member c.Value1 = &c.si.GetValue1(c.m.Current) 
    member c.Value2 = &c.si.GetValue2(c.m.Current) 
    member c.Value3 = &c.si.GetValue3(c.m.Current) 
    member c.Value4 = &c.si.GetValue4(c.m.Current) 
    member c.Values = c.si.[c.m.Current]
    member c.Current = c
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() =
        if c.m.MoveNext() then true
        else
            let mutable found = c.si.MoveNext() 
            while found && c.si.Mask = 0UL do
                found <- c.si.MoveNext() 
            if found then
                c.m <- new MaskEnumerator(c.si.Mask)
                c.m.MoveNext() |> ignore
            found
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4>> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

type ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val mutable private si : SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5>
    val mutable private m : MaskEnumerator
    new(s1, s2, s3, s4, s5) = {
        si = new SegmentQueryEnumerator<_,_,_,_,_,_>(s1, s2, s3, s4, s5)
        m = new MaskEnumerator(0UL)
        }
    member c.Value1 = &c.si.GetValue1(c.m.Current) 
    member c.Value2 = &c.si.GetValue2(c.m.Current) 
    member c.Value3 = &c.si.GetValue3(c.m.Current) 
    member c.Value4 = &c.si.GetValue4(c.m.Current) 
    member c.Value5 = &c.si.GetValue5(c.m.Current) 
    member c.Values = c.si.[c.m.Current]
    member c.Current = c
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() =
        if c.m.MoveNext() then true
        else
            let mutable found = c.si.MoveNext() 
            while found && c.si.Mask = 0UL do
                found <- c.si.MoveNext() 
            if found then
                c.m <- new MaskEnumerator(c.si.Mask)
                c.m.MoveNext() |> ignore
            found
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5>> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

type ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val mutable private si : SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6>
    val mutable private m : MaskEnumerator
    new(s1, s2, s3, s4, s5, s6) = {
        si = new SegmentQueryEnumerator<_,_,_,_,_,_,_>(s1, s2, s3, s4, s5, s6)
        m = new MaskEnumerator(0UL)
        }
    member c.Value1 = &c.si.GetValue1(c.m.Current) 
    member c.Value2 = &c.si.GetValue2(c.m.Current) 
    member c.Value3 = &c.si.GetValue3(c.m.Current) 
    member c.Value4 = &c.si.GetValue4(c.m.Current) 
    member c.Value5 = &c.si.GetValue5(c.m.Current) 
    member c.Value6 = &c.si.GetValue6(c.m.Current)
    member c.Values = c.si.[c.m.Current]
    member c.Current = c
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() =
        if c.m.MoveNext() then true
        else
            let mutable found = c.si.MoveNext() 
            while found && c.si.Mask = 0UL do
                found <- c.si.MoveNext() 
            if found then
                c.m <- new MaskEnumerator(c.si.Mask)
                c.m.MoveNext() |> ignore
            found
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6>> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

type ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val mutable private si : SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7>
    val mutable private m : MaskEnumerator
    new(s1, s2, s3, s4, s5, s6, s7) = {
        si = new SegmentQueryEnumerator<_,_,_,_,_,_,_,_>(s1, s2, s3, s4, s5, s6, s7)
        m = new MaskEnumerator(0UL)
        }
    member c.Value1 = &c.si.GetValue1(c.m.Current) 
    member c.Value2 = &c.si.GetValue2(c.m.Current) 
    member c.Value3 = &c.si.GetValue3(c.m.Current) 
    member c.Value4 = &c.si.GetValue4(c.m.Current) 
    member c.Value5 = &c.si.GetValue5(c.m.Current) 
    member c.Value6 = &c.si.GetValue6(c.m.Current) 
    member c.Value7 = &c.si.GetValue7(c.m.Current) 
    member c.Values = c.si.[c.m.Current]
    member c.Current = c
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member c.MoveNext() =
        if c.m.MoveNext() then true
        else
            let mutable found = c.si.MoveNext() 
            while found && c.si.Mask = 0UL do
                found <- c.si.MoveNext() 
            if found then
                c.m <- new MaskEnumerator(c.si.Mask)
                c.m.MoveNext() |> ignore
            found
    member c.Reset() = c.m.Reset()
    member c.Dispose() = ()
    interface IEnumerator<ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7>> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

// SegmentQuery

// These each store a set of segment lists for iterating/joining over. Note they
// yield tuples of segments instead of component enumerator which yields a result
// object. This way appears to be faster for segment-level iteration.

[<Struct>]
type SegmentQuery<'k, 's1 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    new(s1) = { s1 = s1 }
    member c.GetEnumerator() = new SegmentQueryEnumerator<'k, 's1>(c.s1)
    member c.GetComponentCount() =
        let mutable count = 0
        let mutable e = c.GetEnumerator()
        while e.MoveNext() do count <- count + Bits.bitCount64 e.Mask
        count
    interface IEnumerable<struct(SegmentDescriptor<'k> * SD<'s1>)> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator

[<Struct>]
type SegmentQuery<'k, 's1, 's2 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    new(s1, s2) = { s1 = s1; s2 = s2 }
    member c.GetEnumerator() = new SegmentQueryEnumerator<'k, 's1, 's2>(c.s1, c.s2)
    member c.GetComponentCount() =
        let mutable count = 0
        let mutable e = c.GetEnumerator()
        while e.MoveNext() do count <- count + Bits.bitCount64 e.Mask
        count
    interface IEnumerable<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2>)> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator
    
[<Struct>]
type SegmentQuery<'k, 's1, 's2, 's3 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    new(s1, s2, s3) = { s1 = s1; s2 = s2; s3 = s3 }
    member c.GetEnumerator() = new SegmentQueryEnumerator<'k, 's1, 's2, 's3>(c.s1, c.s2, c.s3)
    member c.GetComponentCount() =
        let mutable count = 0
        let mutable e = c.GetEnumerator()
        while e.MoveNext() do count <- count + Bits.bitCount64 e.Mask
        count
    interface IEnumerable<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3>)> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator
    
[<Struct>]
type SegmentQuery<'k, 's1, 's2, 's3, 's4 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    new(s1, s2, s3, s4) = { s1 = s1; s2 = s2; s3 = s3; s4 = s4 }
    member c.GetEnumerator() = new SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4>(c.s1, c.s2, c.s3, c.s4)
    member c.GetComponentCount() =
        let mutable count = 0
        let mutable e = c.GetEnumerator()
        while e.MoveNext() do count <- count + Bits.bitCount64 e.Mask
        count
    interface IEnumerable<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3> * SD<'s4>)> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator
    
[<Struct>]
type SegmentQuery<'k, 's1, 's2, 's3, 's4, 's5 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    val private s5 : Segments<'k, 's5> 
    new(s1, s2, s3, s4, s5) = { s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5 }
    member c.GetEnumerator() = new SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5>(c.s1, c.s2, c.s3, c.s4, c.s5)
    member c.GetComponentCount() =
        let mutable count = 0
        let mutable e = c.GetEnumerator()
        while e.MoveNext() do count <- count + Bits.bitCount64 e.Mask
        count
    interface IEnumerable<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3> * SD<'s4> * SD<'s5>)> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator
    
[<Struct>]
type SegmentQuery<'k, 's1, 's2, 's3, 's4, 's5, 's6 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    val private s5 : Segments<'k, 's5> 
    val private s6 : Segments<'k, 's6> 
    new(s1, s2, s3, s4, s5, s6) = { s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5; s6 = s6 }
    member c.GetEnumerator() = new SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6>(c.s1, c.s2, c.s3, c.s4, c.s5, c.s6)
    member c.GetComponentCount() =
        let mutable count = 0
        let mutable e = c.GetEnumerator()
        while e.MoveNext() do count <- count + Bits.bitCount64 e.Mask
        count
    interface IEnumerable<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3> * SD<'s4> * SD<'s5> * SD<'s6>)> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator
    
[<Struct>]
type SegmentQuery<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    val private s5 : Segments<'k, 's5> 
    val private s6 : Segments<'k, 's6> 
    val private s7 : Segments<'k, 's7> 
    new(s1, s2, s3, s4, s5, s6, s7) = { s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5; s6 = s6; s7 = s7 }
    member c.GetEnumerator() = new SegmentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7>(c.s1, c.s2, c.s3, c.s4, c.s5, c.s6, c.s7)
    member c.GetComponentCount() =
        let mutable count = 0
        let mutable e = c.GetEnumerator()
        while e.MoveNext() do count <- count + Bits.bitCount64 e.Mask
        count
    interface IEnumerable<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3> * SD<'s4> * SD<'s5> * SD<'s6> * SD<'s7>)> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator

// ComponentQuery

// These are nearly identical to SegmentQuery, but they provide enumeration over Items
// instead of segments.

[<Struct>]
type ComponentQuery<'k, 's1 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    new(s1) = { s1 = s1 }
    member c.Segments = SegmentQuery<_,_>(c.s1)
    member c.GetEnumerator() = new ComponentQueryEnumerator<'k, 's1>(c.s1)
    member c.GetCount() = c.Segments.GetComponentCount()
    interface IEnumerable<ComponentQueryEnumerator<'k, 's1>> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator

[<Struct>]
type ComponentQuery<'k, 's1, 's2 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    new(s1, s2) = { s1 = s1; s2 = s2 }
    member c.Segments = SegmentQuery<_,_,_>(c.s1, c.s2)
    member c.GetEnumerator() = new ComponentQueryEnumerator<'k, 's1, 's2>(c.s1, c.s2)
    member c.GetCount() = c.Segments.GetComponentCount()
    interface IEnumerable<ComponentQueryEnumerator<'k, 's1, 's2>> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator
    
[<Struct>]
type ComponentQuery<'k, 's1, 's2, 's3 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    new(s1, s2, s3) = { s1 = s1; s2 = s2; s3 = s3 }
    member c.Segments = SegmentQuery<_,_,_,_>(c.s1, c.s2, c.s3)
    member c.GetEnumerator() = new ComponentQueryEnumerator<'k, 's1, 's2, 's3>(c.s1, c.s2, c.s3)
    member c.GetCount() = c.Segments.GetComponentCount()
    interface IEnumerable<ComponentQueryEnumerator<'k, 's1, 's2, 's3>> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator
    
[<Struct>]
type ComponentQuery<'k, 's1, 's2, 's3, 's4 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    new(s1, s2, s3, s4) = { s1 = s1; s2 = s2; s3 = s3; s4 = s4 }
    member c.Segments = SegmentQuery<_,_,_,_,_>(c.s1, c.s2, c.s3, c.s4)
    member c.GetEnumerator() = new ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4>(c.s1, c.s2, c.s3, c.s4)
    member c.GetCount() = c.Segments.GetComponentCount()
    interface IEnumerable<ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4>> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator
    
[<Struct>]
type ComponentQuery<'k, 's1, 's2, 's3, 's4, 's5 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    val private s5 : Segments<'k, 's5> 
    new(s1, s2, s3, s4, s5) = { s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5 }
    member c.Segments = SegmentQuery<_,_,_,_,_,_>(c.s1, c.s2, c.s3, c.s4, c.s5)
    member c.GetEnumerator() = new ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5>(c.s1, c.s2, c.s3, c.s4, c.s5)
    member c.GetCount() = c.Segments.GetComponentCount()
    interface IEnumerable<ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5>> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator
    
[<Struct>]
type ComponentQuery<'k, 's1, 's2, 's3, 's4, 's5, 's6 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    val private s5 : Segments<'k, 's5> 
    val private s6 : Segments<'k, 's6> 
    new(s1, s2, s3, s4, s5, s6) = { s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5; s6 = s6 }
    member c.Segments = SegmentQuery<_,_,_,_,_,_,_>(c.s1, c.s2, c.s3, c.s4, c.s5, c.s6)
    member c.GetEnumerator() = new ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6>(c.s1, c.s2, c.s3, c.s4, c.s5, c.s6)
    member c.GetCount() = c.Segments.GetComponentCount()
    interface IEnumerable<ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6>> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator
    
[<Struct>]
type ComponentQuery<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    val private s3 : Segments<'k, 's3> 
    val private s4 : Segments<'k, 's4> 
    val private s5 : Segments<'k, 's5> 
    val private s6 : Segments<'k, 's6> 
    val private s7 : Segments<'k, 's7> 
    new(s1, s2, s3, s4, s5, s6, s7) = { s1 = s1; s2 = s2; s3 = s3; s4 = s4; s5 = s5; s6 = s6; s7 = s7 }
    member c.Segments = SegmentQuery<_,_,_,_,_,_,_,_>(c.s1, c.s2, c.s3, c.s4, c.s5, c.s6, c.s7)
    member c.GetEnumerator() = new ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7>(c.s1, c.s2, c.s3, c.s4, c.s5, c.s6, c.s7)
    member c.GetCount() = c.Segments.GetComponentCount()
    interface IEnumerable<ComponentQueryEnumerator<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7>> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator

// Extensions

[<AutoOpen>]
module QueryExtensions =
    type ISegmentStore<'k
            when 'k :> IComparable<'k> 
            and 'k :> IEquatable<'k> 
            and 'k : equality> with

        // Segment queries
        
        member c.QuerySegments<'s1>() =
            SegmentQuery<'k, 's1>(
                c.GetSegments<'s1>())
            
        member c.QuerySegments<'s1, 's2>() =
            SegmentQuery<'k, 's1, 's2>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>())
            
        member c.QuerySegments<'s1, 's2, 's3>() =
            SegmentQuery<'k, 's1, 's2, 's3>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>())
            
        member c.QuerySegments<'s1, 's2, 's3, 's4>() =
            SegmentQuery<'k, 's1, 's2, 's3, 's4>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>())
                
        member c.QuerySegments<'s1, 's2, 's3, 's4, 's5>() =
            SegmentQuery<'k, 's1, 's2, 's3, 's4, 's5>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>(),
                c.GetSegments<'s5>())
                
        member c.QuerySegments<'s1, 's2, 's3, 's4, 's5, 's6>() =
            SegmentQuery<'k, 's1, 's2, 's3, 's4, 's5, 's6>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>(),
                c.GetSegments<'s5>(),
                c.GetSegments<'s6>())
                
        member c.QuerySegments<'s1, 's2, 's3, 's4, 's5, 's6, 's7>() =
            SegmentQuery<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>(),
                c.GetSegments<'s5>(),
                c.GetSegments<'s6>(),
                c.GetSegments<'s7>())

        // Component queries
        
        member c.Query<'s1>() =
            ComponentQuery<'k, 's1>(
                c.GetSegments<'s1>())
            
        member c.Query<'s1, 's2>() =
            ComponentQuery<'k, 's1, 's2>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>())
            
        member c.Query<'s1, 's2, 's3>() =
            ComponentQuery<'k, 's1, 's2, 's3>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>())
            
        member c.Query<'s1, 's2, 's3, 's4>() =
            ComponentQuery<'k, 's1, 's2, 's3, 's4>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>())
                
        member c.Query<'s1, 's2, 's3, 's4, 's5>() =
            ComponentQuery<'k, 's1, 's2, 's3, 's4, 's5>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>(),
                c.GetSegments<'s5>())
                
        member c.Query<'s1, 's2, 's3, 's4, 's5, 's6>() =
            ComponentQuery<'k, 's1, 's2, 's3, 's4, 's5, 's6>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>(),
                c.GetSegments<'s5>(),
                c.GetSegments<'s6>())
                
        member c.Query<'s1, 's2, 's3, 's4, 's5, 's6, 's7>() =
            ComponentQuery<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>(),
                c.GetSegments<'s5>(),
                c.GetSegments<'s6>(),
                c.GetSegments<'s7>())
