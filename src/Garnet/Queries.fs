namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
//open System.Numerics
open Garnet.Comparisons

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
        if c.mask = 0UL then false
        else
            while c.mask &&& 1UL = 0UL do
                c.mask <- c.mask >>> 1
                c.i <- c.i + 1
            c.mask <- c.mask >>> 1
            c.i <- c.i + 1
            true
// Alternative for .NET 5.0, although performance seems similar:
//        let skip = BitOperations.TrailingZeroCount(c.mask) + 1
//        c.mask <- c.mask >>> skip
//        c.i <- c.i + skip
//        c.i < 64
    member c.Reset() = c.i <- 0
    member c.Dispose() = ()
    interface IEnumerator<int> with
        member c.Current = c.Current
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
        member c.Reset() = c.Reset()
        member c.Dispose() = ()

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
        Span(c.Array, c.Offset, Segment.segmentSize)
    member c.AsReadOnlySpan() =
        ReadOnlySpan(c.Array, c.Offset, Segment.segmentSize)

type internal SD<'a> = SegmentData<'a>

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

[<Struct>]
type SegmentQuery<'k, 's1 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    new(s1) = { s1 = s1 }
    member c.GetEnumerator() = new SegmentQueryEnumerator<'k, 's1>(c.s1)
    interface IEnumerable<struct(SegmentDescriptor<'k> * SD<'s1>)> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator

[<Struct>]
type SegmentQuery<'k, 's1, 's2 when 'k :> IComparable<'k> and 'k :> IEquatable<'k> and 'k : equality> =
    val private s1 : Segments<'k, 's1> 
    val private s2 : Segments<'k, 's2> 
    new(s1, s2) = { s1 = s1; s2 = s2 }
    member c.GetEnumerator() = new SegmentQueryEnumerator<'k, 's1, 's2>(c.s1, c.s2)
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
    interface IEnumerable<struct(SegmentDescriptor<'k> * SD<'s1> * SD<'s2> * SD<'s3> * SD<'s4> * SD<'s5> * SD<'s6> * SD<'s7>)> with
        member c.GetEnumerator() = c.GetEnumerator() :> IEnumerator<_> 
        member c.GetEnumerator() = c.GetEnumerator() :> Collections.IEnumerator

[<AutoOpen>]
module QueryExtensions =
    type ISegmentStore<'k
            when 'k :> IComparable<'k> 
            and 'k :> IEquatable<'k> 
            and 'k : equality> with
            
        member c.Query<'s1>() =
            SegmentQuery<'k, 's1>(
                c.GetSegments<'s1>())
            
        member c.Query<'s1, 's2>() =
            SegmentQuery<'k, 's1, 's2>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>())
            
        member c.Query<'s1, 's2, 's3>() =
            SegmentQuery<'k, 's1, 's2, 's3>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>())
            
        member c.Query<'s1, 's2, 's3, 's4>() =
            SegmentQuery<'k, 's1, 's2, 's3, 's4>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>())
                
        member c.Query<'s1, 's2, 's3, 's4, 's5>() =
            SegmentQuery<'k, 's1, 's2, 's3, 's4, 's5>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>(),
                c.GetSegments<'s5>())
                
        member c.Query<'s1, 's2, 's3, 's4, 's5, 's6>() =
            SegmentQuery<'k, 's1, 's2, 's3, 's4, 's5, 's6>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>(),
                c.GetSegments<'s5>(),
                c.GetSegments<'s6>())
                
        member c.Query<'s1, 's2, 's3, 's4, 's5, 's6, 's7>() =
            SegmentQuery<'k, 's1, 's2, 's3, 's4, 's5, 's6, 's7>(
                c.GetSegments<'s1>(),
                c.GetSegments<'s2>(),
                c.GetSegments<'s3>(),
                c.GetSegments<'s4>(),
                c.GetSegments<'s5>(),
                c.GetSegments<'s6>(),
                c.GetSegments<'s7>())


