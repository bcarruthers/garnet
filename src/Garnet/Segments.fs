namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Text
open System.Runtime.InteropServices
open System.Threading
open Garnet
open Garnet.Comparisons
open Garnet.Collections
open Garnet.Formatting

module internal Bits =
    let inline bitCount x =
        let x = x - ((x >>> 1) &&& 0x55555555)
        let x = (x &&& 0x33333333) + ((x >>> 2) &&& 0x33333333)
        (((x + (x >>> 4)) &&& 0x0F0F0F0F) * 0x01010101) >>> 24

    let inline bitCount64 (x : uint64) =
        bitCount (int (x >>> 32)) + bitCount (int (x &&& 0xffffffffUL))

    let bitCount64Array (arr : uint64[]) =
        let mutable total = 0
        for x in arr do
            total <- total + bitCount64 x
        total

    let inline getNextPow2 x =
        let mutable y = x - 1
        y <- y ||| (y >>> 1)
        y <- y ||| (y >>> 2)
        y <- y ||| (y >>> 4)
        y <- y ||| (y >>> 8)
        y <- y ||| (y >>> 16)
        y <- y + 1
        if y > 0 then y else 1
        
[<AutoOpen>]
module internal Utility =
    let copyArrayMask mask (src : _[]) (dest : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then dest.[i] <- src.[i]
            m <- m >>> 1
            i <- i + 1

    let clearArray (arr : _[]) =
        Array.Clear(arr, 0, arr.Length)
        
    let clearArrayMask mask (arr : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then arr.[i] <- Unchecked.defaultof<_>
            m <- m >>> 1
            i <- i + 1

    let resizeArray (arr : _[]) required =
        let newArr = Array.zeroCreate (Bits.getNextPow2 required)
        arr.CopyTo(newArr, 0)
        newArr

    let getArrayBitCount64 (masks : uint64[]) count =
        let mutable total = 0
        for i = 0 to count - 1 do
            total <- total + Bits.bitCount64 masks.[i]
        total
    
    let formatIndexedList prefix (segments : ReadOnlyMemory<_>) =
        segments.ToArray()
        |> Seq.mapi (fun i x -> sprintf "%d %s" i (x.ToString()))
        |> Format.listToString prefix "" 
    
/// Contiguous 64-element segment with a mask indicating which elements
/// are defined and ID to identify the segment in a sparse collection
[<Struct>]
type Segment<'k, 'a when 'k :> IComparable<'k>> = {
    Data : 'a[]
    Id : 'k
    Mask : uint64
    } with
    override s.ToString() = sprintf "%s %s" (s.Id.ToString()) (Format.maskToString 64 s.Mask)
    member c.Get(i, fallback) =
        if c.Mask &&& (1UL <<< i) <> 0UL then c.Data.[i]
        else fallback    

module Segment =
    let segmentBits = 6
    let segmentSize = 1 <<< segmentBits
    let segmentMask = segmentSize - 1
    let init id mask data = { Id = id; Mask = mask; Data = data }

/// 64-bit mask and ID to identify the segment in a sparse collection
[<Struct>]
type internal BitSegment<'k when 'k :> IComparable<'k>> = {
    Id : 'k
    Mask : uint64
    } with
    static member IsEmpty = Predicate<BitSegment<'k>>(fun s -> s.Mask = 0UL)
    override s.ToString() = sprintf "%s %s" (s.Id.ToString()) (Format.maskToString 64 s.Mask)

module internal BitSegment =
    let init id mask = { Id = id; Mask = mask }
    
/// Provides a method for accepting a generically-typed segment
type ISegmentHandler<'p, 'k
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> =
    abstract member Handle<'a> : 'p * Segment<'k, 'a> -> unit

type ISegmentListHandler<'p, 'k
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> =
    abstract member Handle<'a> : 'p * ReadOnlyMemory<Segment<'k, 'a>> -> unit
    
type PrintHandler<'k
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality>(mask) =
    let sb = StringBuilder()
    let mutable bytes = 0
    let iter action param mask (sa : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then action param sa.[i]
            m <- m >>> 1
            i <- i + 1
    member private c.Print<'a, 'b> (_ : 'a) (x : 'b) = 
        let t = typeof<'b>
        sb.AppendLine() |> ignore
        sb.Append(t.Name) |> ignore
        if not (Format.isEmptyType t) then 
            bytes <- bytes + sizeof<'b>
            sb.Append(sprintf " %A" x) |> ignore
    interface ISegmentHandler<unit, 'k> with               
        member c.Handle((), segment) =
            iter c.Print () (segment.Mask &&& mask) segment.Data            
    interface ISegmentListHandler<unit, 'k> with               
        member c.Handle((), segments) =
            for segment in segments.Span do
                iter c.Print () (segment.Mask &&& mask) segment.Data            
    override c.ToString() =
        sprintf "%d bytes" bytes
        + sb.ToString()    

[<AutoOpen>]
module internal Internal =
    type ISegments<'k 
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> =
        abstract member Clear : unit -> unit
        abstract member TryFind : 'k * byref<int> -> bool
        abstract member Remove : 'k * uint64 -> unit
        abstract member Commit : unit -> unit
        abstract member Handle<'p> : 'p * ISegmentListHandler<'p, 'k> ->unit
        abstract member Handle<'p> : 'p * ISegmentHandler<'p, 'k> * 'k * uint64 ->unit

    let failComponentOperation op mask conflict (s : Segment<_, 'a>) =
        failwithf "Could not %s %s, sid: %A\n  Requested: %s\n  Existing:  %s\n  Error:     %s"
            op (typeof<'a> |> Format.typeToString) s.Id (Format.maskToString 64 mask) 
            (Format.maskToString 64 s.Mask) (Format.maskToString 64 conflict)
            
    [<Struct>]
    type PendingSegment<'k, 'a when 'k :> IComparable<'k>> = {
        Data : 'a[]
        Id : 'k
        mutable Mask : uint64
        mutable RemovalMask : uint64
        }

    /// Ordered list of segments and lookup    
    type CurrentSegments<'k, 'a 
            when 'k :> IComparable<'k> 
            and 'k :> IEquatable<'k> 
            and 'k : equality>(pool : Stack<'a[]>) =    
        let mutable segments = Array.zeroCreate<Segment<'k, 'a>> 8
        let mutable count = 0
        let idToIndex = DictionarySlim<'k, int>()
        member internal c.ComponentCount = 
            let mutable total = 0
            for i = 0 to count - 1 do
                let seg = segments.[i]
                total <- total + Bits.bitCount64 seg.Mask
            total
        member c.Components = seq {
            for i = 0 to count - 1 do
                let seg = segments.[i]
                let mutable m = seg.Mask
                let mutable i = 0
                while m <> 0UL do
                    if m &&& 1UL <> 0UL then 
                        yield seg.Data.[i]
                    m <- m >>> 1
                    i <- i + 1
            }            
        member c.Segments = 
            ReadOnlyMemory(segments).Slice(0, count)
        member c.Count = count
        /// Takes segment index, not ID
        member c.Item with get i = segments.[i]
        member c.Clear() =
            for i = 0 to count - 1 do
                let seg = segments.[i]
                clearArrayMask seg.Mask seg.Data
                pool.Push(seg.Data)
            Array.Clear(segments, 0, count)
            count <- 0
            idToIndex.Clear()
        member c.Handle<'p>(param, handler : ISegmentListHandler<'p, 'k>) =
            handler.Handle(param, c.Segments)
        /// Given a segment ID, returns segment index if found or -1 if not found
        member c.TryFind(id, [<Out>] i : byref<_>) = 
            idToIndex.TryGetValue(id, &i)
        /// Change value of components which are already present
        member c.Set(i, mask) =
            let s = segments.[i]
            let newMask = s.Mask ||| mask
            let diff = s.Mask ^^^ newMask
            if diff <> 0UL then failComponentOperation "set" mask (mask &&& ~~~s.Mask) s
            s.Data        
        member c.Add(i, mask) =
            let s = segments.[i]
            let newMask = s.Mask ||| mask
            segments.[i] <- Segment.init s.Id newMask s.Data
            s.Data        
        member c.Remove(i, mask) =
            let s = segments.[i]
            let newMask = s.Mask &&& ~~~mask
            segments.[i] <- Segment.init s.Id newMask s.Data
            s.Data
        /// Input must be new sorted segments
        member c.MergeFrom(src : PendingSegment<'k, 'a>[], srcCount) =
            // add new segments
            let hasAdded = srcCount > 0
            if hasAdded then
                // allocate space first
                let origCount = count
                count <- count + srcCount
                Buffer.resizeArray count &segments
                let a = segments
                let b = src
                let mutable k = count - 1
                let mutable j = srcCount - 1
                let mutable i = origCount - 1
                while k >= 0 do
                    a.[k] <-
                        if j < 0 || (i >= 0 && (a.[i].Id.CompareTo(b.[j].Id) >= 0)) then
                            let x = a.[i]
                            i <- i - 1
                            x
                        else
                            let x = b.[j]
                            j <- j - 1
                            Segment.init x.Id x.Mask x.Data
                    k <- k - 1
            // remove any empty segments
            let mutable iDest = 0
            for iSrc = 0 to count - 1 do
                let seg = segments.[iSrc]
                if seg.Mask = 0UL then
                    pool.Push(seg.Data)
                else
                    segments.[iDest] <- seg
                    iDest <- iDest + 1
            let hasRemoved = iDest < count
            if hasRemoved then
                Array.Clear(segments, iDest, count - iDest)
                count <- iDest
            // rebuild lookup
            if hasAdded || hasRemoved then
                idToIndex.Clear()
                for i = 0 to count - 1 do
                    let seg = segments.[i]
                    let index = &idToIndex.GetOrAddValueRef(&seg.Id)
                    index <- i        

    type PendingSegments<'k, 'a 
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k>  
        and 'k : equality>(pool : Stack<_>) =
        let comparison = Comparison<PendingSegment<'k, 'a>>(fun a b -> 
            a.Id.CompareTo(b.Id))
        let comparer = Comparer.Create comparison
        let allocateData =
            // if no members, assume type has a single state and use single array
            // in this case, only bits will be stored
            let t = typeof<'a>
            if Format.isEmptyType t then
                let data = Array.zeroCreate(Segment.segmentSize)
                fun () -> data
            else
                fun () ->
                    if pool.Count > 0 then pool.Pop() 
                    else Array.zeroCreate(Segment.segmentSize)
        let mutable segments = Array.zeroCreate<PendingSegment<'k, 'a>> 8
        let mutable count = 0
        let idToIndex = DictionarySlim<'k, int>()
        member c.Clear() =
            for i = 0 to count - 1 do
                let seg = segments.[i]
                clearArray seg.Data
                pool.Push(seg.Data)
            count <- 0
            idToIndex.Clear()
        member c.Item with get i = segments.[i]
        member internal c.Count = count        
        member internal c.Segments = seq {
            for i = 0 to count - 1 do
                yield c.[i]
            }
        member c.GetMask(id) =
            match idToIndex.TryGetValue(id) with
            | true, i -> segments.[i].Mask
            | false, _ -> 0UL
        member c.Add(id, mask) =
            match idToIndex.TryGetValue(id) with
            | true, i ->
                let s = &segments.[i]
                s.Mask <- s.Mask ||| mask
                s.RemovalMask <- s.RemovalMask &&& ~~~mask
                s.Data        
            | false, _ ->
                let i = count
                if count = segments.Length then
                    segments <- resizeArray segments (count + 1)
                let data = allocateData()
                segments.[i] <- { 
                    Id = id
                    Mask = mask
                    Data = data
                    RemovalMask = 0UL
                    }
                count <- count + 1
                let index = &idToIndex.GetOrAddValueRef(&id)
                index <- i
                data
        /// Removes bits given ID, but does not remove segment if empty
        member c.Remove(id, mask) =
            match idToIndex.TryGetValue(id) with
            | true, i ->
                let s = &segments.[i]
                s.Mask <- s.Mask &&& ~~~mask
                s.RemovalMask <- s.RemovalMask ||| mask 
            | false, _ ->
                let i = count
                if count = segments.Length then
                    segments <- resizeArray segments (count + 1)
                let data = allocateData()
                segments.[i] <- { 
                    Id = id
                    Mask = 0UL
                    Data = data
                    RemovalMask = mask
                    }
                count <- count + 1
                let index = &idToIndex.GetOrAddValueRef(&id)
                index <- i
        member c.ApplyRemovalsTo(target : ISegments<_>) =
            // copy in case this is called on self
            let count = count
            for i = 0 to count - 1 do
                let delta = segments.[i]
                if delta.RemovalMask <> 0UL then
                    target.Remove(delta.Id, delta.RemovalMask)           
        member c.FlushTo(target : CurrentSegments<'k, 'a>) =
            // first copy into existing, removing deltas
            let mutable di = 0
            while di < count do
                let delta = segments.[di]
                // if not found, skip past for now
                match target.TryFind(delta.Id) with
                | false, _ -> di <- di + 1
                | true, i -> 
                    // apply removal
                    if delta.RemovalMask <> 0UL then
                        let data = target.Remove(i, delta.RemovalMask)
                        clearArrayMask delta.RemovalMask data
                    // apply addition
                    if delta.Mask <> 0UL then
                        // copy into existing
                        let data = target.Add(i, delta.Mask)
                        copyArrayMask delta.Mask delta.Data data
                        clearArrayMask delta.Mask delta.Data
                    // remove from deltas
                    Array.Clear(delta.Data, 0, delta.Data.Length)
                    pool.Push(delta.Data)
                    count <- count - 1
                    segments.[di] <- segments.[count]
                    segments.[count] <- Unchecked.defaultof<_>
            // remaining are all new segments
            Array.Sort(segments, 0, count, comparer)
            target.MergeFrom(segments, count)
            // clear without recycling to pool since we passed ownership
            Array.Clear(segments, 0, count)
            count <- 0
            c.Clear()
        member c.ToString(formatSegments, formatBitSegments) =
            let additions = 
                segments 
                |> Seq.filter (fun s -> s.Mask <> 0UL)
                |> Seq.map (fun s -> Segment.init s.Id s.Mask s.Data)
                |> Seq.toArray
            let removals = 
                segments 
                |> Seq.filter (fun s -> s.RemovalMask <> 0UL)
                |> Seq.map (fun s -> BitSegment.init s.Id s.RemovalMask)
                |> Seq.toArray
            sprintf "%d/%dA %d/%dR%s%s"
                (additions |> Seq.sumBy (fun s -> Bits.bitCount64 s.Mask)) additions.Length
                (removals |> Seq.sumBy (fun s -> Bits.bitCount64 s.Mask)) removals.Length
                (formatSegments "  A" (ReadOnlyMemory(additions)))
                (formatBitSegments "  R" (ReadOnlyMemory(removals)))
        override c.ToString() =
            c.ToString(formatIndexedList, formatIndexedList)

/// Sparse list of segments
type Segments<'k, 'a 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality>() =    
    let pool = Stack<_>()
    let pending = PendingSegments<'k, 'a>(pool)
    let current = CurrentSegments<'k, 'a>(pool)
    member internal c.PendingCount = pending.Count
    member internal c.GetPending i = pending.[i]
    /// Returns a sequence of the components present
    member internal c.Components = current.Components
    member c.GetSpan() = 
        current.Segments.Span
    member c.GetMemory() = 
        current.Segments
    /// Number of current segments
    member c.Count = current.Count        
    /// Takes segment index, not ID
    member c.Item with get i = current.[i]
    /// Returns the number of current components stored
    member c.GetComponentCount() = 
        let mutable total = 0
        for i = 0 to c.Count - 1 do
            total <- total + Bits.bitCount64 c.[i].Mask
        total
    /// Given a segment ID, returns true if the segment is present and assigns its index
    member c.TryFind(sid, [<Out>] i : byref<_>) = 
        current.TryFind(sid, &i)
    /// Immediately clears all current and pending data
    member c.Clear() =
        pending.Clear()
        current.Clear()
    /// Sets mask for a segment ID
    member c.Set(sid, mask) =
        match c.TryFind(sid) with
        | true, i -> current.Set(i, mask)
        | false, _ -> failwithf "Segment %A not present" sid
    /// Returns segment so data can be filled
    /// Assume either not present or in removal list
    member c.Add(sid, mask) =
        pending.Add(sid, mask)
    /// Assumes present, not considering addition list
    member c.Remove(sid, mask) =
        pending.Remove(sid, mask)
    /// Commits any pending changes and removes empty segments
    member c.Commit() = 
        pending.FlushTo(current)
    member internal c.ApplyRemovalsTo segments =
        if not (obj.ReferenceEquals(c, segments)) then
            pending.ApplyRemovalsTo segments
    interface ISegments<'k> with
        member c.Clear() =
            c.Clear()
        member c.TryFind(sid, [<Out>] i : byref<_>) = 
            current.TryFind(sid, &i)
        member c.Remove(sid, mask) =
            c.Remove(sid, mask)
        member c.Commit() = 
            c.Commit()
        member c.Handle<'p>(param, handler : ISegmentListHandler<'p, 'k>) =
            current.Handle(param, handler)
        member c.Handle(param, handler, sid, mask) =
            match c.TryFind(sid) with
            | false, _ -> ()
            | true, si ->
                let seg = current.[si]
                let masked = Segment.init seg.Id (seg.Mask &&& mask) seg.Data
                handler.Handle(param, masked)
    member internal c.ToString(formatSegments, formatBitSegments) =
        let prefix = ""
        let pendingStr = pending.ToString(formatSegments, formatBitSegments)
        sprintf "%s: %d/%dC %s%s"
            (typeof<'a> |> Format.typeToString)
            current.ComponentCount current.Count
            (formatSegments (prefix + "  C") current.Segments)
            (if pendingStr.Length > 0 then "\n" + pendingStr else "")
    override c.ToString() =
        c.ToString(formatIndexedList, formatIndexedList)

type Segments<'k, 'a
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality> with
    /// Returns mask if present, zero otherwise
    member c.GetMask sid =
        match c.TryFind(sid) with
        | true, i -> c.[i].Mask
        | false, _ -> 0UL
    member c.Contains(sid) =
        let mutable i = 0
        c.TryFind(sid, &i)          
    member c.Get(sid) =
        match c.TryFind(sid) with
        | true, si -> c.[si]
        | false, _ -> failwithf "Cannot get %s segment %A" (Format.typeToString typeof<'a>) sid
    /// Removes entire segment
    member c.Remove(sid) =
        match c.TryFind(sid) with
        | false, _ -> ()
        | true, si ->
            let mask = c.[si].Mask
            c.Remove(sid, mask)
    /// Marks all segments for removal, which is different than immediate Clear()
    member c.RemoveAll() =
        for i = 0 to c.Count - 1 do
            let seg = c.[i]
            c.Remove(seg.Id, seg.Mask)
    member c.GetSegmentOrEmpty(sid) =
        match c.TryFind(sid) with
        | true, i -> c.[i]
        | false, _ -> Segment.init sid 0UL null
    /// Given a segment ID, returns segment index if found or -1 if not found
    member c.Find(sid) = 
        match c.TryFind(sid) with
        | true, i -> i
        | false, _ -> -1

type ISegmentStore<'k
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality> =
    abstract member GetSegments<'b> : unit -> Segments<'k, 'b>
    abstract member Handle<'p> : 'p * ISegmentListHandler<'p, 'k> -> unit
    abstract member Handle<'p> : 'p * 'k * uint64 * ISegmentHandler<'p, 'k> -> unit

type CopyHandler<'k
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality>() =
    static let mutable instance = CopyHandler<'k>()
    static member Instance = instance
    interface ISegmentHandler<ISegmentStore<'k>, 'k> with               
        member c.Handle<'a>(store, segment : Segment<'k, 'a>) =
            let dest = store.GetSegments<'a>()
            let data = dest.Add(segment.Id, segment.Mask)
            segment.Data.CopyTo(data, 0)
    interface ISegmentListHandler<ISegmentStore<'k>, 'k> with               
        member c.Handle<'a>(store, src : ReadOnlyMemory<Segment<'k, 'a>>) =
            let dest = store.GetSegments<'a>()
            for seg in src.Span do
                let data = dest.Add(seg.Id, seg.Mask)
                seg.Data.CopyTo(data, 0)

type internal ComponentTypeId() =
    static let mutable id  = 0
    static member GetNext() = Interlocked.Increment(&id)

type internal ComponentTypeId<'a>() =
    static let mutable id = ComponentTypeId.GetNext()
    static member Id = id
        
type SegmentStore<'k 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality>() =
    let segmentLists = List<ISegments<'k>>()
    let mutable lookup = Array.zeroCreate<ISegments<'k>>(8)
    member c.GetSegments<'a>() =
        let id = ComponentTypeId<'a>.Id
        if id >= lookup.Length then
            Buffer.resizeArray (id + 1) &lookup
        let segs = lookup.[id]
        if isNotNull segs then segs :?> Segments<'k, 'a>
        else            
            let segs = Segments<'k, 'a>()
            lookup.[id] <- segs :> ISegments<'k>
            segmentLists.Add(segs)
            segs
    member c.Clear() =
        for segs in segmentLists do
            segs.Clear()
    member c.Remove(sid, mask) =
        for segs in segmentLists do
            segs.Remove(sid, mask)
    member c.Handle(param, handler : ISegmentListHandler<_,_>) =      
        for s in segmentLists do
            s.Handle(param, handler)
    member c.Handle(param, sid, mask, handler : ISegmentHandler<_,_>) =      
        for s in segmentLists do
            s.Handle(param, handler, sid, mask)
    member c.Commit() =
        for segs in segmentLists do
            segs.Commit()
    member c.ApplyRemovalsFrom (segments : Segments<_,_>) =
        if segments.PendingCount > 0 then
            for segs in segmentLists do
                segments.ApplyRemovalsTo(segs)
    interface ISegmentStore<'k> with
        member c.Handle(param, handler : ISegmentListHandler<_,_>) =      
            c.Handle(param, handler)
        member c.Handle(param, sid, mask, handler : ISegmentHandler<_,_>) =      
            c.Handle(param, sid, mask, handler)
        member c.GetSegments<'a>() = 
            c.GetSegments<'a>()
    override c.ToString() =
        let prefix = ""
        segmentLists
        |> Seq.map (fun item -> item.ToString().Replace("\n", "\n  "))
        |> Format.listToString (prefix + "  ") (c.GetType() |> Format.typeToString)

[<AutoOpen>]
module SegmentStore =
    type ISegmentStore<'k
           when 'k :> IComparable<'k> 
           and 'k :> IEquatable<'k> 
           and 'k : equality> with
           
        member c.CopyTo(dest : ISegmentStore<'k>) =
            let handler = CopyHandler.Instance :> ISegmentListHandler<_,_>
            c.Handle(dest, handler)
            
        member c.CopyTo(dest : ISegmentStore<'k>, sid, mask) =
            let handler = CopyHandler.Instance :> ISegmentHandler<_,_>
            c.Handle(dest, sid, mask, handler)            