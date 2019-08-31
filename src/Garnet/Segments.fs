namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Text
open System.Runtime.InteropServices
open Garnet.Comparisons
open Garnet.Formatting

[<AutoOpen>]
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
        let newArr = Array.zeroCreate (getNextPow2 required)
        arr.CopyTo(newArr, 0)
        newArr

    let getArrayBitCount64 (masks : uint64[]) count =
        let mutable total = 0
        for i = 0 to count - 1 do
            total <- total + bitCount64 masks.[i]
        total
    
    let formatSegments prefix segments =
        segments
        |> Seq.mapi (fun i x -> sprintf "%d %s" i (x.ToString()))
        |> listToString prefix "" 
        
    let formatBitSegments prefix segments =
        segments
        |> Seq.mapi (fun i x -> sprintf "%d %s" i (x.ToString()))
        |> listToString prefix ""

/// Contiguous 64-element segment with a mask indicating which elements
/// are defined and ID to identify the segment in a sparse collection
[<Struct>]
type Segment<'k, 'a when 'k :> IComparable<'k>> = {
    data : 'a[]
    id : 'k
    mask : uint64
    } with
    override s.ToString() = sprintf "%s %s" (s.id.ToString()) (maskToString 64 s.mask)
    member c.Get(i, fallback) =
        if c.mask &&& (1UL <<< i) <> 0UL then c.data.[i]
        else fallback    

module Segment =
    let segmentBits = 6
    let segmentSize = 1 <<< segmentBits
    let segmentMask = segmentSize - 1
    let init id mask data = { id = id; mask = mask; data = data }

/// 64-bit mask and ID to identify the segment in a sparse collection
[<Struct>]
type internal BitSegment<'k when 'k :> IComparable<'k>> = {
    id : 'k
    mask : uint64
    } with
    static member IsEmpty = Predicate<BitSegment<'k>>(fun s -> s.mask = 0UL)
    override s.ToString() = sprintf "%s %s" (s.id.ToString()) (maskToString 64 s.mask)

module internal BitSegment =
    let init id mask = { id = id; mask = mask }
    
/// Provides a method for accepting a generically-typed segment
type ISegmentHandler =
    abstract member Handle<'k, 'a when 'k :> IComparable<'k>> : Segment<'k, 'a> -> unit
    
type PrintHandler(mask) =
    let sb = StringBuilder()
    let mutable bytes = 0
    let iter action param mask (sa : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then action param sa.[i]
            m <- m >>> 1
            i <- i + 1
    member private c.Print<'a, 'b> (arg : 'a) (x : 'b) = 
        let t = typeof<'b>
        sb.AppendLine() |> ignore
        sb.Append(t.Name) |> ignore
        if not (isEmptyType t) then 
            bytes <- bytes + sizeof<'b>
            sb.Append(sprintf " %A" x) |> ignore
    interface ISegmentHandler with               
        member c.Handle segment =
            iter c.Print () (segment.mask &&& mask) segment.data            
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
        abstract member Handle : ISegmentHandler -> 'k -> uint64 ->unit

    let failComponentOperation op mask conflict (s : Segment<_, 'a>) =
        failwithf "Could not %s %s, sid: %A\n  Requested: %s\n  Existing:  %s\n  Error:     %s"
            op (typeof<'a> |> typeToString) s.id (maskToString 64 mask) 
            (maskToString 64 s.mask) (maskToString 64 conflict)
            
    [<Struct>]
    type PendingSegment<'k, 'a when 'k :> IComparable<'k>> = {
        data : 'a[]
        id : 'k
        mask : uint64
        removalMask : uint64
        }

    /// Ordered list of segments and lookup    
    type ImmediateSegments<'k, 'a 
                when 'k :> IComparable<'k> 
                and 'k : equality>(pool : Stack<_>) =    
        let segments = List<Segment<'k, 'a>>()
        let idToIndex = Dictionary<'k, int>()
        let removeIfEmpty = Predicate<Segment<'k, 'a>>(fun s -> 
            let isEmpty = s.mask = 0UL
            if isEmpty then pool.Push(s.data)
            isEmpty)
        let rebuildLookup() =
            // update entire lookup
            idToIndex.Clear()
            for i = 0 to segments.Count - 1 do
                let s = segments.[i]
                idToIndex.[s.id] <- i        
        member internal c.ComponentCount = 
            let mutable total = 0
            for seg in segments do
                total <- total + bitCount64 seg.mask
            total
        member c.Components = seq {
            for seg in segments do
                let mutable m = seg.mask
                let mutable i = 0
                while m <> 0UL do
                    if m &&& 1UL <> 0UL then 
                        yield seg.data.[i]
                    m <- m >>> 1
                    i <- i + 1
            }            
        member c.Segments = segments :> IEnumerable<_>
        member c.Count = segments.Count  
        /// Takes segment index, not ID
        member c.Item with get i = segments.[i]
        member c.Clear() =
            for s in segments do
                clearArrayMask s.mask s.data
                pool.Push(s.data)
            segments.Clear()
            idToIndex.Clear()
        /// Given a segment ID, returns segment index if found or -1 if not found
        member c.TryFind(id, [<Out>] i : byref<_>) = 
            idToIndex.TryGetValue(id, &i)
        /// Change value of components which are already present
        member c.Set(i, mask) =
            let s = segments.[i]
            let newMask = s.mask ||| mask
            let diff = s.mask ^^^ newMask
            if diff <> 0UL then failComponentOperation "set" mask (mask &&& ~~~s.mask) s
            s.data        
        member c.Add(i, mask) =
            let s = segments.[i]
            let newMask = s.mask ||| mask
            segments.[i] <- Segment.init s.id newMask s.data
            s.data        
        member c.Remove(i, mask) =
            let s = segments.[i]
            let newMask = s.mask &&& ~~~mask
            segments.[i] <- Segment.init s.id newMask s.data
            s.data
        /// Input must be new sorted segments
        member c.MergeFrom(src : PendingSegment<'k, 'a>[], count) =
            if count > 0 then
                let a = segments
                let b = src
                let bCount = count
                // allocate space first
                let origCount = a.Count
                for i = 0 to bCount - 1 do
                    a.Add(Unchecked.defaultof<_>)
                let mutable k = a.Count - 1
                let mutable j = bCount - 1
                let mutable i = origCount - 1
                while k >= 0 do
                    a.[k] <-
                        if j < 0 || (i >= 0 && (a.[i].id.CompareTo(b.[j].id) >= 0)) then
                            let x = a.[i]
                            i <- i - 1
                            x
                        else
                            let x = b.[j]
                            j <- j - 1
                            Segment.init x.id x.mask x.data
                    k <- k - 1
                rebuildLookup()
        /// Removes all empty segments
        member c.Prune() =
            if segments.RemoveAll(removeIfEmpty) > 0 then
                rebuildLookup()            

    type PendingSegments<'k, 'a 
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k>  
        and 'k : equality>(pool : Stack<_>) =
        let comparison = Comparison<PendingSegment<'k, 'a>>(fun a b -> 
            a.id.CompareTo(b.id))
        let comparer = Comparer.Create comparison
        let allocateData =
            // if no members, assume type has a single state and use single array
            // in this case, only bits will be stored
            let t = typeof<'a>
            if isEmptyType t then
                let data = Array.zeroCreate(Segment.segmentSize)
                fun () -> data
            else
                fun () ->
                    if pool.Count > 0 then pool.Pop() 
                    else Array.zeroCreate(Segment.segmentSize)
        let mutable segments = Array.zeroCreate<PendingSegment<'k, 'a>> 8
        let mutable count = 0
        let idToIndex = Dictionary<'k, int>()
        member c.Clear() =
            for i = 0 to count - 1 do
                let seg = segments.[i]
                clearArray seg.data
                pool.Push(seg.data)
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
            | true, i -> segments.[i].mask
            | false, _ -> 0UL
        member c.Add(id, mask) =
            match idToIndex.TryGetValue(id) with
            | true, i ->
                let s = segments.[i]
                segments.[i] <- { 
                    s with 
                        mask = s.mask ||| mask
                        removalMask = s.removalMask &&& ~~~mask 
                        }
                s.data        
            | false, _ ->
                let i = count
                if count = segments.Length then
                    segments <- resizeArray segments (count + 1)
                let data = allocateData()
                segments.[i] <- { 
                    id = id
                    mask = mask
                    data = data
                    removalMask = 0UL
                    }
                count <- count + 1
                idToIndex.Add(id, i)
                data
        /// Removes bits given ID, but does not remove segment if empty
        member c.Remove(id, mask) =
            match idToIndex.TryGetValue(id) with
            | true, i ->
                let s = segments.[i]
                let newMask = s.mask &&& ~~~mask
                segments.[i] <- { 
                s with 
                    mask = newMask
                    removalMask = s.removalMask ||| mask 
                    }
            | false, _ ->
                let i = count
                if count = segments.Length then
                    segments <- resizeArray segments (count + 1)
                let data = allocateData()
                segments.[i] <- { 
                    id = id
                    mask = 0UL
                    data = data
                    removalMask = mask
                    }
                count <- count + 1
                idToIndex.Add(id, i)
        member c.ApplyRemovalsTo(target : ISegments<_>) =
            // copy in case this is called on self
            let count = count
            for i = 0 to count - 1 do
                let delta = segments.[i]
                if delta.removalMask <> 0UL then
                    target.Remove(delta.id, delta.removalMask)           
        member c.FlushTo(target : ImmediateSegments<'k, 'a>) =
            // first copy into existing, removing deltas
            let mutable di = 0
            while di < count do
                let delta = segments.[di]
                // if not found, skip past for now
                match target.TryFind(delta.id) with
                | false, _ -> di <- di + 1
                | true, i -> 
                    // apply removal
                    if delta.removalMask <> 0UL then
                        let data = target.Remove(i, delta.removalMask)
                        clearArrayMask delta.removalMask data
                    // apply addition
                    if delta.mask <> 0UL then
                        // copy into existing
                        let data = target.Add(i, delta.mask)
                        copyArrayMask delta.mask delta.data data
                        clearArrayMask delta.mask delta.data
                    // remove from deltas
                    Array.Clear(delta.data, 0, delta.data.Length)
                    pool.Push(delta.data)
                    count <- count - 1
                    segments.[di] <- segments.[count]
                    segments.[count] <- Unchecked.defaultof<_>
            // remaining are all new segments
            Array.Sort(segments, 0, count, comparer)
            target.MergeFrom(segments, count)
            // clear without recycling to pool since we passed ownership
            Array.Clear(segments, 0, count)
            count <- 0
            target.Prune()
            c.Clear()
        member c.ToString(formatSegments, formatBitSegments) =
            let additions = 
                segments 
                |> Seq.filter (fun s -> s.mask <> 0UL)
                |> Seq.map (fun s -> Segment.init s.id s.mask s.data)
                |> Seq.toArray
            let removals = 
                segments 
                |> Seq.filter (fun s -> s.removalMask <> 0UL)
                |> Seq.map (fun s -> BitSegment.init s.id s.removalMask)
                |> Seq.toArray
            sprintf "%d/%dA %d/%dR%s%s"
                (additions |> Seq.sumBy (fun s -> bitCount64 s.mask)) additions.Length
                (removals |> Seq.sumBy (fun s -> bitCount64 s.mask)) removals.Length
                (formatSegments ("  A") (additions :> seq<_>))
                (formatBitSegments ("  R") (removals :> seq<_>))
        override c.ToString() =
            c.ToString(formatSegments, formatBitSegments)

/// Sparse list of segments
type Segments<'k, 'a 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality>() =    
    let pool = Stack<_>()
    let pending = PendingSegments<'k, 'a>(pool)
    let current = ImmediateSegments<'k, 'a>(pool)
    member internal c.PendingCount = pending.Count
    member internal c.GetPending i = pending.[i]
    /// Returns a sequence of the components present
    member internal c.Components = current.Components
    /// Number of current segments
    member c.Count = current.Count        
    /// Takes segment index, not ID
    member c.Item with get i = current.[i]
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
        member c.Commit() = c.Commit()
        member c.Handle handler sid mask =
            match c.TryFind(sid) with
            | false, _ -> ()
            | true, si ->
                let seg = current.[si]
                let masked = Segment.init seg.id (seg.mask &&& mask) seg.data
                handler.Handle masked
    member internal c.ToString(formatSegments, formatBitSegments) =
        let prefix = ""
        let pendingStr = pending.ToString(formatSegments, formatBitSegments)
        sprintf "%s: %d/%dC %s%s"
            (typeof<'a> |> typeToString)
            current.ComponentCount current.Count
            (formatSegments (prefix + "  C") current.Segments)
            (if pendingStr.Length > 0 then "\n" + pendingStr else "")
    override c.ToString() =
        c.ToString(formatSegments, formatBitSegments)

type Segments<'k, 'a
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality> with
    /// Returns mask if present, zero otherwise
    member c.GetMask sid =
        match c.TryFind(sid) with
        | true, i -> c.[i].mask
        | false, _ -> 0UL
    member c.Contains(sid) =
        let mutable i = 0
        c.TryFind(sid, &i)          
    member c.Get(sid) =
        match c.TryFind(sid) with
        | true, si -> c.[si]
        | false, _ -> failwithf "Cannot get %s segment %A" (typeToString typeof<'a>) sid
    /// Removes entire segment
    member c.Remove(sid) =
        match c.TryFind(sid) with
        | false, _ -> ()
        | true, si ->
            let mask = c.[si].mask
            c.Remove(sid, mask)
    /// Marks all segemnts for removal, which is different than immediate Clear()
    member c.RemoveAll() =
        for i = 0 to c.Count - 1 do
            let seg = c.[i]
            c.Remove(seg.id, seg.mask)

type ISegmentStore<'k
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality> =
    abstract member GetSegments<'b> : unit -> Segments<'k, 'b>
        
type SegmentStore<'k 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality>() =
    let lookup = Dictionary<Type, ISegments<'k>>()
    member c.GetSegments<'a>() =
        let t = typeof<'a>
        match lookup.TryGetValue(t) with
        | true, segs -> segs :?> Segments<'k, 'a>
        | false, _ ->
            let segs = Segments<'k, 'a>()
            lookup.Add(t, segs)
            segs
    member c.Clear() =
        for segs in lookup.Values do
            segs.Clear()
    member c.Remove(sid, mask) =
        for segs in lookup.Values do
            segs.Remove(sid, mask)
    member c.Handle(sid, mask, handler) =      
        for s in lookup.Values do
            s.Handle handler sid mask
    member c.Commit() =
        for segs in lookup.Values do
            segs.Commit()
    member c.ApplyRemovalsFrom (segments : Segments<_,_>) =
        for segs in lookup.Values do
            segments.ApplyRemovalsTo segs
    interface ISegmentStore<'k> with
        member c.GetSegments<'a>() = 
            c.GetSegments<'a>()
    override c.ToString() =
        let prefix = ""
        lookup.Values
        |> Seq.map (fun item -> item.ToString().Replace("\n", "\n  "))
        |> listToString (prefix + "  ") (c.GetType() |> typeToString)
