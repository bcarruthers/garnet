namespace Garnet.Ecs

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
    /// Merges into first list. Assumes both are ordered
    let mergeInto (a : List<_>) (b : _[]) bCount (comparison : Comparison<_>) =
        // allocate space first
        let origCount = a.Count
        for i = 0 to bCount - 1 do
            a.Add(Unchecked.defaultof<_>)
        let mutable k = a.Count - 1
        let mutable j = bCount - 1
        let mutable i = origCount - 1
        while k >= 0 do
            a.[k] <-
                if j < 0 || (i >= 0 && (comparison.Invoke(a.[i], b.[j]) >= 0)) then
                    let x = a.[i]
                    i <- i - 1
                    x
                else
                    let x = b.[j]
                    j <- j - 1
                    x
            k <- k - 1

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

[<Struct>]
type Segment<'k, 'a when 'k :> IComparable<'k>> = {
    data : 'a[]
    id : 'k
    mask : uint64
    } with
    override s.ToString() = sprintf "%A %s" s.id (maskToString 64 s.mask)
    member c.Get(i, fallback) =
        if c.mask &&& (1UL <<< i) <> 0UL then c.data.[i]
        else fallback    

[<Struct>]
type BitSegment<'k when 'k :> IComparable<'k>> = {
    id : 'k
    mask : uint64
    } with
    static member IsEmpty = Predicate<BitSegment<'k>>(fun s -> s.mask = 0UL)
    override s.ToString() = sprintf "%A %s" s.id (maskToString 64 s.mask)

module Segment =
    let segmentBits = 6
    let segmentSize = 1 <<< segmentBits
    let segmentMask = segmentSize - 1
    let init id mask data = { id = id; mask = mask; data = data }

module BitSegment =
    let init id mask = { id = id; mask = mask }

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
    let failComponentOperation op mask conflict (s : Segment<_, 'a>) =
        failwithf "Could not %s %s, sid: %A\n  Requested: %s\n  Existing:  %s\n  Error:     %s"
            op (typeof<'a> |> typeToString) s.id (maskToString 64 mask) 
            (maskToString 64 s.mask) (maskToString 64 conflict)

    /// Ordered list of segments and lookup    
    type ImmediateSegments<'k, 'a 
                when 'k :> IComparable<'k> 
                and 'k : equality>(pool : Stack<_>) =    
        let comparison = Comparison<Segment<'k, 'a>>(fun a b -> a.id.CompareTo(b.id))
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
        member c.MergeFrom(src : _[], count) =
            if count > 0 then
                mergeInto segments src count comparison
                rebuildLookup()
        /// Removes all empty segments
        member c.Prune() =
            if segments.RemoveAll(removeIfEmpty) > 0 then
                rebuildLookup()            

    type AdditionSegments<'k, 'a 
        when 'k :> IComparable<'k> 
        and 'k : equality>(pool : Stack<_>) =
        let comparison = Comparison<Segment<'k, 'a>>(fun a b -> a.id.CompareTo(b.id))
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
        let mutable segments = Array.zeroCreate<Segment<'k, 'a>> 8
        let mutable count = 0
        let idToIndex = Dictionary<'k, int>()
        member c.Clear() =
            for i = 0 to count - 1 do
                let seg = segments.[i]
                clearArray seg.data
                pool.Push(seg.data)
            count <- 0
            idToIndex.Clear()
        member internal c.ComponentCount = 
            let mutable total = 0
            for i = 0 to count - 1 do
                total <- total + bitCount64 segments.[i].mask
            total
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
            let i = 
                match idToIndex.TryGetValue(id) with
                | true, i -> i
                | false, _ ->
                    let i = count
                    if count = segments.Length then
                        segments <- resizeArray segments (count + 1)
                    segments.[i] <- Segment.init id 0UL (allocateData())
                    count <- count + 1
                    idToIndex.Add(id, i)
                    i
            let s = segments.[i]
            //segments.[i].mask <- s.mask ||| mask
            let newMask = s.mask ||| mask
            segments.[i] <- Segment.init s.id newMask s.data
            s.data        
        /// Removes bits given ID, but does not remove segment if empty
        member c.Remove(id, mask) =
            match idToIndex.TryGetValue(id) with
            | false, _ -> ()
            | true, i ->
                let s = segments.[i]
                let newMask = s.mask &&& ~~~mask
                clearArrayMask mask s.data
                segments.[i] <- Segment.init s.id newMask s.data
        member c.FlushTo(target : ImmediateSegments<'k, 'a>) =
            // first copy into existing, removing deltas
            let mutable di = 0
            while di < count do
                let delta = segments.[di]
                // if not found, skip past for now
                match target.TryFind(delta.id) with
                | false, _ -> di <- di + 1
                | true, i -> 
                    // copy into existing
                    let data = target.Add(i, delta.mask)
                    copyArrayMask delta.mask delta.data data
                    clearArrayMask delta.mask delta.data
                    // remove from deltas
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
            c.Clear()

    type RemovalSegments<'k 
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality>() =    
        let idToIndex = Dictionary<'k, int>()
        let mutable masks = Array.zeroCreate<uint64> 8
        let mutable ids = Array.zeroCreate<'k> 8
        let mutable count = 0
        member c.Count = count        
        member c.Item with get i = BitSegment.init ids.[i] masks.[i]
        member internal c.ComponentCount = 
            let mutable total = 0
            for i = 0 to count - 1 do
                total <- total + bitCount64 masks.[i]
            total
        member internal c.Segments = seq {
            for i = 0 to count - 1 do
                yield c.[i]
            }
        member c.Clear() =
            count <- 0
            idToIndex.Clear()
        member c.GetMask(sid) =
            match idToIndex.TryGetValue(sid) with
            | false, _ -> 0UL
            | true, i -> masks.[i]
        member c.Add(sid, mask) =
            let i = 
                match idToIndex.TryGetValue(sid) with
                | true, i -> i
                | false, _ ->
                    let i = count
                    if count = masks.Length then
                        masks <- resizeArray masks (count + 1)
                        ids <- resizeArray ids (count + 1)
                    masks.[i] <- 0UL
                    ids.[i] <- sid
                    count <- count + 1
                    idToIndex.Add(sid, i)
                    i
            masks.[i] <- masks.[i] ||| mask
        member c.Remove(sid, mask) =
            match idToIndex.TryGetValue(sid) with
            | true, i -> masks.[i] <- masks.[i] &&& ~~~mask
            | false, _ -> ()
        member c.FlushTo(target : ImmediateSegments<'k, 'a>) =
            for di = 0 to count - 1 do
                match target.TryFind(ids.[di]) with
                | false, _ -> ()
                | true, i ->
                    let mask = masks.[di]
                    let data = target.Remove(i, mask)
                    clearArrayMask mask data
            target.Prune()
            c.Clear()

    type ISegments<'k 
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality> =
        abstract member Clear : unit -> unit
        abstract member TryFind : 'k * byref<int> -> bool
        abstract member RemoveMask : 'k * uint64 -> unit
        abstract member Remove : RemovalSegments<'k> -> unit
        abstract member Commit : unit -> unit
        abstract member Handle : ISegmentHandler -> 'k -> uint64 ->unit
    
    type PendingBatch<'k, 'a 
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality>(pool) =
        let additions = AdditionSegments<'k, 'a>(pool)
        let removals = RemovalSegments<'k>()
        member c.IsEmpty =
            additions.Count = 0 && removals.Count = 0
        member c.GetRemovalMask sid =
            removals.GetMask sid
        member c.Clear() =
            additions.Clear()
            removals.Clear()
        member inline c.Add(sid, mask) =
            additions.Add(sid, mask)
        member inline c.Remove(sid, mask) =
            removals.Add(sid, mask)
        member c.FlushTo target =
            // For versioned/unique keys, we can have scenarios where key is added
            // and then removed in same step due to timing (e.g. animation coroutine).
            // In this case, we expect add and then remove.
            // For non-unique keys, we might want to remove an old value and add a new
            // value in the same step (e.g. replace grid cell, assuming old is present
            // and new is not), in which case we'd want the opposite behavior.
            additions.FlushTo target
            removals.FlushTo target
        member c.ToString(formatSegments, formatBitSegments) =
            sprintf "%d/%dA %d/%dR%s%s"
                additions.ComponentCount additions.Count
                removals.ComponentCount removals.Count
                (formatSegments ("  A") additions.Segments)
                (formatBitSegments ("  R") removals.Segments)
        override c.ToString() =
            c.ToString(formatSegments, formatBitSegments)

    type PendingBatches<'k, 'a 
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality>(createBatch) =
        let batchPool = Stack<_>()
        let createBatch() =
            if batchPool.Count > 0 then batchPool.Pop() 
            else createBatch()
        let batches = Queue<PendingBatch<'k, 'a>>()
        let mutable current = createBatch()
        member c.FlushTo target =
            while batches.Count > 0 do
                let batch = batches.Dequeue()
                batch.FlushTo target
                batchPool.Push batch
            current.FlushTo target
        member c.Clear() =
            while batches.Count > 0 do
                let batch = batches.Dequeue()
                batch.Clear()
                batchPool.Push batch
            current.Clear()
        member inline c.Add(sid, mask) =
            let removalMask = current.GetRemovalMask sid
            if removalMask &&& mask <> 0UL then 
                batches.Enqueue current
                current <- createBatch()
            current.Add(sid, mask)
        member inline c.Remove(sid, mask) =
            current.Remove(sid, mask)
        member c.ToString(formatSegments, formatBitSegments) =
            String.Join("\n", seq {
                for batch in batches do
                    yield batch.ToString(formatSegments, formatBitSegments)
                if not current.IsEmpty then
                    yield current.ToString(formatSegments, formatBitSegments)
                })
        override c.ToString() =
            c.ToString(formatSegments, formatBitSegments)

    type SparseSegmentsFactory<'k, 'a 
            when 'k :> IComparable<'k> 
            and 'k :> IEquatable<'k> 
            and 'k : equality>() =
        let pool = Stack<'a[]>()
        member c.CreateSegments() =
            ImmediateSegments<'k, 'a>(pool)
        member c.CreatePendingBatch() =
            PendingBatch<'k, 'a>(pool)

/// Sparse list of segments
type Segments<'k, 'a 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality>() =    
    let factory = SparseSegmentsFactory<'k, 'a>()
    let pending = PendingBatches(factory.CreatePendingBatch)
    let current = factory.CreateSegments()
    /// Returns a sequence of the components present
    member c.Components = current.Components
    /// Number of components within all current segments
    member c.ComponentCount = current.ComponentCount
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
    member c.SetMask(sid, mask) =
        match c.TryFind(sid) with
        | true, i -> current.Set(i, mask)
        | false, _ -> failwithf "Segment %A not present" sid
    /// Returns segment so data can be filled
    /// Assume either not present or in removal list
    member c.AddMask(sid, mask) =
        pending.Add(sid, mask)
    /// Assumes present, not considering addition list
    member c.RemoveMask(sid, mask) =
        pending.Remove(sid, mask)
    /// Commits any pending changes and removes empty segments
    member c.Commit() = 
        pending.FlushTo(current)
    interface ISegments<'k> with
        member c.Clear() =
            c.Clear()
        member c.TryFind(sid, [<Out>] i : byref<_>) = 
            current.TryFind(sid, &i)
        member c.RemoveMask(sid, mask) =
            c.RemoveMask(sid, mask)
        member c.Remove(ids : RemovalSegments<'k>) =
            for i = 0 to ids.Count - 1 do
                let delta = ids.[i]
                c.RemoveMask(delta.id, delta.mask)
        member c.Commit() = c.Commit()
        member c.Handle handler sid mask =
            match c.TryFind(sid) with
            | false, _ -> ()
            | true, si ->
                let seg = current.[si]
                let masked = Segment.init seg.id (seg.mask &&& mask) seg.data
                handler.Handle masked
    member c.ToString(formatSegments, formatBitSegments) =
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
    member c.ContainsSegment(sid) =
        let mutable i = 0
        c.TryFind(sid, &i)          
    member c.GetSegment(sid) =
        match c.TryFind(sid) with
        | true, si -> c.[si]
        | false, _ -> failwithf "Cannot get %s segment %A" (typeToString typeof<'a>) sid
    /// Removes entire segment
    member c.RemoveSegment(sid) =
        match c.TryFind(sid) with
        | false, _ -> ()
        | true, si ->
            let mask = c.[si].mask
            c.RemoveMask(sid, mask)
    /// Marks all segemnts for removal, which is different than immediate Clear()
    member c.RemoveAll() =
        for i = 0 to c.Count - 1 do
            let seg = c.[i]
            c.RemoveMask(seg.id, seg.mask)

type ISegmentStore<'k
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality> =
    abstract member Get<'b> : unit -> Segments<'k, 'b>
        