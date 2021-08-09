namespace Garnet.Composition

open System
open System.Runtime.CompilerServices
open Garnet
open Garnet.Comparisons
open Garnet.Formatting

/// 32-bit entity ID
[<Struct>]
type Eid =
    val Value : int
    new(value) = { Value = value }
    override e.ToString() = "0x" + e.Value.ToString("x")
    
module Eid =
    // Entity ID bits:
    // gggg gggg [ pppp xxxx xxxx xxxx xxxx xxxx ]
    // (8)  g: generation, max 256
    // (24) Slot:
    //      (4)  p: partition, max 16
    //      (20) x: index, max ~1,000,000
    
    [<Literal>]
    let TotalBits = 32

    [<Literal>]
    let GenBits = 8

    [<Literal>]
    let GenCount = 256

    [<Literal>]
    let GenMask = 255

    [<Literal>]
    let MaxGen = GenMask

    [<Literal>]
    let SlotBits = 24

    [<Literal>]
    let SlotCount = 0x1000000
    
    [<Literal>]
    let SlotMask = 0xffffff

    [<Literal>]
    let PartitionBits = 4

    [<Literal>]
    let PartitionCount = 0x10

    [<Literal>]
    let PartitionMask = 0xf

    [<Literal>]
    let IndexBits = 20
    
    [<Literal>]
    let IndexCount = 0x100000
    let IndexMask = 0xfffff

    let inline init id = Eid id

    let undefined = init 0
    
    let inline getIndex (eid : Eid) =
        eid.Value &&& IndexMask

    let inline getPartition (eid : Eid) =
        (eid.Value >>> IndexBits) &&& PartitionMask

    let inline getSlot (eid : Eid) =
        eid.Value &&& SlotMask

    let inline getGen (eid : Eid) = 
        uint32 eid.Value >>> SlotBits |> int

    let inline setGen (eid : Eid) gen =
        (getSlot eid) ||| (gen <<< SlotBits)
        |> init
        
    let inline incrementGen (eid : Eid) =
        let gen = getGen eid
        let next = (gen + 1) &&& GenMask
        setGen eid next
        
    let inline getSegmentIndex (eid : Eid) = 
        getSlot eid >>> Segment.segmentBits

    let inline getComponentIndex (eid : Eid) = 
        eid.Value &&& Segment.segmentMask

    let inline fromParts gen partition id =
        (gen <<< SlotBits) |||
        (partition <<< IndexBits) |||
        id
        |> init

    let formatEid eid =
        sprintf "%d %d %d" (getGen eid) (getPartition eid) (getIndex eid)

    let segmentToPartitionBits = IndexBits - Segment.segmentBits
    let segmentInPartitionMask = (1 <<< segmentToPartitionBits) - 1

    let inline segmentToPartition sid =
        sid >>> segmentToPartitionBits

type Eid with
    member i.Index = Eid.getIndex i
    member i.Slot = Eid.getSlot i
    member i.Gen = Eid.getGen i
    member i.Partition = Eid.getPartition i
    member i.IsDefined = i.Value <> 0
    member i.IsUndefined = i.Value = 0

[<Struct>]
type EidSegmentKeyMapper =
    interface ISegmentKeyMapper<int, Eid> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member c.GetSegmentKey(id) = Eid.getSegmentIndex id
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member c.GetComponentIndex(id) = Eid.getComponentIndex id

type Entity = Entity<int, Eid, EidSegmentKeyMapper>

type EidPool(partition) =
    let mutable known = Array.zeroCreate 1
    let mutable used = Array.zeroCreate 1
    let mutable eids = Array.zeroCreate 64
    let mutable current = Segment.segmentBits - 1
    let mutable mask = 0UL
    member c.Used = Bits.bitCount64Array used
    member c.Allocated = Bits.bitCount64Array known
    member c.Total = c.Used + c.Allocated
    member c.SegmentCount = known.Length
    member private c.EnsureSize si =
        let required = si + 1
        if used.Length < required then
            Buffer.resizeArray required &known
            Buffer.resizeArray required &used
            Buffer.resizeArray (required * 64) &eids
    member c.Clear() =
        Array.Clear(known, 0, known.Length)
        Array.Clear(used, 0, used.Length)
        Array.Clear(eids, 0, eids.Length)
        c.Reset()
    member c.Reset() =
        current <- Segment.segmentBits - 1
        mask <- 0UL
    member c.Next() =
        if mask = 0UL then
            // Seek until unused segment found. Note we always increment,
            // ensuring we start at segment 1 to avoid eid zero.
            let mutable si = (current >>> 6) + 1
            c.EnsureSize si
            while used.[si] = UInt64.MaxValue do
                si <- si + 1
                current <- si * 64
                c.EnsureSize si
            // allocate new eids as needed
            let knownMask = known.[si]
            if knownMask <> UInt64.MaxValue then
                let offset = si * 64
                let mutable m = ~~~knownMask
                let mutable i = offset
                while m <> 0UL do
                    if m &&& 1UL <> 0UL then 
                        eids.[i] <- Eid.fromParts 0 partition i
                    m <- m >>> 1
                    i <- i + 1
                known.[si] <- UInt64.MaxValue
            mask <- ~~~used.[si]
            current <- (si <<< 6) - 1
        // increment first with assumption that we start at -1
        current <- current + 1
        // advance to next unused slot with segment
        while mask &&& 1UL = 0UL do
            mask <- mask >>> 1
            current <- current + 1
        // claim eid and advance to next
        let eid = eids.[current]        
        mask <- mask >>> 1
        eid
    member c.Recycle(eid : Eid) =
        c.Apply {
            Data = null
            Id = Eid.getSegmentIndex eid
            Mask = 0UL
            RemovalMask = 1UL <<< (Eid.getComponentIndex eid)
        }
    member internal c.Apply(seg : PendingSegment<int, Eid>) =
        let sid = seg.Id &&& Eid.segmentInPartitionMask
        c.EnsureSize sid
        let offset = sid * 64
        if seg.Mask &&& seg.RemovalMask <> 0UL then
            failwithf "Segment contains overlapping add/remove"
        // When eid added and not previously known, write to pool with 
        // incremented gen.
        if seg.Mask <> 0UL then
            let addMask = seg.Mask &&& ~~~known.[sid]
            let mutable m = addMask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then 
                    eids.[offset + i] <- Eid.incrementGen seg.Data.[i]
                m <- m >>> 1
                i <- i + 1
            known.[sid] <- known.[sid] ||| addMask
            used.[sid] <- used.[sid] ||| seg.Mask
        // When eid removed, mark as unused and increment.
        if seg.RemovalMask <> 0UL then
            // Note we're not checking if used or not -- if not marked as used, 
            // eid was created/destroyed before commit, but we still need to 
            // return it to increment gen.
            let removalMask = seg.RemovalMask
            // Since removal seg doesn't have populated eids, we must take them
            // from stored value in pool, which means they must be known. In the
            // case of restore, expect that add would always be called first to
            // populate eids.
            if removalMask &&& known.[sid] <> removalMask then
                failwithf "Cannot return unknown IDs to pool"
            let mutable m = removalMask
            let mutable i = offset
            while m <> 0UL do
                if m &&& 1UL <> 0UL then 
                    eids.[i] <- Eid.incrementGen eids.[i]
                m <- m >>> 1
                i <- i + 1
            used.[sid] <- used.[sid] &&& ~~~removalMask
        c.Reset()
    override p.ToString() =
        let formatBits known used =
            Array.init 64 (fun i ->
                let k = (known >>> i) &&& 1UL
                let u = (used >>> i) &&& 1UL
                match k, u with
                | 0UL, 0UL -> ' '
                | 0UL, 1UL -> 'u'
                | 1UL, 0UL -> '.'
                | 1UL, 1UL | _ -> 'x')
                |> String
        sprintf "%d alloc, %d used, %d total, %d segs%s" 
            p.Allocated p.Used p.Total used.Length
            (seq {
                for i = 0 to used.Length - 1 do
                    let k = known.[i] 
                    let u = used.[i]
                    if k ||| u <> 0UL then
                        yield sprintf "%d %s" i (formatBits k u)
                } |> Format.listToString "    " "")

type EidPools() =
    let pools = Array.init Eid.PartitionCount EidPool
    member c.Count = pools.Length
    member c.Next p = 
        pools.[p].Next()
    member c.Apply(active : Segments<int, Eid>) =
        for i = 0 to active.PendingCount - 1 do
            let seg = active.GetPending i
            let p = Eid.segmentToPartition seg.Id
            pools.[p].Apply seg        
    member c.Clear() =
        for pool in pools do
            pool.Clear()
    override c.ToString() =
        let prefix = ""
        pools
        |> Seq.mapi (fun i p -> 
            if p.Total > 0 
            then sprintf "%d: %s" i (p.ToString()) 
            else "")
        |> Seq.filter (fun str -> str.Length > 0)
        |> Format.listToString (prefix + "  ") (prefix + "Pools")
