namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open Garnet
open Garnet.Comparisons
open Garnet.Formatting

/// 32-bit entity ID
[<Struct>]
type Eid =
    val value : int
    new(id) = { value = id }
    override e.ToString() = "0x" + e.value.ToString("x")
    
module Eid =
    // ID bits:
    // gggg gggg pppp xxxx xxxx xxxx xxxx xxxx
    // (8)  g: generation, max 256
    // (4)  p: partition, max 16
    // (20) x: index, max ~1,000,000
    let totalBits = 32

    let genBits = 8
    let genCount = 1 <<< genBits
    let genMask = genCount - 1
    let maxGen = genMask

    // 24 non-generation bits
    let slotBits = totalBits - genBits
    let slotCount = 1 <<< slotBits
    let slotMask = slotCount - 1

    let partitionBits = 4
    let partitionCount = 1 <<< partitionBits
    let partitionMask = partitionCount - 1

    // 20 remaining non-generation/non-partition bits
    let indexBits = slotBits - partitionBits
    let indexCount = 1 <<< indexBits
    let indexMask = indexCount - 1

    let inline init id = Eid id

    let inline getIndex (eid : Eid) =
        eid.value &&& indexMask

    let inline getPartition (eid : Eid) =
        (eid.value >>> indexBits) &&& partitionMask

    let inline getSlot (eid : Eid) =
        eid.value &&& slotMask

    let inline getGen (eid : Eid) = 
        uint32 eid.value >>> slotBits |> int

    let inline setGen (eid : Eid) gen =
        (getSlot eid) ||| (gen <<< slotBits)
        |> init
        
    let inline incrementGen (eid : Eid) =
        let gen = getGen eid
        let next = (gen + 1) &&& genMask
        setGen eid next
        
    let inline getSegmentIndex (eid : Eid) = 
        getSlot eid >>> Segment.segmentBits

    let inline getComponentIndex (eid : Eid) = 
        eid.value &&& Segment.segmentMask

    let inline fromParts gen partition id =
        (gen <<< slotBits) |||
        (partition <<< indexBits) |||
        id
        |> init

    let undefined = init 0

    let formatEid eid =
        sprintf "%d %d %d" (getGen eid) (getPartition eid) (getIndex eid)

    let inline eidToComponentKey (id : Eid) =
        struct(getSegmentIndex id, getComponentIndex id)

    let segmentToPartitionBits = indexBits - Segment.segmentBits
    let segmentInPartitionMask = (1 <<< segmentToPartitionBits) - 1

    let inline segmentToPartition sid =
        sid >>> segmentToPartitionBits

type Eid with
    member i.Index = Eid.getIndex i
    member i.Slot = Eid.getSlot i
    member i.Gen = Eid.getGen i
    member i.Partition = Eid.getPartition i
    member i.IsDefined = i.value <> 0
    member i.IsUndefined = i.value = 0

type Entity = Entity<int, Eid>

type internal EidPool(partition) =
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
    member internal c.Apply(seg : PendingSegment<int, Eid>) =
        let sid = seg.id &&& Eid.segmentInPartitionMask
        c.EnsureSize sid
        let offset = sid * 64
        if seg.mask &&& seg.removalMask <> 0UL then
            failwithf "Segment contains overlapping add/remove"
        // When eid added and not previously known, write to pool with 
        // incremented gen.
        if seg.mask <> 0UL then
            let addMask = seg.mask &&& ~~~known.[sid]
            let mutable m = addMask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then 
                    eids.[offset + i] <- Eid.incrementGen seg.data.[i]
                m <- m >>> 1
                i <- i + 1
            known.[sid] <- known.[sid] ||| addMask
            used.[sid] <- used.[sid] ||| seg.mask
        // When eid removed, mark as unused and increment.
        if seg.removalMask <> 0UL then
            // Note we're not checking if used or not -- if not marked as used, 
            // eid was created/destroyed before commit, but we still need to 
            // return it to increment gen.
            let removalMask = seg.removalMask
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
                } |> formatSegments ("    "))

type EidPools() =
    let pools = Array.init Eid.partitionCount EidPool
    member c.Count = pools.Length
    member c.Next p = 
        pools.[p].Next()
    member c.Apply(active : Segments<int, Eid>) =
        for i = 0 to active.PendingCount - 1 do
            let seg = active.GetPending i
            let p = Eid.segmentToPartition seg.id
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
        |> listToString (prefix + "  ") (prefix + "Pools")

/// Wrapper over resource lookup with default types for ECS
type Container() =
    let reg = Registry()
    let channels = reg.GetInstance<Channels>()
    let scheduler = reg.GetInstance<CoroutineScheduler>()
    let segments = reg.GetInstance<SegmentStore<int>>()
    let outbox = reg.GetInstance<Outbox>()
    let eidPools = reg.GetInstance<EidPools>()
    let components = ComponentStore(segments, Eid.eidToComponentKey)
    let eids = segments.GetSegments<Eid>()
    member c.Get<'a>() = components.Get<'a>()
    member c.GetSegments<'a>() = segments.GetSegments<'a>()
    member c.GetChannel<'a>() = channels.GetChannel<'a>()
    member c.Register x = reg.Register x
    member c.RegisterInstance x = reg.RegisterInstance x
    member c.TryGetInstance<'a>([<Out>] r : byref<_>) = 
        reg.TryGetInstance<'a>(&r)
    member c.GetAddresses() =
        outbox.Current.addresses
    member internal c.Clear() =
        channels.Clear()
        components.Clear()
        eidPools.Clear()
        scheduler.Clear()
    member c.Commit() =
        // Order of commits doesn't matter since we're just moving data
        // into committed state and not calling any handlers.
        channels.Commit()
        // Copy removals from eids to other component types first,
        // then apply eid changes to partition cache, then after all
        // this commit all resulting component changes.
        segments.ApplyRemovalsFrom(eids)   
        eidPools.Apply(eids)
        components.Commit()
        // Publish event to allow for custom commit implementations.
        channels.Publish <| Commit()
    /// Returns true if events were handled
    member private c.DispatchOnce() = 
        c.Commit()
        channels.Publish()
    member private c.DispatchAll() = 
        while c.DispatchOnce() do ()
    member private c.RunOnce() = 
        c.Commit()
        scheduler.RunOnce()
    member c.Run() = 
        c.DispatchAll()
        while c.RunOnce() do
            c.DispatchAll()
    member c.Contains(eid : Eid) =
        let struct(sid, ci) = Eid.eidToComponentKey eid
        let mask = eids.GetMask sid
        (mask &&& (1UL <<< ci)) <> 0UL
    member c.Get(eid) = { 
        id = eid
        container = components 
        }
    member internal c.CreateEid(partition) =
        let eid = eidPools.Next(partition)
        let struct(sid, ci) = Eid.eidToComponentKey eid
        let data = eids.Add(sid, 1UL <<< ci)
        data.[ci] <- eid
        eid
    member c.Handle(id, handler) =
        components.Handle(id, handler)
    member c.Destroy(eid : Eid) =
        // Only removing from eids and relying on commit to remove
        // other components.
        let struct(sid, ci) = Eid.eidToComponentKey eid
        eids.Remove(sid, 1UL <<< ci)
    member c.Step deltaTime =
        scheduler.Step deltaTime
    member c.Start coroutine = 
        scheduler.Schedule coroutine
    member c.SetPublisher pub =
        channels.SetPublisher pub
    member c.SetPublisher pub =
        c.SetPublisher (ValueSome pub)
    interface IRegistry with
        member c.Register f = c.Register f
        member c.RegisterInstance x = c.RegisterInstance x
        member c.TryGetInstance<'a>([<Out>] r : byref<_>) = 
            c.TryGetInstance<'a>(&r)
    interface IChannels with
        member c.GetChannel<'a>() = channels.GetChannel<'a>()
    interface IComponentStore<int, Eid> with
        member c.Get<'a>() = 
            components.Get<'a>()
    interface ISegmentStore<int> with
        member c.GetSegments<'a>() = 
            segments.GetSegments<'a>()
    member c.BeginSend() =
        outbox.BeginSend()
    interface IOutbox with
        member c.BeginSend() =
            outbox.BeginSend()
    member c.Receive (e : Envelope<_>) =
        // assign outbox for duration of call
        use s = outbox.Push e
        let channel = c.GetChannel<'a>()
        channel.PublishAll e.message
        c.Run()
    interface IInbox with
        member c.Receive e =
            c.Receive(e)
    override c.ToString() = 
        reg.ToString()

type Container with
    member c.Create(partition) =
        let eid = c.CreateEid(partition)
        c.Get eid

    member c.Create() = c.Create(0)

    member c.DestroyAll() =
        c.GetSegments<Eid>().RemoveAll()

    member c.Run(msg) = 
        c.Send(msg)
        c.Run()

    member c.BeginRespond() =
        c.BeginSend(c.GetAddresses().sourceId)

    member c.Respond(msg) =
        c.Send(c.GetAddresses().sourceId, msg)
    
[<AutoOpen>]
module internal Prefab =
    let cmp c (e : Entity<_,_>) = e.Add c

    let create prefab (c : Container) =
        let e = c.Create()
        prefab e
        e

    let compose components =
        let arr = components |> Seq.toArray
        fun e -> for c in arr do c e    
