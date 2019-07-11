namespace Garnet.Ecs

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open Garnet.Comparisons
open Garnet.Formatting
open Garnet.Actors

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

type Eid with
    member i.Index = Eid.getIndex i
    member i.Slot = Eid.getSlot i
    member i.Gen = Eid.getGen i
    member i.Partition = Eid.getPartition i
    member i.IsDefined = i.value <> 0
    member i.IsUndefined = i.value = 0

/// Stores available IDs with a given partition. IDs start at 1
type internal EidPool(partition) =
    let mask = partition <<< Eid.indexBits
    let pendingIds = List<Eid>()
    let availableIds = Queue<int>()
    let mutable count = 0
    let getNewEid baseId =
        baseId ||| mask |> Eid
    member c.Count = count
    member c.Pooled = availableIds.Count
    member c.Pending = pendingIds.Count
    member c.Total = availableIds.Count + pendingIds.Count
    member c.Next() =
        let baseId =
            // start at segment 1 to avoid partial starting segment since
            // Eid 0 is reserved for null
            let minId = Segment.segmentSize
            // impose threshold to maximize cycles between duplicate IDs
            // but shouldn't be too high so we don't allocate excess
            let reserveCount = 0//segmentSize
            if availableIds.Count > reserveCount then availableIds.Dequeue()
            elif count = Eid.indexCount - minId then failwith "Max IDs reached"
            else
                let id = count
                count <- count + 1
                id + minId
        getNewEid baseId
    member c.Reset newCount =
        count <- newCount
    member c.Recycle(eid) =
        pendingIds.Add(eid)
    member c.Restore eids =
        c.Clear()
        let mutable maxId = 0
        let baseIdSet = HashSet<int>()
        // assume eids could be in any order and from other partitions
        // bump count to one more than max ID present of partition
        for eid in eids do
            if Eid.getPartition eid = partition then
                let baseId = Eid.getIndex eid
                if baseId > maxId then
                    maxId <- baseId
                baseIdSet.Add baseId |> ignore
        count <- maxId
        // fill in available with any gaps up to max ID
        for baseId = 1 to count - 1 do
            if not (baseIdSet.Contains baseId) then
                availableIds.Enqueue (getNewEid baseId).value
    member c.Clear() =
        pendingIds.Clear()
        availableIds.Clear()
        count <- 0
    member c.Commit() =
        for eid in pendingIds do
            let nextGen = Eid.incrementGen eid
            availableIds.Enqueue(nextGen.value)
        pendingIds.Clear()           
    override p.ToString() =
        sprintf "%dC %dT %dP %dR" p.Count p.Total p.Pooled p.Pending

type internal EidPools() =
    let pools = Array.init Eid.partitionCount EidPool
    member c.Count = pools.Length
    member c.Item with get i = pools.[i]
    member c.Next() = c.[0].Next()
    member c.Recycle(id) = c.[Eid.getPartition id].Recycle(id)        
    member c.Restore eids =
        for pool in pools do
            pool.Restore eids
    member c.Commit() =
        for pool in pools do
            pool.Commit()
    member c.Clear() =
        for pool in pools do
            pool.Clear()
    override c.ToString() =
        let prefix = ""
        pools
        |> Seq.mapi (fun i p -> 
            if p.Count > 0 
            then sprintf "%d: %s" i (p.ToString()) 
            else "")
        |> Seq.filter (fun str -> str.Length > 0)
        |> listToString (prefix + "  ") (prefix + "Pools")

/// Event published when commit occurs    
type Commit = struct end

/// Wrapper over resource lookup with default types for ECS
type Container() =
    let types = Registry()
    let channels = types.GetInstance<Channels>()
    let scheduler = types.GetInstance<CoroutineScheduler>()
    let segments = types.GetInstance<SegmentStore<int>>()
    let components = 
        let store = ComponentStore(segments, Eid.eidToComponentKey)
        types.RegisterInstance(store)
        store        
    let eidPools = types.GetInstance<EidPools>()
    let eids = components.Get<Eid>()
    member c.Get<'a>() = components.Get<'a>()
    member c.GetSegments<'a>() = segments.GetSegments<'a>()
    member c.GetChannel<'a>() = channels.GetChannel<'a>()
    member c.Register x = types.Register x
    member c.RegisterInstance x = types.RegisterInstance x
    member c.TryGetInstance<'a>([<Out>] r : byref<_>) = 
        types.TryGetInstance<'a>(&r)
    member internal c.Clear() =
        channels.Clear()
        components.Clear()
        eidPools.Clear()
        scheduler.Clear()
    /// Returns true if events were handled
    member internal c.Dispatch() = 
        channels.Publish()
    member c.Commit() =
        // order doesn't matter since we're just moving data
        // into committed state and not calling any handlers
        channels.Commit()
        components.Commit()
        eidPools.Commit()
        channels.Handle <| Commit()
    member c.RunOnce() = 
        c.Commit()
        scheduler.RunOnce()
    member c.Contains(eid : Eid) =
        eids.Contains(eid)
    member internal c.CreateEid(partition) =
        let eid = eidPools.[partition].Next()
        eids.Add(eid, eid)
        eid
    member c.Handle(id, handler) =
        components.Handle(id, handler)
    member c.Destroy(id : Eid) =
        components.Destroy(id)
        let partition = Eid.getPartition id
        eidPools.[partition].Recycle(id)
    /// Assumes eid components have been populated and restores 
    /// eid pools from that state
    member c.RestoreEids() =
        eidPools.Restore eids.Components
    member c.Step deltaTime =
        scheduler.Step deltaTime
    member c.Start coroutine = 
        scheduler.Schedule coroutine
    member c.SetDispatcher dispatcher =
        channels.SetDispatcher dispatcher
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
    override c.ToString() = 
        types.ToString()
        
[<Struct>]
type Entity = {
    id : Eid
    container : Container
    } with
    member e.Add x = e.container.Get<_>().Add(e.id, x)
    member e.Set x = e.container.Get<_>().Set(e.id, x)
    member e.Remove<'a>() = e.container.Get<'a>().Remove(e.id)
    member e.Get<'a>() = e.container.Get<'a>().Get(e.id)    
    member e.Get<'a>(fallback) = e.container.Get<'a>().Get(e.id, fallback)
    member e.Contains<'a>() = e.container.Get<'a>().Contains(e.id)
    member e.Destroy() = e.container.Destroy(e.id)
    member e.With x = e.Add x; e
    override e.ToString() = 
        let printer = PrintHandler(UInt64.MaxValue)
        e.container.Handle(e.id, printer)
        "Entity " + e.id.ToString() + ": " + printer.ToString()

type Container with
    member c.Get(eid) = { id = eid; container = c }

    member c.Create(partition) =
        let eid = c.CreateEid(partition)
        c.Get eid

    member c.Create() = c.Create(0)

    member c.DestroyAll() =
        let segs = c.GetSegments<Eid>()
        for si = 0 to segs.Count - 1 do
            let seg = segs.[si]
            let mutable m = seg.mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then c.Destroy seg.data.[i]
                m <- m >>> 1
                i <- i + 1

    member private c.DispatchOnce() = 
        c.Commit()
        c.Dispatch()
        
    member private c.DispatchAll() = 
        while c.DispatchOnce() do ()

    member c.Run() = 
        c.DispatchAll()
        while c.RunOnce() do
            c.DispatchAll()

    member c.Run(msg) = 
        c.Send(msg)
        c.Run()
    
[<AutoOpen>]
module internal Composition =
    let cmp c (e : Entity) = e.Add c

    let create prefab (c : Container) =
        let e = c.Create()
        prefab e
        e

    let compose components =
        let arr = components |> Seq.toArray
        fun e -> for c in arr do c e    
        
/// Runs container for each incoming message
type internal ContainerMessageHandler(container : Container) =
    let mutable batchCount = 0
    let mutable messageCount = 0
    let receiver = container.GetInstance<MessageReceiver>()
    let sender = container.GetInstance<MessageSender>()
    interface IMessageHandler with
        member c.Handle e =
            // assign outbox for duration of call
            use s = sender.PushOutbox(e.outbox)
            let channel = container.GetChannel<'a>()
            let addresses = e.addresses
            for msg in e.message do
                let mapped = receiver.Map msg addresses
                channel.Send mapped
            batchCount <- batchCount + 1
            messageCount <- messageCount + e.message.Count
            container.Run()
    override c.ToString() =
        sprintf "Handler: %d batches, %d messages" batchCount messageCount

module ActorDefinition =
    let container c =
        ContainerMessageHandler(c) |> ActorDefinition.init
