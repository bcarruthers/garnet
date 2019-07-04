namespace Garnet.Ecs

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

    let inline eidToComponentKey (id : Eid) = {
        segmentId = getSegmentIndex id
        componentIndex = getComponentIndex id }

type Eid with
    member i.Index = Eid.getIndex i
    member i.Slot = Eid.getSlot i
    member i.Gen = Eid.getGen i
    member i.Partition = Eid.getPartition i
    member i.IsDefined = i.value <> 0
    member i.IsUndefined = i.value = 0

type Components<'a> = Components<int, Eid, 'a>
type ComponentStore = ComponentStore<int, Eid>

module Components =
    let create<'a>() = Components<'a>(Eid.eidToComponentKey)

module ComponentStore =
    let create() = ComponentStore(Eid.eidToComponentKey)

/// Stores available IDs with a given partition. IDs start at 1
type EidPool(partition) =
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

type EidPools() =
    let pools = Array.init Eid.partitionCount EidPool
    member internal c.Items = pools
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
        c.Items
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
    let types = ResourceLookup()
    let channels = types.GetResource<Channels>()
    let scheduler = types.GetResource<CoroutineScheduler>()
    let components = types.CreateResource(ComponentStore.create())
    let eidPools = types.GetResource<EidPools>()
    let eids = components.Get<Eid>()
    member c.Get<'a>() = components.Get<'a>()
    member c.GetChannel<'a>() = channels.GetChannel<'a>()
    member c.GetPool(i) = eidPools.[i]
    member c.RegisterResource x = types.RegisterResource x
    member c.AddResource x = types.AddResource x
    member c.TryGetResource<'a>([<Out>] r : byref<_>) = 
        types.TryGetResource<'a>(&r)
    /// Returns true if events were handled
    member c.Dispatch() = 
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
    member c.CreateEid(partition) =
        let eid = c.GetPool(partition).Next()
        eids.Add(eid, eid)
        eid
    member c.Destroy(id : Eid) =
        components.Destroy(id)
        let partition = Eid.getPartition id
        c.GetPool(partition).Recycle(id)
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
    interface IResourceLookup with
        member c.RegisterResource f = c.RegisterResource f
        member c.AddResource x = c.AddResource x
        member c.TryGetResource<'a>([<Out>] r : byref<_>) = 
            c.TryGetResource<'a>(&r)
    interface IChannels with
        member c.GetChannel<'a>() = channels.GetChannel<'a>()
    interface ISegmentStore<int> with
        member c.Get<'a>() = 
            components.Get<'a>() :> Segments<_,_>
    interface ISegmentsLookup<int, Eid> with
        member c.Get<'a>() = components.Get<'a>()
        member c.Destroy(id) = c.Destroy id
        member c.Handle id handler =
            components.Handle id handler
    override c.ToString() = 
        types.ToString()

type Entity = Entity<int, Eid, Container>

type Container with
    member c.Get(eid) = { id = eid; container = c }

    member c.Create(partition) =
        let eid = c.CreateEid(partition)
        c.Get eid

    member c.Create() = c.Create(0)

    member c.DestroyAll() =
        let segs = c.Get<Eid>()
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
module Composition =
    let cmp c (e : Entity) = e.Add c

    let create prefab (c : Container) =
        let e = c.Create()
        prefab e
        e

    let compose components =
        let arr = components |> Seq.toArray
        fun e -> for c in arr do c e    
        
/// Runs container for each incoming message
type ContainerMessageHandler(container : Container) =
    let mutable batchCount = 0
    let mutable messageCount = 0
    let receiver = container.GetResource<MessageReceiver>()
    let sender = container.GetResource<MessageSender>()
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
