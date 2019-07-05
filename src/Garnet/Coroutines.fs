namespace Garnet.Ecs

open System
open System.Collections.Generic
open Garnet.Comparisons

[<Struct>]
type Wait = {
    duration : int
    }

[<AutoOpen>]
module Wait =
    let time x = { Wait.duration = x }
    let defer = time -1

[<AutoOpen>]
module internal Collections =
    /// Mutable min-heap
    type Heap<'a>(compareTo : 'a -> 'a -> int) = // when 'a :> IComparable<'a>>() =
        // create a dummy value for easier indexing
        let items = List<'a>()
        do items.Add(Unchecked.defaultof<'a>)
        let compare a b = compareTo items.[a] items.[b]
        let swap a b =
            let temp = items.[b]
            items.[b] <- items.[a]
            items.[a] <- temp
        let getMinChildIndex parentIndex =
            let ci = parentIndex * 2
            if ci >= items.Count then -1
            else
                // if we have a second child that's smaller, pick it
                // we know that if second exists, first exists due to shape
                let offset =
                    if ci + 1 < items.Count && 
                        compare (ci + 1) ci < 0
                        then 1 else 0
                ci + offset
        let rec siftDown index =
            // start at top and swap down through min child
            let ci = getMinChildIndex index
            if ci >= 0 && compare index ci > 0 then
                swap index ci
                siftDown ci
        let rec siftUp index =
            // start at end and swap up through parent
            // maintain parent/child invariant at each iteration
            if index > 1 && compare index (index / 2) < 0 then
                swap index (index / 2)
                siftUp (index / 2)
        member h.Items = items
        member h.Count = items.Count - 1
        member h.Top = items.[1]
        member h.Insert item =
            items.Add(item)
            siftUp (items.Count - 1)
        member h.RemoveMin() =
            if h.Count = 0 then failwith "Heap is empty"
            let top = h.Top
            items.[1] <- items.[items.Count - 1]
            items.RemoveAt(items.Count - 1)
            siftDown 1
            top
        member h.Clear() =
            while items.Count > 1 do items.RemoveAt(items.Count - 1)

    [<Struct>]
    type Pair<'a, 'b> =
        val first : 'a
        val second : 'b
        new(x, y) = { first = x; second = y }
        override v.ToString() = sprintf "%A %A" v.first v.second

    /// Mutable, min queue (min priority value dequeued first)
    type PriorityQueue<'p, 'a when 'p :> IComparable<'p>>() =
        let compareTuples (a : Pair<'p,_>) (b : Pair<'p,_>) = a.first.CompareTo(b.first)
        let heap = Heap<Pair<'p, 'a>>(compareTuples)
        member q.Items = heap.Items
        member q.Count = heap.Count
        member q.Top = heap.Top
        member q.Enqueue priority value =
            heap.Insert (Pair(priority, value))
        member q.Dequeue() =
            heap.RemoveMin().second
        member q.Clear() = heap.Clear()
        
type internal StackScheduler() =
    let temp = List<_>()
    let pending = List<_>()
    let active = List<_>()
    let frames = List<_>()
    member c.Clear() =
        pending.Clear()
        active.Clear()
        frames.Clear()
    member c.Schedule (coroutine : _ seq) =
        pending.Add coroutine
    member c.Enqueue e =
        active.Add e
    // Returns true if work was done
    member c.RunOnce (iterate : Action<_>) =
        let hasPending = pending.Count > 0
        if hasPending then
            frames.Add(pending.Count)
            for coroutine in pending do
                let e = coroutine.GetEnumerator()
                c.Enqueue e
            pending.Clear()
        let hasFrames = frames.Count > 0
        if hasFrames then
            let frameSize = frames.[frames.Count - 1]
            let priorActiveCount = active.Count
            let frameStart = active.Count - frameSize
            // transfer into temp list and clear frame
            for i = frameStart to active.Count - 1 do
                temp.Add(active.[i])
            active.RemoveRange(frameStart, frameSize)
            // run coroutines, possibly re-enqueuing
            for e in temp do
                iterate.Invoke e
            temp.Clear()
            // update frame
            let removedCount = priorActiveCount - active.Count
            let newFrameSize = frameSize - removedCount
            if newFrameSize = 0 
                then frames.RemoveAt(frames.Count - 1)
                else frames.[frames.Count - 1] <- newFrameSize
        hasPending || hasFrames      
    override c.ToString() =
        sprintf "Stack: %d pending, %d active, frames: %s" pending.Count active.Count
            (String.Join(", ", frames))
            
type internal TimeScheduler() =
    let mutable time = 0
    let active = PriorityQueue<int,_>()
    member c.Enqueue (e : IEnumerator<_>) =
        let delay = e.Current.duration
        let nextTime = time + delay
        active.Enqueue nextTime e
    member c.RunOnce(iterate : Action<_>) =
        let mutable iterCount = 0
        while active.Count > 0 && active.Top.first <= time do
            iterCount <- iterCount + 1
            let e = active.Dequeue()
            iterate.Invoke e
        iterCount > 0
    member c.Step deltaTime =
        time <- time + deltaTime
    member c.Clear() =
        active.Clear()
        time <- 0
    override c.ToString() =
        sprintf "Timed: %d total, due: %s" active.Count
            (String.Join(", ", active.Items |> Seq.map (fun p -> p.first)))
            
type internal CoroutineScheduler() =
    let timed = TimeScheduler()
    let stacked = StackScheduler()
    let iterate = 
        Action<_>(fun (e : IEnumerator<_>) ->
            let isContinued = 
                try 
                    e.MoveNext()
                with
                | ex -> 
                    let str = sprintf "Error in coroutine %s" (e.ToString())
                    exn(str, ex) |> raise                
            if isContinued then 
                // Add to queue based on scheduling type:
                // - Non-negative indicates we want to wait for some duration
                // - Negative indicates we want to wait on nested coroutines
                let isTimed = e.Current.duration >= 0        
                if isTimed then timed.Enqueue e else stacked.Enqueue e)
    member c.Schedule coroutine =
        stacked.Schedule coroutine
    member c.RunOnce() =
        let hasStacked = stacked.RunOnce(iterate)
        let hasTimed = timed.RunOnce(iterate)
        hasStacked || hasTimed
    member c.Run() =
        while c.RunOnce() do ()      
    member c.Step deltaTime =
        timed.Step deltaTime
    member c.Clear() =
        timed.Clear()
        stacked.Clear()
    override c.ToString() =
        sprintf "Coroutines\n  %s\n  %s" (stacked.ToString()) (timed.ToString())

[<AutoOpen>]
module CoroutineChannelExtensions =
    type Channel<'a> with    
        member c.Wait(msg) =
            c.Send(msg)
            Wait.defer

    type IChannels with
        member c.Wait(msg) =
            c.GetChannel<'a>().Wait msg
