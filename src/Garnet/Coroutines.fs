namespace Garnet.Composition

open System
open System.Collections.Generic
open Garnet.Comparisons
open Garnet.Collections

[<Struct>]
type Wait = {
    duration : int64
    }
    
module Wait =
    let time x = { Wait.duration = x }
    let defer = time -1L
        
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
    let mutable time = 0L
    let active = PriorityQueue<int64,_>()
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
        time <- 0L
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
                let isTimed = e.Current.duration >= 0L
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
