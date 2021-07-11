namespace Garnet.Samples.Engine

open System
open System.Collections.Generic

/// Mutable min-heap
type Heap<'k, 'a when 'k :> IComparable<'k>>() =
    // create a dummy value for easier indexing
    let items = List<KeyValuePair<'k, 'a>>()
    do items.Add(Unchecked.defaultof<_>)
    let compare a b = 
        items.[a].Key.CompareTo(items.[b].Key)
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
    member h.Count = items.Count - 1
    member h.Top = items.[1]
    member h.Insert(key, value) =
        items.Add(KeyValuePair(key, value))
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

/// Mutable, min queue (min priority value dequeued first)
type PriorityQueue<'k, 'a when 'k :> IComparable<'k>>() =
    let heap = Heap<'k, 'a>()
    member _.Count = heap.Count
    member _.Top = heap.Top
    member _.Enqueue(priority, value) =
        heap.Insert(priority, value)
    member _.Dequeue() =
        heap.RemoveMin().Value
    member _.Clear() = 
        heap.Clear()
        