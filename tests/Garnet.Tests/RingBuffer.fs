namespace Garnet.Collections

open System
open System.Threading
open Garnet.Comparisons

module RingBuffer =
    let defaultBufferSize = 32

/// Single producer, single consumer
type RingBuffer<'a>(size) =
    let buffer = Array.zeroCreate<'a> (size)
    [<VolatileField>]
    let mutable readPos = 0
    [<VolatileField>]
    let mutable writePos = 0
    new() = new RingBuffer<'a>(RingBuffer.defaultBufferSize)
    /// Assumes single thread access
    member c.TryEnqueue(item) =
        if writePos - readPos = buffer.Length then false
        else
            let index = writePos &&& (buffer.Length - 1)
            buffer.[index] <- item
            writePos <- writePos + 1
            true
    /// Assumes single thread access
    member c.TryDequeue(item : byref<'a>) =
        if readPos = writePos then false
        else
            let index = readPos &&& (buffer.Length - 1)
            item <- buffer.[index]
            readPos <- readPos + 1
            true

/// Single producer, single consumer
[<AllowNullLiteral>]
type RingBufferNode<'a>(size) =
    let buffer = RingBuffer(size)
    [<VolatileField>]
    let mutable next = null
    new() = new RingBufferNode<'a>(RingBuffer.defaultBufferSize)
    member c.Enqueue(item) =
        // first try to add to current
        if buffer.TryEnqueue(item) then c
        else
            // if full, create a new node
            // next will only ever be set to non-null
            // need to guarantee this value will be written AFTER ring buffer
            // increment, otherwise consumer could miss an item
            next <- RingBufferNode(size * 2)
            next.Enqueue(item)
    /// Returns the node item was obtained from, or null if no item available
    member c.TryDequeue(item : byref<'a>) =
        // first look in current
        if buffer.TryDequeue(&item) then c
        // if empty, then either no items or writer moved onto another buffer
        // if another buffer, we can safely discard current buffer
        elif isNotNull next then next.TryDequeue(&item)
        else null
    member c.NodeCount = 
        if isNull next then 1 else 1 + next.NodeCount
            
/// Single producer, single consumer
type RingBufferQueue<'a>(initialSize) =
    let mutable count = 0
    let mutable enqueueCount = 0
    let mutable dequeueCount = 0
    let mutable allocatedCount = 1
    [<VolatileField>]
    let mutable readNode = RingBufferNode<'a>(initialSize)
    [<VolatileField>]
    let mutable writeNode = readNode
    new() = new RingBufferQueue<'a>(RingBuffer.defaultBufferSize)
    member c.Count = count
    member c.Enqueue(item) =
        writeNode <- writeNode.Enqueue(item)
        Interlocked.Increment(&count) |> ignore
        enqueueCount <- enqueueCount + 1
    member c.TryDequeue(item : byref<'a>) =
        let newReadNode = readNode.TryDequeue(&item)
        let isDequeued = isNotNull newReadNode
        if isDequeued then 
            if not (obj.ReferenceEquals(readNode, newReadNode)) then
                readNode <- newReadNode
                allocatedCount <- allocatedCount + 1
            Interlocked.Decrement(&count) |> ignore
            dequeueCount <- dequeueCount + 1
        isDequeued
    member c.DequeueAll(action : Action<_>) =
        let mutable item = Unchecked.defaultof<'a>
        while c.TryDequeue(&item) do
            action.Invoke item
    override c.ToString() =
        sprintf "%d items, %d/%d nodes, %d enqueued, %d dequeued" 
            count readNode.NodeCount allocatedCount enqueueCount dequeueCount
                
type RingBufferPool<'a>(create) =
    let pool = RingBufferQueue<'a>()
    let onDispose = Action<_>(pool.Enqueue)
    member c.Get() =
        let mutable item = Unchecked.defaultof<'a>
        if pool.TryDequeue(&item) then item
        else create onDispose
    override c.ToString() =
        pool.ToString()
