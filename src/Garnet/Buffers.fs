namespace Garnet

open System
open System.Buffers

module Buffer =
    let private log2 x =
        let mutable log = 0
        let mutable y = x
        while y > 1 do
            y <- y >>> 1
            log <- log + 1;
        log

    let private nextLog2 x =
        let log = log2 x
        if x - (1 <<< log) > 0 then 1 + log else log

    let internal resizeArray count (arr : byref<_[]>) =
        if count > arr.Length then
            let required = 1 <<< nextLog2 count
            let newArr = Array.zeroCreate required
            arr.CopyTo(newArr, 0)
            arr <- newArr

    let internal addToArray (count : byref<int>) (arr : byref<_[]>) x =
        count <- count + 1
        resizeArray count &arr
        arr.[count - 1] <- x

    let internal addAllToArray (count : byref<int>) (arr : byref<_[]>) (src : ReadOnlySpan<_>) =
        let destOffset = count
        count <- count + src.Length
        resizeArray count &arr
        let dest = Span(arr, destOffset, src.Length)
        src.CopyTo(dest)

/// Similar to ArrayBufferWriter, but provides additional read/write access to 
/// the underlying array.
type internal ResizableBuffer<'a>(capacity) =
    let mutable buffer = Array.zeroCreate<'a> capacity
    let mutable pos = 0
    member c.Item 
        with get i = buffer.[i]
        and set i x = buffer.[i] <- x
    member c.WrittenCount = pos
    member c.WrittenSpan =
        ReadOnlySpan(buffer, 0, pos)
    member c.WrittenMemory =
        ReadOnlyMemory(buffer, 0, pos)
    member c.GetMemory(count) =
        // note min allocation in case count is zero
        let required = pos + max count 8
        Buffer.resizeArray required &buffer
        buffer.AsMemory().Slice(pos)
    member c.GetSpan(count) =
        c.GetMemory(count).Span
    member c.Advance(count) = 
        if count < 0 then failwithf "Cannot advance a negative value: %d" count
        pos <- pos + count
    member c.WriteValue(value) =
        if pos >= buffer.Length then
            Buffer.resizeArray (pos + 1) &buffer
        buffer.[pos] <- value
        pos <- pos + 1
    member c.RemoveLast() =
        pos <- pos - 1
    member c.Clear() =
        Array.Clear(buffer, 0, buffer.Length)
        pos <- 0
    interface IBufferWriter<'a> with
        member c.GetSpan(count) =
            c.GetSpan(count)
        member c.GetMemory(count) =
            c.GetMemory(count)
        member c.Advance(count) = 
            c.Advance(count)        
    