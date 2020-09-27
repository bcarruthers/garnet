namespace Garnet

open System

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

type internal ResizableBuffer<'a>(capacity) =
    let mutable array = Array.zeroCreate<'a> capacity
    let mutable count = 0
    member c.Item 
        with get i = array.[i]
        and set i x = array.[i] <- x
    member c.Array = array
    member c.Count = count
    member c.Capacity = array.Length
    member c.Buffer = ReadOnlyMemory(array, 0, count)
    member c.Add x = Buffer.addToArray &count &array x
    member c.AddAll x = Buffer.addAllToArray &count &array x
    member c.RemoveLast() =
        count <- count - 1
    member c.Clear() = count <- 0
    