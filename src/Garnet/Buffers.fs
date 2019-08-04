namespace Garnet

open System
open System.Collections
open System.Collections.Generic

[<Struct>]
type BufferEnumerator<'a> =
    val private array : 'a[]
    val private max : int
    val mutable private offset : int
    new(arr, offset, max) = { array = arr; offset = offset - 1; max = max }
    member c.Reset() = ()
    member c.Dispose() = ()
    member c.Current = c.array.[c.offset]
    member c.MoveNext() =
        c.offset <- c.offset + 1
        c.offset < c.max
    interface IEnumerator with
        member c.Reset() = ()
        member c.Current = c.Current :> obj
        member c.MoveNext() = c.MoveNext()
    interface IEnumerator<'a> with
        member c.Dispose() = ()
        member c.Current = c.Current

[<Struct>]
type Buffer<'a> = {
    Array : 'a[]
    Offset : int
    Count : int 
    } with
    member inline c.Item 
        with get i = c.Array.[c.Offset + i]
        and set i x = c.Array.[c.Offset + i] <- x
    member c.Clear() =
        Array.Clear(c.Array, c.Offset, c.Count)
    member c.GetEnumerator() =
        new BufferEnumerator<'a>(c.Array, c.Offset, c.Offset + c.Count)
    interface IEnumerable with
        member c.GetEnumerator() =
            c.GetEnumerator() :> IEnumerator
    interface IEnumerable<'a> with
        member c.GetEnumerator() =
            c.GetEnumerator() :> IEnumerator<'a>

module Buffer =
    let ofArraySubset arr offset count = {
        Array = arr
        Offset = offset
        Count = count
        }

    let ofArrayStart arr count = 
        ofArraySubset arr 0 count

    let ofArray (arr : _[]) =
        ofArrayStart arr arr.Length

    let ofSeq s =
        s |> Seq.toArray |> ofArray

    let zeroCreate<'a> x = {
        Array = Array.zeroCreate<'a> x
        Offset = 0
        Count = x
        }

    let single x =
        let b = zeroCreate<_> 1
        b.Array.[0] <- x
        b

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

type ResizableBuffer<'a>(capacity) =
    let mutable array = Array.zeroCreate<'a> capacity
    let mutable count = 0
    member c.Item 
        with get i = array.[i]
        and set i x = array.[i] <- x
    member c.Array = array
    member c.Count = count
    member c.Capacity = array.Length
    member c.Buffer = Buffer.ofArrayStart array count
    member c.Add x = Buffer.addToArray &count &array x
    member c.Clear() = count <- 0
    member c.GetEnumerator() =
        new BufferEnumerator<'a>(array, 0, count)
    interface IEnumerable with
        member c.GetEnumerator() =
            c.GetEnumerator() :> IEnumerator
    interface IEnumerable<'a> with
        member c.GetEnumerator() =
            c.GetEnumerator() :> IEnumerator<'a>
    