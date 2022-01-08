namespace Garnet.Streaming

open System
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices
open Garnet.Composition

type ISerializer<'a> =
    abstract member Write : Stream -> 'a -> unit
    abstract member Read : Stream -> 'a

type Serializer<'a>(read, write) =
    interface ISerializer<'a> with
        member c.Write output value = write value output
        member c.Read input = read input

[<AutoOpen>]
module internal Serialization =
    let getNextPow2 x =
        let mutable y = x - 1
        y <- y ||| (y >>> 1)
        y <- y ||| (y >>> 2)
        y <- y ||| (y >>> 4)
        y <- y ||| (y >>> 8)
        y <- y ||| (y >>> 16)
        y <- y + 1
        if y > 0 then y else 1

    let checkBlittable<'a>() =
        try
            let x = Array.zeroCreate<'a> 1
            let handle = GCHandle.Alloc(x, GCHandleType.Pinned)
            handle.Free()
            true
        with _ ->
            false

    let copyFromBytes<'a> (src : byte[]) (dest : 'a[]) =
        let handle = GCHandle.Alloc(dest, GCHandleType.Pinned)
        let ptr = handle.AddrOfPinnedObject()
        Marshal.Copy(src, 0, ptr, src.Length)
        handle.Free()

    let copyToBytes<'a> (src : 'a[]) (dest : byte[]) =
        let handle = GCHandle.Alloc(src, GCHandleType.Pinned)
        let ptr = handle.AddrOfPinnedObject()
        Marshal.Copy(ptr, dest, 0, dest.Length)
        handle.Free()

    let serializer read write = Serializer(read, write)
    
    let isEmptyType (t : Type) =
        not (t.IsPrimitive || t.IsEnum || t.GetProperties().Length > 0)

module internal LittleEndian =
    let writeInt32 (bytes : byte[]) offset x =
        bytes.[offset] <- x &&& 0xff |> byte
        bytes.[offset + 1] <- (x >>> 8) &&& 0xff |> byte
        bytes.[offset + 2] <- (x >>> 16) &&& 0xff |> byte
        bytes.[offset + 3] <- (x >>> 24) &&& 0xff |> byte
        
type StringSerializer() =
    let mutable buffer = Array.zeroCreate<byte> 4
    let resize length =
        if buffer.Length < length then
            Array.Resize(&buffer, getNextPow2 length)
    interface ISerializer<string> with
        member c.Read stream =
            stream.Read(buffer, 0, 4) |> ignore
            let length = BitConverter.ToInt32(buffer, 0)
            resize length
            stream.Read(buffer, 0, length) |> ignore
            System.Text.Encoding.UTF8.GetString(buffer, 0, length)
        member c.Write stream value =
            LittleEndian.writeInt32 buffer 0 value.Length
            stream.Write(buffer, 0, 4)
            let length = System.Text.Encoding.UTF8.GetByteCount(value)
            resize length
            System.Text.Encoding.UTF8.GetBytes(value, 0, value.Length, buffer, 0) |> ignore
            stream.Write(buffer, 0, length)

type RawSerializer<'a>() =
    let bytes = Array.zeroCreate<byte> sizeof<'a>
    let data = Array.zeroCreate<'a> 1
    // for types without members, avoid writing any bytes
    let storedLength = if isEmptyType typeof<'a> then 0 else bytes.Length
    do
        if not (checkBlittable<'a>()) then
            failwithf "Type %s is not blittable" (typeof<'a>.Name)
    interface ISerializer<'a> with
        member c.Write (output : Stream) (value : 'a) =
            data.[0] <- value
            copyToBytes data bytes
            output.Write(bytes, 0, storedLength)
        member c.Read(input : Stream) =
            let count = input.Read(bytes, 0, storedLength) 
            if count < bytes.Length then Unchecked.defaultof<'a>
            else
                copyFromBytes bytes data
                data.[0]

type Accessor<'a, 'b> = {
    wrap : 'a -> 'b
    unwrap : 'b -> 'a
    }

type WrappedStringSerializer<'a>(accessor) =
    let serializer = StringSerializer() :> ISerializer<string>
    interface ISerializer<'a> with
        member c.Write (output : Stream) (value : 'a) =
            serializer.Write output (accessor.unwrap value)
        member c.Read(input : Stream) =
            serializer.Read input |> accessor.wrap
                    
type IFactory =
    abstract member Create<'a> : unit -> obj

type MessageTypeInfo = {
    messageType : Type
    typeId : int
    serializer : obj
    create : Func<IFactory, obj>
    }

[<Struct>]
type MessageTypeInfo<'a> = {
    typeId : int
    serializer : ISerializer<'a>
    }

type MessageRegistry() =
    let typeLookup = Dictionary<Type, MessageTypeInfo>()
    let idLookup = Dictionary<int, MessageTypeInfo>()
    member c.Get<'a>() =
        let t = typeof<'a>
        match typeLookup.TryGetValue(t) with
        | false, _ -> failwithf "Message type %s not registered" t.Name
        | true, info ->
            {   typeId = info.typeId
                serializer = info.serializer :?> ISerializer<'a>
                }
    member c.Get id =
        match idLookup.TryGetValue(id) with
        | false, _ -> failwithf "Message ID %d not registered" id
        | true, info -> info
    member c.Ignore<'a>() =
        let read s = Unchecked.defaultof<'a>
        let write value s = ()
        c.Register<'a> 0 (serializer read write)
    member c.Register<'a> id (serializer : ISerializer<'a>) =
        let t = typeof<'a>
        let info = {
            messageType = t
            typeId = id 
            serializer = serializer
            create = Func<_,_>(fun factory -> factory.Create<'a>())
            }
        typeLookup.Add(t, info)
        if id <> 0 then
            idLookup.Add(id, info)

[<Struct>]
type MessageHeader = {
    sourceId : ActorId
    destinationId : ActorId
    messageTypeId : int
    messageCount : int
    }

module MessageHeader =
    let empty = {
        sourceId = ActorId 0
        destinationId = ActorId 0
        messageTypeId = 0
        messageCount = 0
        }
    
type IMessageStreamReader =
    abstract member Start : MessageHeader -> IOutbox -> unit
    abstract member Read : Stream -> unit
    abstract member Flush : unit -> unit
    