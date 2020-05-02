namespace Garnet.Streaming

open System
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices
open Garnet
open Garnet.Formatting
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
            failwithf "Type %s is not blittable" (typeof<'a> |> typeToString)
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
        | false, _ -> failwithf "Message type %s not registered" (typeToString t)
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
    
[<AutoOpen>]
module internal Streaming =    
    type NullLogReader() =
        interface IMessageStreamReader with
            member c.Start _ _ = ()
            member c.Read stream = ()
            member c.Flush() = ()

    type LogReader<'a>(serializer : ISerializer<'a>) =
        let nullMessageWriter = new MessageWriter<'a>() :> IMessageWriter<'a>
        let mutable writer = nullMessageWriter
        interface IMessageStreamReader with
            member c.Start (header : MessageHeader) (outbox : IOutbox) =
                writer <- outbox.BeginSend<'a>()
                writer.SetSource header.sourceId
                writer.AddRecipient header.destinationId
            member c.Read stream =
                let msg = serializer.Read stream
                writer.Write msg
            member c.Flush() =
                writer.Dispose()
                writer <- nullMessageWriter

    type LogMessageWriter<'a>(actorId : ActorId, logger : IInbox, onDispose : Action<_>) =
        let nullOutbox = NullOutbox()
        let nullMessageWriter = new MessageWriter<'a>() :> IMessageWriter<'a>
        let logWriter = new MessageWriter<'a>()
        let mutable baseWriter = nullMessageWriter
        member c.SetWriter writer =
            baseWriter <- writer
        interface IMessageWriter<'a> with
            member c.SetSource id =
                logWriter.SetSource id
            member c.AddRecipient id =
                logWriter.AddRecipient id
            member c.Advance count =
                logWriter.Advance count
            member c.GetMemory minSize =
                logWriter.GetMemory minSize
            member c.GetSpan minSize =
                logWriter.GetSpan minSize
            member c.Dispose() = 
                // omitting any empty messages with the expectation
                // that the underlying writer will do the same
                if logWriter.Memory.Length > 0 then
                    for id in logWriter.Recipients do                    
                        logger.Receive { 
                            outbox = nullOutbox
                            sourceId = logWriter.SourceId
                            destinationId = id
                            message = logWriter.Memory
                            }
                logWriter.CopyTo(baseWriter)
                logWriter.Dispose()
                baseWriter.Dispose()
                baseWriter <- nullMessageWriter
                onDispose.Invoke c

    type LogMessageOutbox<'a>(actorId, logger) =
        let builders = Stack<_>()
        let recycle = Action<_>(builders.Push)
        member c.BeginSend(writer : IMessageWriter<'a>) = 
            let b =
                if builders.Count = 0 then new LogMessageWriter<'a>(actorId, logger, recycle)
                else builders.Pop()
            b.SetWriter writer
            b :> IMessageWriter<'a>

    type LogMessageOutbox(actorId, logger) =
        let nullOutbox = NullOutbox() :> IOutbox
        let mutable baseOutbox = nullOutbox
        let dict = Dictionary<Type, obj>()
        member private c.Get<'a>() =
            let t = typeof<'a>
            let pool =
                match dict.TryGetValue(t) with
                | true, x -> x :?> LogMessageOutbox<'a>
                | false, _ ->
                    let pool = new LogMessageOutbox<'a>(actorId, logger)
                    dict.Add(t, pool)
                    pool
            pool
        member c.SetOutbox outbox =
            baseOutbox <- outbox
        interface IOutbox with
            member c.BeginSend() = 
                let writer = baseOutbox.BeginSend()
                c.Get().BeginSend writer

type MessageStreamReaderPool(registry : MessageRegistry) =
    let dict = Dictionary<int, obj>()
    interface IFactory with
        member c.Create<'a>() =
            let info = registry.Get<'a>()
            new LogReader<'a>(info.serializer) :> obj
    member c.Get typeId =
        let reader =
            match dict.TryGetValue(typeId) with
            | true, x -> x
            | false, _ ->
                let info = registry.Get typeId
                let r = info.create.Invoke c
                dict.Add(typeId, r)
                r
        reader :?> IMessageStreamReader
    member c.Read (header : MessageHeader) stream outbox =
        let reader = c.Get header.messageTypeId
        reader.Start header outbox
        for i = 0 to header.messageCount - 1 do
            reader.Read stream
        reader.Flush()

type internal IntDictionary<'a when 'a : equality>() =
    let counts = Dictionary<'a, int>()
    member c.Item 
        with get key = 
            match counts.TryGetValue(key) with
            | true, x -> x
            | false, _ -> 0
        and set key value =
            if value = 0 then counts.Remove(key) |> ignore
            else counts.[key] <- value
    member c.Add key delta =
        c.[key] <- c.[key] + delta

/// Reads messages from stream and sends them to outbox
type StreamMessageSender(registry : MessageRegistry, filter) =
    let nullOutbox = NullOutbox() :> IOutbox
    let pool = MessageStreamReaderPool(registry)
    let headerInfo = registry.Get<MessageHeader>()
    let typeCounts = IntDictionary()
    let mutable sentCount = 0
    member c.SentCount = sentCount
    member c.GetCount typeId = typeCounts.[typeId]
    member c.Send(stream : Stream, outbox : IOutbox) =
        let header = headerInfo.serializer.Read stream
        typeCounts.Add header.messageTypeId 1
        let canRead = header.messageTypeId <> 0
        if canRead then 
            let outbox = 
                if filter header then 
                    sentCount <- sentCount + 1
                    outbox 
                else nullOutbox
            try
                pool.Read header stream outbox
            with ex ->
                let msg = 
                    sprintf "Failed reading at %d/%d, header: %A" 
                        stream.Position stream.Length header
                exn(msg, ex) |> raise
        canRead
    member c.Send(stream, outbox, count) =
        while c.SentCount < count && c.Send(stream, outbox) do ()
    member c.SendAll(stream, outbox) =
        while c.Send(stream, outbox) do ()

/// Serializes individual messages to stream
type StreamInbox(registry : MessageRegistry, stream : Stream) =
    let headerInfo = registry.Get<MessageHeader>()
    interface IInbox with
        member c.Receive<'a> (e : Envelope<Memory<'a>>) =     
            StreamInbox.Write registry headerInfo stream e
    static member Write<'a> (registry : MessageRegistry) headerInfo (stream : Stream) (e : Envelope<Memory<'a>>) =
        let info = registry.Get<'a>()
        if info.typeId <> 0 then
            let header = {
                sourceId = e.sourceId
                destinationId = e.destinationId
                messageTypeId = info.typeId
                messageCount = e.message.Length
                }
            headerInfo.serializer.Write stream header
            for msg in e.message.Span do
                info.serializer.Write stream msg
