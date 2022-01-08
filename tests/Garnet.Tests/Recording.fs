namespace Garnet.Streaming

open System
open System.IO
open System.Collections.Generic
open System.Text
open Garnet.Composition.Comparisons
open Garnet.Composition

type ActorLogFilter = {
    actorFilter : ActorId -> bool
    sourceFilter : ActorId -> bool
    destinationFilter : ActorId -> bool
    }

module ActorLogFilter =
    let isAny (_ : ActorId) = true

    let all = {
        actorFilter = isAny
        sourceFilter = isAny
        destinationFilter = isAny
        }

    let singleLog actorId = {
        actorFilter = (=)actorId
        sourceFilter = isAny
        destinationFilter = isAny
        }

    let toActor actorId = {
        actorFilter = (=)actorId
        sourceFilter = isAny
        destinationFilter = isAny
        }

    let receivedBy actorId = {
        actorFilter = (=)actorId
        sourceFilter = isAny
        destinationFilter = (=)actorId
        }

    let sent actorId = {
        actorFilter = (=)actorId
        sourceFilter = isAny
        destinationFilter = (<>)actorId
        }

    let sentTo actorId destinationId = {
        actorFilter = (=)actorId
        sourceFilter = isAny
        destinationFilter = (=)destinationId
        }

[<Struct>]
type MessageRange = {
    messageTypeId : int
    start : int
    count : int 
    }
        
module MessageRange =
    let init id start count = { 
        messageTypeId = id
        start = start
        count = count }

    let typeCount id count = init id 0 count
    let count count = typeCount 0 count
    let all = count Int32.MaxValue

type PrintOptions = {
    createMessageRegistry : unit -> MessageRegistry
    createFormatter : unit -> Formatter
    print : string -> unit
    filter : ActorLogFilter
    range : MessageRange
    }

module PrintOptions =
    let range r opt = { opt with range = r }
    let filtered filter opt = { opt with filter = filter }
    let count c opt = range (MessageRange.count c) opt

type ReplayOptions = {
    createMessageRegistry : unit -> MessageRegistry
    filter : ActorLogFilter
    range : MessageRange
    }

type IActorStreamReader =
    abstract member GetActorIds : unit -> ActorId seq
    abstract member OpenRead : ActorId -> Stream

type IActorStreamWriter =
    abstract member OpenWrite : ActorId -> Stream

type IActorStreamSource =
    inherit IActorStreamReader
    inherit IActorStreamWriter

/// Multiple files in a directory
/// Stream lookup is thread-safe, but reading/writing is not
type DirectoryActorStreamSource(path) =
    let prefix = "actor-"
    let extension = ".log"
    let sync = obj()
    let getFullPath (actorId : ActorId) =
        let file = sprintf "%s%d%s" prefix actorId.Value extension
        Path.Combine(path, file)
    interface IActorStreamSource with
        member c.GetActorIds() =
            lock sync <| fun () ->
                Directory.EnumerateFiles(path, prefix + "*" + extension)
                |> Seq.map (fun file -> 
                    // remove prefix and extension, then extract number 
                    let name = Path.GetFileNameWithoutExtension(file)
                    Int32.Parse(name.Replace(prefix, "")) |> ActorId)
                |> Seq.cache
        member c.OpenRead (id : ActorId) =
            lock sync <| fun () ->
                let fullPath = getFullPath id
                File.OpenRead(fullPath) :> Stream
        member c.OpenWrite (id : ActorId) =
            lock sync <| fun () ->
                let fullPath = getFullPath id
                Directory.CreateDirectory path |> ignore
                File.OpenWrite(fullPath) :> Stream
        
/// Single file/stream
type FileActorStreamSource(path) =
    let actorIds = [| ActorId.Undefined |] :> seq<_>
    interface IActorStreamSource with
        member c.GetActorIds() = actorIds
        member c.OpenRead (id : ActorId) =
            File.OpenRead(path) :> Stream
        member c.OpenWrite (id : ActorId) =
            File.OpenWrite(path) :> Stream

/// Note Dispose() is absent
type private NonDisposingStream(stream : Stream) =
    inherit Stream()
    override c.Position
        with get() = stream.Position
        and set value = stream.Position <- value
    override c.CanRead = stream.CanRead
    override c.CanWrite = stream.CanWrite
    override c.CanSeek = stream.CanSeek
    override c.Length = stream.Length
    override c.Write(input, offset, count) =
        stream.Write(input, offset, count)
    override c.Read(output, offset, count) =
        stream.Read(output, offset, count)
    override c.Flush() = stream.Flush()
    override c.Seek(offset, origin) =
        stream.Seek(offset, origin)
    override c.SetLength(length) =
        stream.SetLength(length)

/// Stream lookup is thread-safe, but reading/writing is not
type MemoryActorStreamSource() =
    let logs = Dictionary<_,_>()
    let sync = obj()
    member private c.Open (id : ActorId) =
        lock sync <| fun () ->
            match logs.TryGetValue id with
            | true, x -> x
            | false, _ ->
                let ms = new MemoryStream()
                logs.Add(id, ms)
                ms
    member c.OpenWrite (id : ActorId) =
        new NonDisposingStream(c.Open id) :> Stream
    member c.GetActorIds() =
        lock sync <| fun () ->
            logs.Keys |> Seq.cache
    member c.OpenRead (id : ActorId) =
        let ms = c.Open id
        let length = int ms.Length
        let buffer = ms.GetBuffer()
        new MemoryStream(buffer, 0, length, false) :> Stream 
    interface IActorStreamSource with
        member c.OpenWrite id =
            c.OpenWrite id
        member c.GetActorIds() = 
            c.GetActorIds()
        member c.OpenRead id =
            c.OpenRead id
    override c.ToString() =
        String.Join("\n",
            logs |> Seq.map (fun kvp ->
                sprintf "%d: %d" kvp.Key.Value kvp.Value.Length))

[<Struct>]
type ActorLogCommand = {
    enableActorLog : bool 
    }
    