namespace Garnet.Streaming

open System
open System.IO
open System.Collections.Generic
open System.Text
open Garnet.Composition.Comparisons
open Garnet.Composition

/// Logs incoming and outgoing messages
type LogInbox(actorId : ActorId, baseHandler : IInbox, logger : IInbox) =
    let nullOutbox = NullOutbox() :> IOutbox
    let outbox = LogMessageOutbox(actorId, logger)
    interface IInbox with
        member c.Receive e =
            // log incoming message
            logger.Receive e
            // redirect logging outbox
            outbox.SetOutbox e.Outbox
            // handle message using logging outbox
            // so outgoing messages will go to log first, then to actual outbox
            try baseHandler.Receive { e with Outbox = outbox }
            // revert logging outbox
            finally outbox.SetOutbox nullOutbox

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
    
type PrintInbox(id : ActorId, formatter : IFormatter, counter : ref<int>, print) =
    let formatMessagesTo (sb : StringBuilder) (formatMsg : _ -> string) (batch : ReadOnlySpan<_>) maxCount =
        let printCount = min batch.Length maxCount
        for i = 0 to printCount - 1 do
            let msg = batch.[i]
            sb.AppendLine() |> ignore
            sb.Append("  ") |> ignore
            sb.Append(formatMsg msg) |> ignore
            //sb.Append(sprintf "%A" msg) |> ignore
        // count of messages not printed
        let remaining = batch.Length - printCount
        if remaining > 0 then
            sb.AppendLine() |> ignore
            sb.Append(sprintf "  +%d" remaining) |> ignore
    let sb = System.Text.StringBuilder()
    let mutable batchCount = 0
    let mutable messageCount = 0
    let mutable isEnabled = false
    let mutable maxMessages = 10
    let handler = 
        let h = Mailbox()
        h.On<ActorLogCommand> <| fun e -> isEnabled <- e.enableActorLog
        h :> IInbox
    member c.IsEnabled 
        with get() = isEnabled
        and set value = isEnabled <- value
    interface IInbox with
        member c.Receive<'a> (e : Envelope<ReadOnlyMemory<'a>>) =
            // print if enabled before or after
            let isEnabledBefore = isEnabled
            handler.Receive e
            let isEnabledAfter = isEnabled
            if (isEnabledBefore || isEnabledAfter) && formatter.CanFormat<'a>() then 
                sb.Append(sprintf "%d: %d->%d %d/%d/%d: %dx %s" 
                    id.Value e.SourceId.Value e.DestinationId.Value 
                    counter.Value batchCount messageCount
                    e.Message.Length (typeof<'a>.Name)) |> ignore
                formatMessagesTo sb formatter.Format e.Message.Span maxMessages
                sb.ToString() |> print//printfn "%s"
                sb.Clear() |> ignore
            counter.Value <- counter.Value + 1
            batchCount <- batchCount + 1
            messageCount <- messageCount + e.Message.Length
          
[<AutoOpen>]
module internal RecordingInternal =
    let getSentCount (sender : StreamMessageSender) typeId = 
        if typeId = 0 then sender.SentCount else sender.GetCount typeId
            
    let seek reg filter typeId count ms =
        let sender = StreamMessageSender(reg, filter)
        let nullOutbox = NullOutbox()
        while getSentCount sender typeId < count && sender.Send(ms, nullOutbox) do
            ()

    let replayTo (a : IMessagePump) reg filter range ms =
        let filter = fun (h : MessageHeader) -> filter.sourceFilter h.sourceId
        seek reg filter range.messageTypeId range.start ms
        let sender = StreamMessageSender(reg, filter)
        while getSentCount sender range.messageTypeId < range.count && sender.Send(ms, a) do
            a.ProcessAll()

    let createPrintActorSystem (options : PrintOptions) =
        // share counter/formatter since no threads
        let formatter = options.createFormatter()
        let counter = ref 0
        let a = new ActorSystem(workerCount = 0)
        a.Register(options.filter.destinationFilter, fun createId ->
            let h = PrintInbox(createId, formatter, counter, options.print) 
            h.IsEnabled <- true
            Actor(h))
        a
