namespace Garnet.Streaming

open System
open System.IO
open System.Collections.Generic
open Garnet
//open Garnet.Comparisons
open Garnet.Formatting
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
            outbox.SetOutbox e.outbox
            // handle message using logging outbox
            // so outgoing messages will go to log first, then to actual outbox
            try baseHandler.Receive { e with outbox = outbox }
            // revert logging outbox
            finally outbox.SetOutbox nullOutbox

type ActorLogFilter = {
    actorFilter : ActorId -> bool
    sourceFilter : ActorId -> bool
    destinationFilter : ActorId -> bool
    }

module ActorLogFilter =
    let all = {
        actorFilter = ActorId.isAny
        sourceFilter = ActorId.isAny
        destinationFilter = ActorId.isAny
        }

    let singleLog actorId = {
        actorFilter = (=)actorId
        sourceFilter = ActorId.isAny
        destinationFilter = ActorId.isAny
        }

    let toActor actorId = {
        actorFilter = (=)actorId
        sourceFilter = ActorId.isAny
        destinationFilter = ActorId.isAny
        }

    let receivedBy actorId = {
        actorFilter = (=)actorId
        sourceFilter = ActorId.isAny
        destinationFilter = (=)actorId
        }

    let sent actorId = {
        actorFilter = (=)actorId
        sourceFilter = ActorId.isAny
        destinationFilter = (<>)actorId
        }

    let sentTo actorId destinationId = {
        actorFilter = (=)actorId
        sourceFilter = ActorId.isAny
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
        let file = sprintf "%s%d%s" prefix actorId.value extension
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
    let actorIds = [| ActorId.undefined |] :> seq<_>
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
                sprintf "%d: %d" kvp.Key.value kvp.Value.Length))

[<Struct>]
type ActorLogCommand = {
    enableActorLog : bool }
    
type PrintInbox(id : ActorId, formatter : IFormatter, counter : ref<int>, print) =
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
                    id.value e.sourceId.value e.destinationId.value 
                    counter.Value batchCount messageCount
                    e.message.Length (typeof<'a> |> typeToString)) |> ignore
                let typeInfo = CachedTypeInfo<'a>.Info
                if not typeInfo.isEmpty then
                    formatMessagesTo sb formatter.Format e.message.Span maxMessages
                sb.ToString() |> print//printfn "%s"
                sb.Clear() |> ignore
            counter.Value <- counter.Value + 1
            batchCount <- batchCount + 1
            messageCount <- messageCount + e.message.Length
          
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
            a.RunAll()

    let createPrintActorSystem (options : PrintOptions) =
        // share counter/formatter since no threads
        let formatter = options.createFormatter()
        let counter = ref 0
        let a = new ActorSystem(threadCount = 0)
        a.Register(fun id ->
            if options.filter.destinationFilter id then
                let h = PrintInbox(id, formatter, counter, options.print) 
                h.IsEnabled <- true
                h |> Actor.inbox
            else Actor.none)
        a

[<AutoOpen>]
module Recording =          
    module Actor =        
        let print id formatter =
            PrintInbox(id, formatter, ref 0, printfn "%s") 
            |> Actor.inbox
    
    module ActorFactory =
        let withPrinting createFormatter print createActor =
            fun id ->
                let formatter = createFormatter()
                let h = PrintInbox(id, formatter, ref 0, print) 
                h.IsEnabled <- true
                Actor.combine [
                    createActor id
                    Actor.inbox h
                    ]
            
        let withLogging createRegistry openStream createActor =
            fun id ->
                let def = createActor id
                let stream = openStream id
                let registry = createRegistry()
                let logHandler = StreamInbox(registry, stream)
                { def with
                    dispose = fun () ->
                        def.dispose()
                        stream.Dispose()
                    inbox = 
                        LogInbox(id, def.inbox, logHandler)
                        :> IInbox
                }

    type IActorStreamReader with
        member c.CopyTo (writer : IActorStreamWriter) =
            for id in c.GetActorIds() do
                use input = c.OpenRead id
                use output = writer.OpenWrite id
                input.CopyTo(output)

        member c.Save path =
            c.CopyTo (FileActorStreamSource(path))

        member c.SaveTimestamped path =
            let path = Path.Combine(path, DateTime.Now.ToString("yyyyMMdd-hhmmss"))
            c.Save path

        member c.Replay (a : IMessagePump) (options : ReplayOptions) =
            let actorIds =
                c.GetActorIds() 
                |> Seq.filter options.filter.actorFilter
                |> Seq.toArray
            for actorId in actorIds do
                let reg = options.createMessageRegistry()
                use input = c.OpenRead actorId
                replayTo a reg options.filter options.range input

        member c.Print (options : PrintOptions) =
            let actorIds =
                c.GetActorIds() 
                |> Seq.filter options.filter.actorFilter
                |> Seq.toArray
            let a = createPrintActorSystem options
            for actorId in actorIds do
                options.print (sprintf "Log %A --------------------------" actorId)
                let reg = options.createMessageRegistry()
                use input = c.OpenRead actorId
                replayTo a reg options.filter options.range input
