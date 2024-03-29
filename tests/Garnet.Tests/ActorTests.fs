module Garnet.Tests.Actors

open System
open System.Collections.Generic
open System.IO
open System.Threading
open Expecto
open Garnet.Composition
open Garnet.Streaming

type Run = struct end
type Ping = struct end
type Pong = struct end

type Inbox() =
    let dict = Dictionary<Type, obj>()
    member c.OnAll<'a>(action : Message<'a> -> unit) =
        let t = typeof<'a>
        let combined =
            match dict.TryGetValue t with
            | false, _ -> action
            | true, existing -> 
                let existing = existing :?> (Message<'a> -> unit)
                fun e -> 
                    existing e
                    action e        
        dict.[t] <- combined
    member c.TryReceive<'a> e =
        match dict.TryGetValue(typeof<'a>) with
        | true, x -> 
            let handle = x :?> (Message<'a> -> unit)
            handle e
            true
        | false, _ -> false
    interface IInbox with
        member c.Receive(outbox, message) =
            c.TryReceive(message) |> ignore

let bgDispatcherId = 0
let mainDispatcherId = 1

let runPingPong onPing onPong iterations =
    let mutable count = 0
    use a = new ActorSystem(0)
    a.Register(ActorId 1, fun _ ->
        let h = Mailbox()
        h.On<Run> <| fun e ->
            h.Send(ActorId 2, Ping())
        h.On<Pong> <| fun e ->
            count <- count + 1
            if count < iterations then
                onPing(e)
                h.Respond(Ping())
        Actor(h))
    a.Register(ActorId 2, fun _ -> 
        let h = Mailbox()
        h.On<Ping> <| fun e -> 
            onPong e
            h.Respond(Pong())
        Actor(h))
    a.Process(ActorId 1, Run())
    a.ProcessAll()
    count

let sendReceiveMessages send =
    let results = List<_>()
    use a = new ActorSystem(0)
    let h = Inbox()
    h.OnAll<int> <| fun msg ->
        results.Add {
            Buffer = ReadOnlySpan(msg.Buffer, 0, msg.Count).ToArray()
            Pool = null
            Count = msg.Count
            SourceId = msg.SourceId
            DestinationId = msg.DestinationId
            }
    a.Register(ActorId 1, fun _ -> Actor(h))
    send (a.Get(ActorId 1))
    a.ProcessAll()
    results |> List.ofSeq
    
[<Tests>]
let tests =
    testList "actors" [            
        testCase "send to undefined actor" <| fun () ->
            use a = new ActorSystem()
            a.Send(ActorId 1, 10)
            a.ProcessAll()

        testCase "send batch" <| fun () ->
            let results = sendReceiveMessages <| fun a ->
                a.SendAll(ReadOnlyMemory([| 1; 2; 3 |]).Span)
            let r = List.head results
            r.SourceId |> shouldEqual (ActorId 0)
            r.DestinationId |> shouldEqual (ActorId 1)
            r.Buffer |> shouldEqual [| 1; 2; 3 |]

        testCase "send single" <| fun () ->
            let results = sendReceiveMessages <| fun a ->
                a.Send(1)                
            let r = List.head results
            r.SourceId |> shouldEqual (ActorId 0)
            r.DestinationId |> shouldEqual (ActorId 1)
            r.Buffer |> shouldEqual [| 1 |]

        testCase "send single with source" <| fun () ->
            let results = sendReceiveMessages <| fun a ->
                a.Send(1, sourceId = ActorId 2)                
            let r = List.head results
            r.SourceId |> shouldEqual (ActorId 2)
            r.DestinationId |> shouldEqual (ActorId 1)
            r.Buffer |> shouldEqual [| 1 |]

        testCase "send to any actor" <| fun () ->
            let msgs = List<_>()
            let inbox = Mailbox()
            inbox.On<int> msgs.Add
            use a = new ActorSystem()
            a.Register(fun id -> Actor(inbox))
            a.Send(ActorId 1, 10)
            a.ProcessAll()
            msgs.Count |> shouldEqual 1

        testCase "create ping pong actors" <| fun () ->
            let iterations = 10
            runPingPong ignore ignore iterations |> shouldEqual iterations

        testCase "send message to self" <| fun () ->
            let mutable count = 0
            use a = new ActorSystem(0)
            a.Register(ActorId 1, fun _ ->
                let c = Mailbox()
                c.On<int> <| fun e ->
                    if e < 10 then
                        count <- count + 1
                        c.Send(ActorId 1, e + 1)
                Actor(c, mainDispatcherId))
            a.Process(ActorId 1, 0)
            count |> shouldEqual 10

        testCase "send message to other" <| fun () ->
            let mutable count1 = 0
            let mutable count2 = 0
            use a = new ActorSystem(0)
            a.Register(ActorId 1, fun _ ->
                let c = Mailbox()
                c.On<int> <| fun e ->
                    if e < 10 then
                        count1 <- count1 + 1
                        c.Send(ActorId 2, e + 1)
                Actor(c))
            a.Register(ActorId 2, fun _ ->
                let c = Mailbox()
                c.On<int> <| fun e ->
                    if e < 10 then
                        count2 <- count2 + 1
                        c.Send(ActorId 1, e + 1)
                Actor(c))
            a.Process(ActorId 1, 0)
            count1 |> shouldEqual 5
            count2 |> shouldEqual 5

        testCase "send message to background actor" <| fun () ->
            let mutable count1 = 0
            let mutable count2 = 0
            use a = new ActorSystem(1)
            a.Register(ActorId 1, fun _ ->
                let c = Mailbox()
                c.On<Thread> <| fun e ->
                    // bg thread should be different
                    Expect.notEqual Thread.CurrentThread.ManagedThreadId e.ManagedThreadId ""
                c.On<int> <| fun e ->
                    if e < 10 then
                        count1 <- count1 + 1
                        //printfn "FG: %d" Thread.CurrentThread.ManagedThreadId
                        c.Send(ActorId 2, e + 1)
                Actor(c, mainDispatcherId))
            a.Register(ActorId 2, fun _ ->
                let c = Mailbox()
                c.On<int> <| fun e ->
                    if e < 10 then
                        count2 <- count2 + 1
                        //printfn "BG: %d" Thread.CurrentThread.ManagedThreadId
                        c.Send(ActorId 1, Thread.CurrentThread)
                        c.Send(ActorId 1, e + 1)
                Actor(c))    
            a.Process(ActorId 1, 0)
            a.ProcessAll()
            count1 |> shouldEqual 5
            count2 |> shouldEqual 5

        testCase "respond to messages" <| fun () ->
            let config = { 
                Dispatchers = 
                    [|
                        {
                            Name = ""
                            DispatcherType = DispatcherType.Background
                            ThreadCount = 2
                            Throughput = 100
                        }                
                    |]
                }
            use a = new ActorSystem(config)
            a.Register(ActorFactory.Create(fun _ m ->
                m.On<Ping> <| fun e -> 
                    //printfn "Responding from %A to %A" m.DestinationId m.SourceId
                    m.Respond(Pong())))
            for i = 1 to 10 do
                a.Send(ActorId i, ActorId (i * 2), Ping())
            a.ProcessAll()

        testCase "send random messages to background actors" <| fun () ->
            use a = new ActorSystem(2)
            a.Register(fun (createId : ActorId) -> 
                let rand = Random(createId.Value)
                let c = Mailbox()
                c.On<int> <| fun e -> 
                    //printfn "%d: %d" id.id e
                    if e < 1000 then
                        let nextId = rand.Next(1, 256)
                        c.Send(ActorId nextId, e + 1)
                Actor(c))
            a.Process(ActorId 1, 123)

        testCase "actor thread stress test" <| fun () ->
            let mutable count = 0L
            let log = MemoryActorStreamSource()
            let createRegistry() = 
                let r = MessageRegistry()
                r.Register 1 <| RawSerializer<MessageHeader>()
                r.Register 2 <| RawSerializer<int>()
                r.Register 3 <| RawSerializer<uint32>()                
                r.Register 4 <| RawSerializer<uint8>()                
                r.Register 5 <| RawSerializer<uint16>()
                r.Register 6 <| RawSerializer<int8>()
                r
            let start = DateTime.Now
            let config = { 
                Dispatchers = 
                    [|
                        {
                            Name = ""
                            DispatcherType = DispatcherType.Background
                            ThreadCount = 4
                            Throughput = 100
                        }                
                    |]
                }
            use a = new ActorSystem(4)
            let rules = 
                [
                ActorFactory.Create(ActorId 1, fun _ (c : Mailbox) ->
                    c.On<int> <| fun e ->
                        for i = 1 to 1000 do
                            c.Send(ActorId 2, uint32 i)
                            c.Send(ActorId 2, uint16 i))
                ActorFactory.Create(ActorId 2, fun _ (c : Mailbox) ->
                    c.On<int> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 3, uint32 i)
                            c.Send(ActorId 4, uint16 i)
                    c.On<uint32> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 3, uint8 i))
                ActorFactory.Create(ActorId 3, fun _ (c : Mailbox) ->
                    c.On<uint8> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 4, int8 i)
                    c.On<uint16> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 4, int8 i)
                    c.On<uint32> <| fun e ->
                        Interlocked.Increment(&count) |> ignore)
                ActorFactory.Create(ActorId 4, fun _ (c : Mailbox) ->
                    c.On<int8> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 2, int i)
                    c.On<uint16> <| fun e ->
                        Interlocked.Increment(&count) |> ignore)
                ]     
                //|> List.map (ActorFactory.withLogging createRegistry log.OpenWrite)
            a.Register(rules)
            for i = 1 to 100 do
                a.Process(ActorId 1, 0)
                Thread.Sleep(0)
            a.ProcessAll()
            //let duration = DateTime.Now - start
            // printfn "%s" <| a.ToString()
            // printfn "%A" <| duration
            //log.Print {
            //    createMessageRegistry = createRegistry
            //    createFormatter = Formatter
            //    print = ignore
            //    range = MessageRange.count 1000
            //    filter = ActorLogFilter.all
            //    }

        testCase "threaded actor logging" <| fun () ->
            let log = MemoryActorStreamSource()
            let createRegistry() = 
                let r = MessageRegistry()
                r.Register 1 <| RawSerializer<MessageHeader>()
                r.Register 2 <| RawSerializer<int>()
                r.Register 3 <| RawSerializer<uint32>()
                r.Register 4 <| RawSerializer<uint8>()
                r
            use a = new ActorSystem(4)
            let rules = 
                [
                ActorFactory.Create(ActorId 1, fun _ c ->
                    c.On<int> <| fun e ->
                        for i = 1 to 100 do
                            c.Send(ActorId 2, uint32 i))
                ActorFactory.Create(ActorId 2, fun _ c ->
                    c.On<uint32> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 3, uint8 i))
                ]     
                //|> List.map (ActorFactory.withLogging createRegistry log.OpenWrite)
            a.Register(rules)
            for i = 1 to 100 do
                a.Process(ActorId 1, 0)
                Thread.Sleep(0)
            a.ProcessAll()
            //log.Print {
            //    createMessageRegistry = createRegistry
            //    createFormatter = Formatter
            //    print = ignore
            //    range = MessageRange.count 1000
            //    filter = ActorLogFilter.all
            //    }

        testCase "serialize string" <| fun () ->
            let s = StringSerializer() :> ISerializer<_>
            let ms = new MemoryStream()
            let value = "test"
            s.Write ms value
            ms.Position <- 0L
            let str2 = s.Read ms 
            str2 |> shouldEqual value

//        testCase "log messages" <| fun () ->
//            let reg = MessageRegistry()
//            reg.Register 1 <| RawSerializer<MessageHeader>()
//            reg.Register 10 <| RawSerializer<int>()            
//            reg.Register 11 <| StringSerializer()
//            let ms = new MemoryStream()
//            use a = new ActorSystem(0)
//            let id1 = ActorId 15
//            let id2 = ActorId 25
//            a.Register [
//                ActorFactory.Create(id1, fun _ ->
//                    let h = Mailbox()
//                    h.On<int> <| fun e -> 
//                        h.Send(id2, "msg" + e.ToString())
//                    Actor(LogInbox(id1, h, StreamInbox(reg, ms))))
//                ]
//            a.Process(id1, 100)
//            ms.Position <- 0L
//            use a2 = new ActorSystem(0)
//            a2.Register <| ActorFactory.Create(fun id -> 
//                Actor(PrintInbox(id, Formatter(), ref 0, ignore)))
//            a2.Process(id1, { enableActorLog = true })
//            a2.Process(id2, { enableActorLog = true })
//            let sender = StreamMessageSender(reg, fun h -> true)
//            sender.SendAll(ms, a2)
//            a2.ProcessAll()
    ]