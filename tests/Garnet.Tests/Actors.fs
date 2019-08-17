module Garnet.Tests.Actors

open System
open System.Collections.Generic
open System.IO
open System.Threading
open Expecto
open Garnet
open Garnet.Formatting
open Garnet.Composition
open Garnet.Streaming

type Run = struct end
type Ping = struct end
type Pong = struct end

module ActorFactory =
    let main actorId register = 
        ActorFactory.handler actorId register 
        |> ActorFactory.map Actor.execMain

type Inbox() =
    let dict = Dictionary<Type, obj>()
    member c.OnAll<'a>(action : Envelope<Buffer<'a>> -> unit) =
        let t = typeof<'a>
        let combined =
            match dict.TryGetValue t with
            | false, _ -> action
            | true, existing -> 
                let existing = existing :?> (Envelope<Buffer<'a>> -> unit)
                fun e -> 
                    existing e
                    action e        
        dict.[t] <- combined
    member c.TryReceive<'a> e =
        match dict.TryGetValue(typeof<'a>) with
        | true, x -> 
            let handle = x :?> (Envelope<Buffer<'a>> -> unit)
            handle e
            true
        | false, _ -> false
    interface IInbox with
        member c.Receive e =
            c.TryReceive e |> ignore

let runPingPong onPing onPong iterations =
    let mutable count = 0
    use a = new ActorSystem(0)
    a.Register(ActorId 1, fun h ->
        h.On<Run> <| fun e ->
            h.Send(ActorId 2, Ping())
        h.On<Pong> <| fun e ->
            count <- count + 1
            if count < iterations then
                onPing(e)
                h.Respond(Ping())
        )
    a.Register(ActorId 2, fun h -> 
        h.On<Ping> <| fun e -> 
            onPong e
            h.Respond(Pong())
        )
    a.Run(ActorId 1, Run())
    a.RunAll()
    count

let mapEnvelope f (mail : Envelope<_>) = {
    outbox = mail.outbox
    sourceId = mail.sourceId
    destinationId = mail.destinationId
    message = f mail.message
    }

let sendReceiveMessages send =
    let results = List<_>()
    use a = new ActorSystem(0)
    let h = Inbox()
    h.OnAll<int> <| fun e ->
        results.Add(e |> mapEnvelope List.ofSeq)
    a.Register(ActorId 1, h)
    send (a.Get(ActorId 1))
    a.RunAll()
    results |> List.ofSeq
    
[<Tests>]
let tests =
    testList "actors" [            
        testCase "send to undefined actor" <| fun () ->
            use a = new ActorSystem()
            a.Send(ActorId 1, 10)
            a.RunAll()

        testCase "send batch" <| fun () ->
            let results = sendReceiveMessages <| fun a ->
                a.SendAll(Buffer.ofSeq [ 1; 2; 3 ])                
            let r = List.head results
            r.sourceId |> shouldEqual (ActorId 0)
            r.destinationId |> shouldEqual (ActorId 1)
            r.message |> shouldEqual [ 1; 2; 3 ]

        testCase "send single" <| fun () ->
            let results = sendReceiveMessages <| fun a ->
                a.Send(1)                
            let r = List.head results
            r.sourceId |> shouldEqual (ActorId 0)
            r.destinationId |> shouldEqual (ActorId 1)
            r.message |> shouldEqual [ 1 ]

        testCase "send single with source" <| fun () ->
            let results = sendReceiveMessages <| fun a ->
                a.Send(1, sourceId = ActorId 2)                
            let r = List.head results
            r.sourceId |> shouldEqual (ActorId 2)
            r.destinationId |> shouldEqual (ActorId 1)
            r.message |> shouldEqual [ 1 ]

        testCase "send to any actor" <| fun () ->
            let msgs = List<_>()
            let inbox = Mailbox()
            inbox.On<int> msgs.Add
            use a = new ActorSystem()
            a.Register(ActorFactory.any (fun id -> Actor.inbox inbox))
            a.Send(ActorId 1, 10)
            a.RunAll()
            msgs.Count |> shouldEqual 1

        testCase "get routed actor" <| fun () ->
            let route = ActorFactory.route (fun id -> ActorId (id.value + 1))
            let actor = route(ActorId 1)
            actor.routedId |> shouldEqual (ActorId 2)
            actor.execution |> shouldEqual Execution.Route

        testCase "route to self" <| fun () ->
            use a = new ActorSystem()
            a.Register (ActorFactory.route id)
            a.Send(ActorId 1, 10)
            a.RunAll()

        testCase "get combined factory with route" <| fun () ->
            let list1 = List<_>()
            let list2 = List<_>()
            let create = 
                ActorFactory.combine [
                    ActorFactory.handler (ActorId 5) (fun h -> h.On<int> list1.Add)
                    ActorFactory.route (fun id -> ActorId (id.value + 1))
                    ActorFactory.handler (ActorId 6) (fun h -> h.On<int> list2.Add)
                ]
            let actor = create(ActorId 4)
            actor.execution |> shouldEqual Execution.Route
            actor.routedId |> shouldEqual (ActorId 5)
            let actor = create(ActorId 5)
            actor.execution |> shouldEqual Execution.Default
            actor.routedId |> shouldEqual ActorId.undefined
            use a = new ActorSystem()
            a.Register create
            a.Send(ActorId 4, 10)
            a.Send(ActorId 5, 10)
            a.Send(ActorId 6, 10)
            a.Send(ActorId 7, 10)
            a.RunAll()
            list1.Count |> shouldEqual 2
            list2.Count |> shouldEqual 1
            
        testCase "create ping pong actors" <| fun () ->
            let iterations = 10
            runPingPong ignore ignore iterations |> shouldEqual iterations

        testCase "send message to self" <| fun () ->
            let mutable count = 0
            use a = new ActorSystem(0)
            a.RegisterAll [
                ActorFactory.main (ActorId 1) <| fun c ->
                    c.On<int> <| fun e ->
                        if e < 10 then
                            count <- count + 1
                            c.Send(ActorId 1, e + 1)
                ]
            a.Run(ActorId 1, 0)
            count |> shouldEqual 10

        testCase "send message to other" <| fun () ->
            let mutable count1 = 0
            let mutable count2 = 0
            use a = new ActorSystem(0)
            a.RegisterAll [
                ActorFactory.handler (ActorId 1) <| fun c ->
                    c.On<int> <| fun e ->
                        if e < 10 then
                            count1 <- count1 + 1
                            c.Send(ActorId 2, e + 1)
                ActorFactory.handler (ActorId 2) <| fun c ->
                    c.On<int> <| fun e ->
                        if e < 10 then
                            count2 <- count2 + 1
                            c.Send(ActorId 1, e + 1)
                ]
            a.Run(ActorId 1, 0)
            count1 |> shouldEqual 5
            count2 |> shouldEqual 5

        testCase "send message to background actor" <| fun () ->
            let mutable count1 = 0
            let mutable count2 = 0
            use a = new ActorSystem(1)
            a.RegisterAll [
                ActorFactory.main (ActorId 1) <| fun c ->
                    c.On<Thread> <| fun e ->
                        // bg thread should be different
                        Expect.notEqual Thread.CurrentThread.ManagedThreadId e.ManagedThreadId ""
                    c.On<int> <| fun e ->
                        if e < 10 then
                            count1 <- count1 + 1
                            //printfn "FG: %d" Thread.CurrentThread.ManagedThreadId
                            c.Send(ActorId 2, e + 1)
                ActorFactory.handler (ActorId 2) <| fun c ->
                    c.On<int> <| fun e ->
                        if e < 10 then
                            count2 <- count2 + 1
                            //printfn "BG: %d" Thread.CurrentThread.ManagedThreadId
                            c.Send(ActorId 1, Thread.CurrentThread)
                            c.Send(ActorId 1, e + 1)
                ]            
            a.Run(ActorId 1, 0)
            a.RunAll()
            count1 |> shouldEqual 5
            count2 |> shouldEqual 5

        testCase "send random messages to background actors" <| fun () ->
            use a = new ActorSystem(2)
            a.RegisterAll [
                //ActorId.consoleOut, fun id -> PrintMessageHandler.Handler 
                ActorFactory.filterHandler (fun id -> true) <| fun id c -> 
                    let rand = Random(id.value)
                    c.On<int> <| fun e -> 
                        //printfn "%d: %d" id.id e
                        if e < 1000 then
                            let nextId = rand.Next(1, 256)
                            c.Send(ActorId nextId, e + 1)
                ]
            a.Run(ActorId 1, 123)

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
            use a = new ActorSystem(4)
            let rules = 
                [
                ActorFactory.handler (ActorId 1) <| fun c ->
                    c.On<int> <| fun e ->
                        for i = 1 to 1000 do
                            c.Send(ActorId 2, uint32 i)
                            c.Send(ActorId 2, uint16 i)
                ActorFactory.handler (ActorId 2) <| fun c ->
                    c.On<int> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 3, uint32 i)
                            c.Send(ActorId 4, uint16 i)
                    c.On<uint32> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 3, uint8 i)
                ActorFactory.handler (ActorId 3) <| fun c ->
                    c.On<uint8> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 4, int8 i)
                    c.On<uint16> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 4, int8 i)
                    c.On<uint32> <| fun e ->
                        Interlocked.Increment(&count) |> ignore
                ActorFactory.handler (ActorId 4) <| fun c ->
                    c.On<int8> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 2, int i)
                    c.On<uint16> <| fun e ->
                        Interlocked.Increment(&count) |> ignore
                ]     
                |> List.map (ActorFactory.withLogging createRegistry log.OpenWrite)
            a.RegisterAll rules
            for i = 1 to 100 do
                a.Run(ActorId 1, 0)
                Thread.Sleep(0)
            a.RunAll()
            let duration = DateTime.Now - start
            // printfn "%s" <| a.ToString()
            // printfn "%A" <| duration
            log.Print {
                createMessageRegistry = createRegistry
                createFormatter = Formatter
                print = ignore
                range = MessageRange.count 1000
                filter = ActorLogFilter.all
                }

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
                ActorFactory.handler (ActorId 1) <| fun c ->
                    c.On<int> <| fun e ->
                        for i = 1 to 100 do
                            c.Send(ActorId 2, uint32 i)
                ActorFactory.handler (ActorId 2) <| fun c ->
                    c.On<uint32> <| fun e ->
                        for i = 1 to 1 do
                            c.Send(ActorId 3, uint8 i)
                ]     
                |> List.map (ActorFactory.withLogging createRegistry log.OpenWrite)
            a.RegisterAll rules
            for i = 1 to 100 do
                a.Run(ActorId 1, 0)
                Thread.Sleep(0)
            a.RunAll()
            log.Print {
                createMessageRegistry = createRegistry
                createFormatter = Formatter
                print = ignore
                range = MessageRange.count 1000
                filter = ActorLogFilter.all
                }

        testCase "serialize string" <| fun () ->
            let s = StringSerializer() :> ISerializer<_>
            let ms = new MemoryStream()
            let value = "test"
            s.Write ms value
            ms.Position <- 0L
            let str2 = s.Read ms 
            str2 |> shouldEqual value

        testCase "log messages" <| fun () ->
            let reg = MessageRegistry()
            reg.Register 1 <| RawSerializer<MessageHeader>()
            reg.Register 10 <| RawSerializer<int>()            
            reg.Register 11 <| StringSerializer()
            let ms = new MemoryStream()
            use a = new ActorSystem(0)
            let id1 = ActorId 15
            let id2 = ActorId 25
            a.RegisterAll [
                ActorFactory.init id1 <| fun () ->
                    let h = Mailbox()
                    h.On<int> <| fun e -> 
                        h.Send(id2, "msg" + e.ToString())
                    Actor.inbox (LogInbox(id1, h, StreamInbox(reg, ms)))
                ]
            a.Run(id1, 100)
            ms.Position <- 0L
            use a2 = new ActorSystem(0)
            a2.Register <| ActorFactory.any (fun id -> 
                PrintInbox(id, Formatter(), ref 0, ignore) |> Actor.inbox)
            a2.Run(id1, { enableActorLog = true })
            a2.Run(id2, { enableActorLog = true })
            let sender = StreamMessageSender(reg, fun h -> true)
            sender.SendAll(ms, a2)
            a2.RunAll()
    ]