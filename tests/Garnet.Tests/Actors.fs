module Garnet.Tests.Actors

open System
open System.Collections.Generic
open System.IO
open System.Threading
open Expecto
open Garnet.Formatting
open Garnet.Actors

type Ping = struct end
type Pong = struct end

module ActorFactory =
    let single bg actorId register = 
        ActorFactory.init bg ((=)actorId) (fun id -> ActorDefinition.handler register)
        
    let fg actorId register = single false actorId register
    let bg actorId register = single true actorId register
    
[<Tests>]
let tests =
    testList "actors" [            
        testCase "create ping pong actors" <| fun () ->
            let msgs = List<obj>()
            use a = new ActorSystem()
            a.Register(ActorId 1, fun c ->
                c.On<Ping> <| fun e -> 
                    msgs.Add e.message
                    e.Respond(Pong())
                )
            a.Register(ActorId 2, fun c ->
                c.On<Pong> <| fun e -> 
                    msgs.Add e.message
                )
            a.Send(ActorId 1, Ping(), sourceId = ActorId 2)
            a.RunAll()
            msgs.Count |> shouldEqual 2

        testCase "send message to self" <| fun () ->
            let mutable count = 0
            use a = new ActorSystem(0)
            a.RegisterAll [
                ActorFactory.fg (ActorId 1) <| fun c ->
                    c.On<int> <| fun e ->
                        if e.message < 10 then
                            count <- count + 1
                            e.outbox.Send(ActorId 1, e.message + 1)
                ]
            a.Run(ActorId 1, 0)
            count |> shouldEqual 10
            a.RunOnce() |> shouldEqual false

        testCase "send message to other" <| fun () ->
            let mutable count1 = 0
            let mutable count2 = 0
            use a = new ActorSystem(0)
            a.RegisterAll [
                ActorFactory.fg (ActorId 1) <| fun c ->
                    c.On<int> <| fun e ->
                        if e.message < 10 then
                            count1 <- count1 + 1
                            e.outbox.Send(ActorId 2, e.message + 1)
                ActorFactory.fg (ActorId 2) <| fun c ->
                    c.On<int> <| fun e ->
                        if e.message < 10 then
                            count2 <- count2 + 1
                            e.outbox.Send(ActorId 1, e.message + 1)
                ]
            a.Run(ActorId 1, 0)
            count1 |> shouldEqual 5
            count2 |> shouldEqual 5
            a.RunOnce() |> shouldEqual false

        testCase "send message to background actor" <| fun () ->
            let mutable count1 = 0
            let mutable count2 = 0
            use a = new ActorSystem(1)
            a.RegisterAll [
                ActorFactory.fg (ActorId 1) <| fun c ->
                    c.On<Thread> <| fun e ->
                        // bg thread should be different
                        Expect.notEqual Thread.CurrentThread.ManagedThreadId e.message.ManagedThreadId ""
                    c.On<int> <| fun e ->
                        if e.message < 10 then
                            count1 <- count1 + 1
                            //printfn "FG: %d" Thread.CurrentThread.ManagedThreadId
                            e.outbox.Send(ActorId 2, e.message + 1)
                ActorFactory.bg (ActorId 2) <| fun c ->
                    c.On<int> <| fun e ->
                        if e.message < 10 then
                            count2 <- count2 + 1
                            //printfn "BG: %d" Thread.CurrentThread.ManagedThreadId
                            e.outbox.Send(ActorId 1, Thread.CurrentThread)
                            e.outbox.Send(ActorId 1, e.message + 1)
                ]            
            a.Run(ActorId 1, 0)
            a.RunAll()
            count1 |> shouldEqual 5
            count2 |> shouldEqual 5
            a.RunOnce() |> shouldEqual false

        testCase "send random messages to background actors" <| fun () ->
            use a = new ActorSystem(2)
            a.RegisterAll [
                //ActorId.consoleOut, fun id -> PrintMessageHandler.Handler 
                ActorFactory { 
                    isBackground = false
                    canCreate = fun id -> true
                    create = ActorDefinition.handlerId <| fun id c -> 
                        let rand = Random(id.value)
                        c.On<int> <| fun e -> 
                            //printfn "%d: %d" id.id e.message
                            if e.message < 1000 then
                                let nextId = rand.Next(1, 256)
                                e.outbox.Send(ActorId nextId, e.message + 1)
                }]
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
                ActorFactory.bg (ActorId 1) <| fun c ->
                    c.On<int> <| fun e ->
                        for i = 1 to 1000 do
                            e.outbox.Send(ActorId 2, uint32 i)
                            e.outbox.Send(ActorId 2, uint16 i)
                ActorFactory.bg (ActorId 2) <| fun c ->
                    c.On<int> <| fun e ->
                        for i = 1 to 1 do
                            e.outbox.Send(ActorId 3, uint32 i)
                            e.outbox.Send(ActorId 4, uint16 i)
                    c.On<uint32> <| fun e ->
                        for i = 1 to 1 do
                            e.outbox.Send(ActorId 3, uint8 i)
                ActorFactory.bg (ActorId 3) <| fun c ->
                    c.On<uint8> <| fun e ->
                        for i = 1 to 1 do
                            e.outbox.Send(ActorId 4, int8 i)
                    c.On<uint16> <| fun e ->
                        for i = 1 to 1 do
                            e.outbox.Send(ActorId 4, int8 i)
                    c.On<uint32> <| fun e ->
                        Interlocked.Increment(&count) |> ignore
                ActorFactory.bg (ActorId 4) <| fun c ->
                    c.On<int8> <| fun e ->
                        for i = 1 to 1 do
                            e.outbox.Send(ActorId 2, int i)
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
                ActorFactory.bg (ActorId 1) <| fun c ->
                    c.On<int> <| fun e ->
                        for i = 1 to 100 do
                            e.outbox.Send(ActorId 2, uint32 i)
                ActorFactory.bg (ActorId 2) <| fun c ->
                    c.On<uint32> <| fun e ->
                        for i = 1 to 1 do
                            e.outbox.Send(ActorId 3, uint8 i)
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
            a.Register <| ActorFactory {
                isBackground = false
                canCreate = fun id -> id = ActorId 15
                create = fun id -> 
                    let h = MessageHandler()
                    h.On<int> <| fun e -> e.outbox.Send(ActorId 25, "msg" + e.message.ToString())
                    LogMessageHandler(id, h, StreamMessageHandler(reg, ms)) |> ActorDefinition.init
                }
            a.Run(ActorId 15, 100)
            ms.Position <- 0L
            use a2 = new ActorSystem(0)
            a2.Register <| ActorFactory {
                isBackground = false
                canCreate = fun id -> true
                create = fun id -> PrintMessageHandler(id, Formatter(), ref 0, ignore) |> ActorDefinition.init
                }
            a2.Run(ActorId 15, { enableActorLog = true })
            a2.Run(ActorId 25, { enableActorLog = true })
            let sender = StreamMessageSender(reg, fun h -> true)
            sender.SendAll(ms, a2)
            a2.RunAll()
    ]