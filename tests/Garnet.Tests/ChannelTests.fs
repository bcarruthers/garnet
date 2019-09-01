module Garnet.Tests.Channels

open System.Collections.Generic
open Expecto
open Garnet.Composition

[<Tests>]
let tests =
    testList "channels" [
        testCase "send before subscribe" <| fun () ->
            let r = List<_>()
            let c = Channels()
            c.Send 1
            let sub = c.On<int> r.Add
            c.Commit()
            c.Publish() |> shouldEqual true
            r |> Seq.toList |> shouldEqual [ 1 ]
            
        testCase "publish before subscribe" <| fun () ->
            let r = List<_>()
            let c = Channels()
            c.Publish 1
            let sub = c.On<int> r.Add
            c.Commit()
            c.Publish() |> shouldEqual false
            r |> Seq.toList |> shouldEqual []

        testCase "publish" <| fun () ->
            let r = List<_>()
            let c = Channels()
            let sub = c.On<int> r.Add
            for i = 1 to 10 do
                c.Publish i
            r |> Seq.toList |> shouldEqual [ 1..10 ]
        
        testCase "reentrant publish" <| fun () ->
            let r = List<_>()
            let c = Channels()
            let sub =
                c.On<int> <| fun e ->
                    r.Add e
                    if e < 10 then
                        c.Publish (e + 1)
            c.Publish 1
            r |> Seq.toList |> shouldEqual [ 1..10 ]
        
        testCase "send" <| fun () ->
            let r = List<_>()
            let c = Channels()
            let sub =
                c.On<int> <| fun e ->
                    r.Add e
                    if e < 10 then
                        c.Send (e + 1)
            c.Publish 1
            c.Commit()
            while c.Publish() do
                c.Commit()
            r |> Seq.toList |> shouldEqual [ 1..10 ]

        testCase "publish then send" <| fun () ->
            let r = List<_>()
            let c = Channels()
            let sub =
                c.On<int> <| fun e ->
                    r.Add e
                    if e < 10 then
                        if e % 2 = 0 then c.Publish (e + 1)
                        else c.Send (e + 1)
            c.Send 1
            c.Commit()
            while c.Publish() do
                c.Commit()
            r |> Seq.toList |> shouldEqual [ 1..10 ]

        testCase "mixed send and publish" <| fun () ->
            let r = List<_>()
            let c = Channels()
            let sub =
                c.On<int> <| fun e ->
                    r.Add e
                    if e < 10 then
                        if e % 2 = 0 then c.Publish (e + 1)
                        else c.Send (e + 1)
            c.Publish 1
            c.Commit()
            while c.Publish() do
                c.Commit()
            r |> Seq.toList |> shouldEqual [ 1..10 ]
            
        testCase "set publisher" <| fun () ->
            let r = List<_>()
            let c = Channels()
            let sub = c.On<int> r.Add
            c.Send 1
            c.Commit()
            c.Publish() |> shouldEqual true
            r |> Seq.toList |> shouldEqual [ 1 ]
            c.SetPublisher (ValueSome Publisher.Null)
            c.Send 2
            c.Commit()
            c.Publish() |> shouldEqual true
            r |> Seq.toList |> shouldEqual [ 1 ]
            let r = List<_>()
            let sub = c.On<string> r.Add
            c.Send "a"
            c.Commit()
            c.Publish() |> shouldEqual true
            r |> Seq.toList |> shouldEqual []
            c.SetPublisher (ValueSome Publisher.Default)
            c.Send "a"
            c.Commit()
            c.Publish() |> shouldEqual true
            r |> Seq.toList |> shouldEqual [ "a" ]

        testCase "log publisher" <| fun () ->
            let r = List<_>()
            let c = Container()
            let sub =
                c.On<int> <| fun e -> 
                    if e = 2 then failwith "error"
                    c.Send<string> "a"
                    c.Publish 'b'
            c.SetPublisher(Publisher.Print {
                PrintPublisherOptions.enabled with
                    sendLog = r.Add
                    })
            c.Run 1
            try c.Run 2 with ex -> r.Add(sprintf "%s" <| ex.ToString())
            c.Run "c"
            r.Count |> shouldEqual 6
    ]