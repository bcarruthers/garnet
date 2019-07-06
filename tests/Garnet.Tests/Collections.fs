module Garnet.Tests.Collections

open System
open System.Collections.Generic
open Expecto
open Garnet.Collections
          
[<Tests>]
let tests =
    testList "collections" [
        testCase "enqueue to ring buffer" <| fun () ->
            let r = RingBuffer(2)
            r.TryEnqueue 1 |> shouldEqual true
            r.TryEnqueue 2 |> shouldEqual true
            r.TryEnqueue 3 |> shouldEqual false
            // dequeue
            let x = ref 0
            r.TryDequeue x |> shouldEqual true
            x.Value |> shouldEqual 1
            r.TryDequeue x |> shouldEqual true
            x.Value |> shouldEqual 2
            r.TryDequeue x |> shouldEqual false
            // enqueue
            r.TryEnqueue 3 |> shouldEqual true
            r.TryDequeue x |> shouldEqual true
            x.Value |> shouldEqual 3

        testCase "enqueue to ring buffer node" <| fun () ->
            let n = RingBufferNode(2)
            n.Enqueue 1 |> shouldEqual n
            n.Enqueue 2 |> shouldEqual n
            let n2 = n.Enqueue 3 
            n2 |> shouldNotEqual n
            n2.Enqueue 4 |> shouldEqual n2
            let x = ref 0
            n.TryDequeue x |> shouldEqual n
            x.Value |> shouldEqual 1
            n.TryDequeue x |> shouldEqual n
            x.Value |> shouldEqual 2
            n2.TryDequeue x |> shouldEqual n2
            x.Value |> shouldEqual 3
            n2.TryDequeue x |> shouldEqual n2
            x.Value |> shouldEqual 4
            n2.TryDequeue x |> shouldEqual null

        testCase "enqueue to ring buffer queue" <| fun () ->
            let items = List<int>()
            let q = RingBufferQueue(2)
            [ 1..40 ] |> List.iter q.Enqueue
            q.DequeueAll (Action<_>(items.Add))
            Expect.sequenceEqual items [ 1..40 ] ""
    ]