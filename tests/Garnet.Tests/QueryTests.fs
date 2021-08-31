module Garnet.Tests.Query

open System
open System.Collections.Generic
open Expecto
open Garnet.Composition

[<Tests>]
let tests =
    let testMaskIteration mask =
        let mutable r = 0UL
        let mutable last = -1
        for i in MaskEnumerable(mask) do
            if i <= last || i < 0 || i >= 64 then failwith $"Invalid index {i}"
            r <- r ||| (1UL <<< i)
            last <- i
        r |> shouldEqual mask
    
    testList "query" [
        testCase "iterate over mask" <| fun () ->
            testMaskIteration 0UL
            testMaskIteration UInt64.MaxValue
            testMaskIteration 0x10UL
            let rand = Random(1)
            for i = 1 to 1000 do
                let x = 
                    (uint64 (rand.Next()) <<< 32) |||
                    (uint64 (rand.Next()))
                testMaskIteration x
        
        testCase "iter2" <| fun () ->
            let c = ComponentStore<_,_,EidSegmentKeyMapper>()
            for i = 1 to 100 do
                let e = c.Get(Eid i)
                if i % 2 = 0 then e.Add(i)
                if i % 3 = 0 then e.Add('a' + char i)
                if i % 5 = 0 then e.Add(i.ToString())
            c.Commit()
            let r = List<_>()
            for row in c.Query<int, string>() do
                r.Add(row.Values)
            r.Count |> shouldEqual 10
            let r = List<_>()
            for row in c.Query<int, char>() do
                r.Add(row.Values)
            r.Count |> shouldEqual 16
    ]