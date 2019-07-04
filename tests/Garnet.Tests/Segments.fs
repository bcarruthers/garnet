module Garnet.Tests.Segments

open System
open System.Collections.Generic
open Expecto
open Garnet.Ecs

[<AutoOpen>]
module Assertions =
    let shouldEqual b a =
        Expect.equal a b ""

    let shouldNotEqual b a =
        Expect.notEqual a b ""

[<Tests>]
let tests =
    testList "segments" [
        testCase "data cleared before added back to pool" <| fun () ->
            let s = Segments<int, int>()
            let data = s.AddMask(1, UInt64.MaxValue)
            Array.fill data 0 data.Length 1
            (s :> ISegments<int>).Commit()
            let data = s.AddMask(2, 0UL)
            data.[0] |> shouldEqual 0

        testCase "adding is deferred" <| fun () ->
            let s = Components.create()
            s.Add(Eid 1, 10)
            s.ComponentCount |> shouldEqual 0
            s.Contains(Eid 1) |> shouldEqual false
            s.GetOrDefault(Eid 1, 0) |> shouldEqual 0
            s.Commit()
            s.ComponentCount |> shouldEqual 1
            s.Contains(Eid 1) |> shouldEqual true
            s.GetOrDefault(Eid 1, 0) |> shouldEqual 10
                
        testCase "removal is deferred" <| fun () ->
            let s = Components.create()
            s.Add(Eid 1, 10)
            s.Commit()
            s.Remove(Eid 1)
            s.ComponentCount |> shouldEqual 1
            s.Contains(Eid 1) |> shouldEqual true
            s.GetOrDefault(Eid 1, 0) |> shouldEqual 10
            s.Commit()
            s.ComponentCount |> shouldEqual 0
            s.Contains(Eid 1) |> shouldEqual false
                
        testCase "setting is immediate" <| fun () ->
            let s = Components.create()
            s.Add(Eid 1, 10)
            s.Commit()
            s.Set(Eid 1, 11)
            s.Get(Eid 1) |> shouldEqual 11
                
        testCase "random adding and removing" <| fun () ->
            let remove (list : List<_>) index =
                let eid = list.[index]
                list.[index] <- list.[list.Count - 1]
                list.RemoveAt(list.Count - 1)
                eid                    
            let s = Components.create()
            let rand = Random(1)
            let added = List<Eid>()
            let removed = List<Eid>()
            let meanCount = 1000
            for i = 1 to 60 do
                for i = 1 to 1000 do
                    // half probability of add/removing
                    let index = rand.Next(meanCount * 2)
                    if index < added.Count then
                        // remove component
                        let eid = remove added index
                        removed.Add(eid)
                        s.Remove(eid)
                    else
                        // add component
                        let index = index - added.Count
                        let eid =
                            if index < removed.Count 
                                then remove removed index 
                                else Eid (added.Count + removed.Count)
                        added.Add(eid)
                        s.Add(eid, eid.value)
                    s.Commit()
            for eid in added do
                s.Contains(eid) |> shouldEqual true
                s.Get(eid) |> shouldEqual eid.value
            for eid in removed do
                s.Contains(eid) |> shouldEqual false
    ]