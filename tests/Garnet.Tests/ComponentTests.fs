module Garnet.Tests.Components

open System
open System.Collections.Generic
open Expecto
open Garnet.Composition

[<Tests>]
let tests =
    let create() = Components(Eid.eidToComponentKey)
    testList "components" [
        testCase "get component" <| fun () ->
            let s = create()
            s.Add(Eid 1, 10)
            s.Commit()
            s.Get(Eid 1, 10) |> shouldEqual 10

        testCase "get component without commit" <| fun () ->
            let s = create()
            s.Add(Eid 1, 10)
            Expect.throws (fun () -> s.Get(Eid 1) |> ignore) "exception expected"

        testCase "get component or default" <| fun () ->
            let s = create()
            s.Get(Eid 1, 10) |> shouldEqual 10

        testCase "add component multiple times" <| fun () ->
            let s = create()
            s.Add(Eid 1, 10)
            s.Commit()
            s.Get(Eid 1) |> shouldEqual 10
            s.Add(Eid 1, 11)
            s.Commit()
            s.Get(Eid 1) |> shouldEqual 11

        testCase "add component multiple times before commit" <| fun () ->
            let s = create()
            s.Add(Eid 1, 10)
            s.Add(Eid 1, 11)
            s.Commit()
            s.Get(Eid 1) |> shouldEqual 11

        testCase "add and remove component before commit" <| fun () ->
            let s = create()
            s.Add(Eid 1, 10)
            s.Remove(Eid 1)
            s.Commit()
            s.Contains(Eid 1) |> shouldEqual false

        testCase "remove and add component before commit" <| fun () ->
            let s = create()
            s.Remove(Eid 1)
            s.Add(Eid 1, 10)
            s.Commit()
            s.Get(Eid 1) |> shouldEqual 10

        testCase "remove component not present" <| fun () ->
            let s = create()
            s.Remove(Eid 1)
            s.Commit()
            
        testCase "adding is deferred" <| fun () ->
            let s = create()
            s.Add(Eid 1, 10)
            s.Count |> shouldEqual 0
            s.Contains(Eid 1) |> shouldEqual false
            s.Get(Eid 1, 0) |> shouldEqual 0
            s.Commit()
            s.Count |> shouldEqual 1
            s.Contains(Eid 1) |> shouldEqual true
            s.Get(Eid 1, 0) |> shouldEqual 10
                
        testCase "removal is deferred" <| fun () ->
            let s = create()
            s.Add(Eid 1, 10)
            s.Commit()
            s.Remove(Eid 1)
            s.Count |> shouldEqual 1
            s.Contains(Eid 1) |> shouldEqual true
            s.Get(Eid 1, 0) |> shouldEqual 10
            s.Commit()
            s.Count |> shouldEqual 0
            s.Contains(Eid 1) |> shouldEqual false
                
        testCase "setting is immediate" <| fun () ->
            let s = create()
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
            let s = create()
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