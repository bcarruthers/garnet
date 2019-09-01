module Garnet.Tests.Entities

open System
open System.Collections.Generic
open Garnet.Composition
open Expecto

type Velocity = {
    velocity : int }

type Position = {
    position : int }

[<Tests>]
let tests =
    let getComponents (c : Segments<_,_>) = seq {
        for i = 0 to c.Count - 1 do
            let seg = c.[i]
            let mutable m = seg.mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then 
                    yield seg.data.[i]
                m <- m >>> 1
                i <- i + 1
        }            

    testList "entities" [
        testCase "increment ID version" <| fun () ->
            Eid.fromParts 1 0 0 |> Eid.getGen |> shouldEqual 1
            Eid.incrementGen (Eid 100) |> shouldEqual (Eid.fromParts 1 0 100)

        testCase "incrementing ID version wraps" <| fun () ->
            Eid.fromParts Eid.maxGen 0 100 
            |> Eid.incrementGen 
            |> shouldEqual (Eid.fromParts 0 0 100)

        testCase "create IDs from pool" <| fun () ->
            let p = EidPools()
            [ for i = 1 to 100 do yield p.Next(0).Index ]
            |> shouldEqual [ 64..163 ]

        testCase "create IDs from partition" <| fun () ->
            let p = EidPool(5)
            p.Next().value |> shouldEqual 0x500040
            p.SegmentCount |> shouldEqual 2

        testCase "create and destroy before commit" <| fun () ->
            let c = Container()
            let e = c.Create()
            e.Destroy()
            c.Commit()
            c.Get<Eid>().Count |> shouldEqual 0

        testCase "create and destroy before commit with pooled" <| fun () ->
            let c = Container()
            let e1 = c.Create()
            c.Commit()
            let e2 = c.Create()
            e2.Destroy()
            c.Commit()
            c.Get<Eid>().Count |> shouldEqual 1

        testCase "recycle ID to pool" <| fun () ->
            let segmentSize = 64
            let c = Container()
            let usedIds = HashSet<Eid>()
            for j = 1 to 100 do
                for i = 1 to segmentSize do
                    let eid = c.Create().id
                    eid.Gen |> shouldEqual (j - 1)
                    usedIds.Add(eid) |> shouldEqual true
                    c.Destroy eid
                c.Commit()

        testCase "create entity" <| fun () ->
            let c = Container()
            let e = c.Create()
            e.Add { velocity = 3 }
            e.Add { position = 5 }
            c.GetSegments<Eid>().Count |> shouldEqual 0
            c.Commit()
            c.GetSegments<Eid>().Count |> shouldEqual 1            
            c.GetSegments<Velocity>().Count |> shouldEqual 1
            
        testCase "remove entity" <| fun () ->
            let c = Container()
            let e = c.Create()
            e.Add { velocity = 3 }
            c.Commit()
            e.Destroy()
            c.GetSegments<Eid>().Count |> shouldEqual 1
            c.GetSegments<Velocity>().Count |> shouldEqual 1
            c.Commit()
            c.GetSegments<Eid>().Count |> shouldEqual 0
            c.GetSegments<Velocity>().Count |> shouldEqual 0

        testCase "add and remove simultaneously" <| fun () ->
            let c = Container()
            // Eid and component go into additions
            let e = c.Create()
            e.Add 123
            // Eid goes into removal
            e.Destroy()
            c.Commit()
            // so components are present
            c.GetSegments<Eid>().Count |> shouldEqual 0

        testCase "add and remove sequentially" <| fun () ->
            let c = Container()
            // Eid and component go into additions
            let e = c.Create()
            e.Add 123
            // additions applied
            c.Commit()
            c.GetSegments<Eid>().Count |> shouldEqual 1
            // Eid goes into removal
            e.Destroy()
            // removals applied
            c.Commit()
            // so components are not present
            c.GetSegments<Eid>().Count |> shouldEqual 0

        testCase "add and remove entities randomly" <| fun () ->
            let iterations = 10000
            let maxBatchSize = 1000
            let print = false
            let rand = Random(1)
            let eids = HashSet<Eid>()
            let active = List<Eid>()
            let c = Container()
            for i = 1 to iterations do
                if print then printfn "Iteration %d" i
                let batchSize = min i maxBatchSize
                match rand.Next() % 3 with
                | 0 ->
                    // create batch
                    let count = rand.Next(batchSize)
                    for i = 1 to count do
                        let e = c.Create(rand.Next 16)
                        if print then printfn "Create %A" e.id
                        if e.id.Index < 64 then
                            failwithf "Invalid value: %A" e.id
                        if not (eids.Add e.id) then
                            failwithf "Duplicate: %A" e.id
                        active.Add e.id
                | 1 ->
                    // destroy batch
                    let count = rand.Next(min batchSize active.Count)
                    for i = 1 to count do
                        let ri = rand.Next(active.Count)
                        let eid = active.[ri]
                        active.[ri] <- active.[active.Count - 1]
                        active.RemoveAt(active.Count - 1)
                        c.Destroy(eid)
                        if print then printfn "Destroy %A" eid
                | _ -> 
                    // commit and validate
                    c.Commit()
                    let eidStore = c.Get<Eid>()
                    if print then 
                        printfn "Commit %s" <| c.ToString()
                        getComponents eidStore.Segments
                        |> Seq.toArray
                        |> printfn "Active: %A"
                    eidStore.Count |> shouldEqual active.Count
            c.Commit()
    ]