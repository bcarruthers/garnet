module Garnet.Tests.Segments

open System
open Expecto
open Garnet.Composition

[<Tests>]
let tests =
    testList "segments" [
        testCase "add" <| fun () ->
            let s = Segments()
            s.Add(1, 0b111000UL).[5] <- 10
            s.Count |> shouldEqual 0
            s.Commit()
            s.Count |> shouldEqual 1
            s.[0].id |> shouldEqual 1
            s.[0].mask |> shouldEqual 0b111000UL
            s.[0].data.[5] |> shouldEqual 10

        testCase "remove" <| fun () ->
            let s = Segments()
            s.Add(1, 0b111000UL).[4] <- 10
            s.Commit()
            s.Remove(1, 0b101000UL)
            s.[0].mask |> shouldEqual 0b111000UL
            s.Commit()
            s.Count |> shouldEqual 1
            s.[0].mask |> shouldEqual 0b010000UL
            s.[0].data.[4] |> shouldEqual 10

        testCase "clear immediately" <| fun () ->
            let s = Segments<int, int>()
            s.Add(1, UInt64.MaxValue) |> ignore
            s.Commit()
            s.Count |> shouldEqual 1
            s.Clear()
            s.Count |> shouldEqual 0

        testCase "data cleared before added back to pool" <| fun () ->
            let s = Segments<int, int>()
            let data = s.Add(1, UInt64.MaxValue)
            Array.fill data 0 data.Length 1
            s.Commit()
            let data = s.Add(2, 0UL)
            data.[0] |> shouldEqual 0
    ]