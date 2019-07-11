module Garnet.Tests.Segments

open System
open Expecto
open Garnet.Ecs

[<Tests>]
let tests =
    testList "segments" [
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