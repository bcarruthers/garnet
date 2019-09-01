module Garnet.Tests.Iteration

open System
open System.Collections.Generic
open Expecto
open Garnet.Composition

[<Tests>]
let tests =
    testList "iteration" [
        testCase "iter2" <| fun () ->
            let c = ComponentStore(Eid.eidToComponentKey)
            for i = 1 to 100 do
                let e = c.Get(Eid i)
                if i % 2 = 0 then e.Add(i)
                if i % 3 = 0 then e.Add('a' + char i)
                if i % 5 = 0 then e.Add(i.ToString())
            c.Commit()
            let r = List<_>()
            let iter =
                fun param struct(a : int, b : string) ->
                    r.Add((param, a, b))
                |> Join.iter2
                |> Join.over c
            iter 0
            r.Count |> shouldEqual 10
            let r = List<_>()
            let iter =
                fun param struct(a : int, b : char) ->
                    r.Add((param, a, b))
                |> Join.iter2
                |> Join.over c
            iter 0
            r.Count |> shouldEqual 16

    ]