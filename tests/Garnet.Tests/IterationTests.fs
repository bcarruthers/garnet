﻿module Garnet.Tests.Iteration

open System
open System.Collections.Generic
open Expecto
open Garnet.Composition

[<Tests>]
let tests =
    testList "iteration" [
        testCase "iter2" <| fun () ->
            let c = ComponentStore<_,_,EidSegmentKeyMapper>()
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
            
        testCase "where" <| fun () ->
            let c = ComponentStore<_,_,EidSegmentKeyMapper>()
            for i = 1 to 100 do
                let e = c.Get(Eid i)
                e.Add(i)
                e.Add(Eid i)
            c.Commit()
            let r = List<_>()
            let iterWhere =
                fun param struct(a : int, b : Eid) ->
                    r.Add((param, a, b))
                |> Join.where (fun param struct(a : int, _ : Eid) -> a % 2 = 0)
                |> Join.iter2
                |> Join.over c
            iterWhere 0
            r.Count |> shouldEqual 50            

        testCase "add4" <| fun () ->
            let c = Container()
            let mutable count = 0
            let add4 =
                fun () struct(a : string, b : int, c : char, d : byte) -> 
                    count <- count + 1
                    1us
                |> Join.add4
                |> Join.over c
            for i = 1 to 10 do
                c.Create().With("").With(1).With('a').Add(1uy)
            for i = 1 to 10 do
                c.Create().With("").With(1).Add('a')
            c.Commit()
            add4()
            count |> shouldEqual 10
            c.Commit()
            add4()
            count |> shouldEqual 10
    ]