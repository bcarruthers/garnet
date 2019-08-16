module Garnet.Tests.Registry

open Expecto
open Garnet.Composition

[<Tests>]
let tests =
    testList "registry" [
        testCase "resolve default" <| fun () ->
            let r = Registry()
            r.GetInstance<int>() |> shouldEqual 0

        testCase "resolve instance" <| fun () ->
            let r = Registry()
            r.RegisterInstance 10
            r.GetInstance<int>() |> shouldEqual 10

        testCase "resolve ref" <| fun () ->
            let r = Registry()
            r.Register(fun () -> ref 10)
            r.GetInstance<ref<int>>().Value |> shouldEqual 10

        testCase "resolve circular" <| fun () ->
            let r = Registry()
            r.Register<int>(fun () -> r.GetInstance<string>().Length)
            r.Register<string>(fun () -> r.GetInstance<int>().ToString())
            Expect.throws(fun () -> r.GetInstance<int>() |> ignore) ""

        testCase "instance overrides factory" <| fun () ->
            let r = Registry()
            r.RegisterInstance 11
            r.Register(fun () -> 10)
            r.GetInstance<int>() |> shouldEqual 11
    ]