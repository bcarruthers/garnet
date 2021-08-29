module Garnet.Tests.Registry

open Expecto
open Garnet.Composition

[<AllowNullLiteral>]
type TestClass() = class end

[<Tests>]
let tests =
    testList "registry" [
        testCase "resolve default" <| fun () ->
            let r = Registry()
            r.Get<TestClass>() |> isNull |> shouldEqual false

        testCase "resolve instance" <| fun () ->
            let r = Registry()
            r.Set("a")
            r.Get<string>() |> shouldEqual "a"

        testCase "resolve ref" <| fun () ->
            let r = Registry()
            r.SetFactory<int ref>(fun () -> ref 10)
            r.Get<int ref>().Value |> shouldEqual 10

        testCase "resolve circular" <| fun () ->
            let r = Registry()
            r.SetFactory<int ref>(fun () -> ref (r.Get<string>().Length))
            r.SetFactory<string>(fun () -> r.Get<int ref>().ToString())
            Expect.throws(fun () -> r.Get<int ref>() |> ignore) ""

        testCase "instance overrides factory" <| fun () ->
            let r = Registry()
            r.Set("a")
            r.SetFactory(fun () -> "b")
            r.Get<string>() |> shouldEqual "a"

        testCase "copy registry" <| fun () ->
            let r = Registry()
            r.Set(ref 10)
            r.Set("a")
            let r2 = Registry()
            r.CopyTo(r2)
            r2.Get<int ref>().Value |> shouldEqual 10
            r2.Get<string>() |> shouldEqual "a"
    ]