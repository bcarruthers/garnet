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
            r.GetValue<TestClass>() |> isNull |> shouldEqual false

        testCase "resolve instance" <| fun () ->
            let r = Registry()
            r.SetValue("a")
            r.GetValue<string>() |> shouldEqual "a"

        testCase "resolve ref" <| fun () ->
            let r = Registry()
            r.SetFactory<int ref>(fun () -> ref 10)
            r.GetValue<int ref>().Value |> shouldEqual 10

        testCase "resolve circular" <| fun () ->
            let r = Registry()
            r.SetFactory<int ref>(fun () -> ref (r.GetValue<string>().Length))
            r.SetFactory<string>(fun () -> r.GetValue<int ref>().ToString())
            Expect.throws(fun () -> r.GetValue<int ref>() |> ignore) ""

        testCase "instance overrides factory" <| fun () ->
            let r = Registry()
            r.SetValue("a")
            r.SetFactory(fun () -> "b")
            r.GetValue<string>() |> shouldEqual "a"

        testCase "copy registry" <| fun () ->
            let r = Registry()
            r.SetValue(ref 10)
            r.SetValue("a")
            let r2 = Registry()
            r.CopyTo(r2)
            r2.GetValue<int ref>().Value |> shouldEqual 10
            r2.GetValue<string>() |> shouldEqual "a"
    ]