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
            r.GetInstance<TestClass>() |> isNull |> shouldEqual false

        testCase "resolve instance" <| fun () ->
            let r = Registry()
            r.RegisterInstance("a")
            r.GetInstance<string>() |> shouldEqual "a"

        testCase "resolve ref" <| fun () ->
            let r = Registry()
            r.RegisterFactory<int ref>(fun () -> ref 10)
            r.GetInstance<int ref>().Value |> shouldEqual 10

        testCase "resolve circular" <| fun () ->
            let r = Registry()
            r.RegisterFactory<int ref>(fun () -> ref (r.GetInstance<string>().Length))
            r.RegisterFactory<string>(fun () -> r.GetInstance<int ref>().ToString())
            Expect.throws(fun () -> r.GetInstance<int ref>() |> ignore) ""

        testCase "instance overrides factory" <| fun () ->
            let r = Registry()
            r.RegisterInstance("a")
            r.RegisterFactory(fun () -> "b")
            r.GetInstance<string>() |> shouldEqual "a"

        testCase "copy registry" <| fun () ->
            let r = Registry()
            r.RegisterInstance(ref 10)
            r.RegisterInstance("a")
            let r2 = Registry()
            r.CopyTo(r2)
            r2.GetInstance<int ref>().Value |> shouldEqual 10
            r2.GetInstance<string>() |> shouldEqual "a"
    ]