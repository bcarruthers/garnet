module Garnet.Tests.Coroutines

open System.Collections.Generic
open Expecto
open Garnet.Composition

[<Tests>]
let tests =
    testList "coroutines" [
        testCase "nested" <| fun _ ->
            let s = CoroutineScheduler()
            let r = List<_>()
            let append = r.Add
            s.Schedule <| seq {
                append 1
                yield Wait.defer
                append 2
                // these happen in parallel
                s.Schedule <| seq {
                    append 21
                    yield Wait.defer
                    append 22
                    }
                s.Schedule <| seq {
                    append 31
                    yield Wait.defer
                    append 32
                    yield Wait.defer
                    append 33
                    }
                append 3
                // here we block until two children complete
                yield Wait.defer
                append 4
                }
            s.Run()
            Expect.sequenceEqual [|1; 2; 3; 21; 31; 22; 32; 33; 4|] r ""

        testCase "waiting on messages" <| fun _ ->
            let c = Container()
            let r = List<_>()
            let append = r.Add
            c.On<string> <| fun e ->
                if e = "1" then 
                    append 21
                    c.Send "2"
                elif e = "2" then
                    append 22
                    c.Send "3"
                else
                    append 23
            |> ignore
            c.Start <| seq {
                append 1
                c.Send "1"
                yield Wait.defer
                append 2
                }
            c.Run()
            // r.ToArray()
            Expect.sequenceEqual [|1; 21; 22; 23; 2|] r ""

        testCase "using container" <| fun _ ->
            let c = Container()
            let r = List<_>()
            let append = r.Add
            c.On<string> <| fun e ->
                c.Start <| seq {
                    append 21
                    yield Wait.defer
                    append 22
                    }
            |> ignore
            c.Start <| seq {
                append 1
                yield Wait.defer
                append 2
                c.Send ""
                yield Wait.defer
                append 3
                yield Wait.defer
                append 4
                }
            c.Run()
            // coroutine waits for nested message handling
            Expect.sequenceEqual [|1; 2; 21; 22; 3; 4|] r ""

        testCase "timed" <| fun _ ->
            let s = CoroutineScheduler()
            let r = List<_>()
            let append = r.Add
            s.Schedule <| seq {
                append 1
                yield Wait.time 2
                append 2
                append 3
                s.Schedule <| seq {
                    append 21
                    yield Wait.time 1
                    append 22
                    yield Wait.time 5
                    append 23
                    }
                yield Wait.time 3
                append 4
                append 5
                }
            s.Run()
            for i = 1 to 8 do
                append (i * 100)
                s.Step 1
                s.Run()
            //r.ToArray()
            Expect.sequenceEqual [|1; 100; 200; 2; 3; 21; 300; 22; 400; 500; 4; 5; 600; 700; 800; 23|] r ""

        testCase "timed nested using container" <| fun _ ->
            let c = Container()
            let r = List<_>()
            let append = r.Add
            c.On<string> <| fun e ->
                c.Start <| seq {
                    append 21
                    yield Wait.defer
                    append 22
                    yield Wait.defer
                    append 23
                    }
            |> ignore
            c.Start <| seq {
                append 1
                c.Send ""
                yield Wait.time 0
                append 2
                }
            c.Run()
            // coroutine does not wait for nested message handling
            Expect.sequenceEqual [|1; 2; 21; 22; 23 |] r ""

        testCase "mixed nested using container" <| fun _ ->
            let c = Container()
            let r = List<_>()
            let append = r.Add
            c.On<string> <| fun e ->
                if e = "update" then
                    c.Start <| seq {
                        append 21
                        yield Wait.defer
                        append 22
                        yield Wait.defer
                        append 23
                        }
                elif e = "anim" then
                    c.Start <| seq {
                        append 31
                        yield Wait.time 1
                        append 32
                        yield Wait.time 2
                        append 33
                        }
            |> ignore
            c.Start <| seq {
                append 1
                c.Send "update"
                yield Wait.defer
                append 2
                c.Send "anim"
                yield Wait.time 5
                append 3
                }
            for i = 1 to 8 do
                append (i * 100)
                c.Step 1
                c.Run()
            //r.ToArray()
            Expect.sequenceEqual [|100; 1; 21; 22; 23; 2; 31; 200; 32; 300; 400; 33; 500; 600; 3; 700; 800|] r ""
    ]
