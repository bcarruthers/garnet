module Garnet.Tests.Serialization

open System
open System.Collections.Generic
open System.IO
open Expecto
open Garnet.Ecs
open Garnet.Formatting
open Garnet.Actors

let testRoundtripValue (serializer : ISerializer<_>) x =
    let ms = new MemoryStream()
    serializer.Write ms x
    ms.Position <- 0L
    let x2 = serializer.Read ms
    x2 |> shouldEqual x
    //printfn "%s: %d" (typeToString (serializer.GetType())) ms.Length

let testRoundtrip (serializer : ISerializer<_>) =
    let x = Unchecked.defaultof<_>
    testRoundtripValue serializer x

[<Struct>]
type Test1 = {
    field1 : int
    field2 : uint64
    field3 : uint64
    }

type Enum1 =
    | Case1 = 0uy

[<Struct>]
type Test2 = {
    field1 : int
    field2 : Enum1 
    field3 : Test1
    }

[<Tests>]
let tests =
    testList "serialization" [
        testCase "raw serialization roundtrips" <| fun () ->
            testRoundtrip (RawSerializer<MessageHeader>())
            testRoundtrip (RawSerializer<int>())
            testRoundtrip (RawSerializer<Test1>())
            testRoundtrip (RawSerializer<Test2>())
    ]