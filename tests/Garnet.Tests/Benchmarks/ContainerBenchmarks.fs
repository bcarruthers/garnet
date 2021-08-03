module Garnet.Benchmarks.Containers

open System.Collections.Generic
open System.Numerics
open Garnet.Composition

let create1 count =
    let c = Container()
    for i = 1 to count do c.Create().Add(1)
    c.Commit()
    c
    
let create2 count =
    let c = Container()
    for i = 1 to count do c.Create().With(5.0).Add(1)
    c.Commit()
    c
    
let create3 count =
    let c = Container()
    for i = 1 to count do c.Create().With(Vector2(2.0f, 3.0f)).With(0.1).Add(1)
    c.Commit()
    c
    
let create4 count =
    let c = Container()
    for i = 1 to count do c.Create().With("a").With(Vector2(2.0f, 3.0f)).With(0.1).Add(1)
    c.Commit()
    c

let runCreateDestroyEntities count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to iterations do
        for i = 1 to count do
            let e = c.Create()
            eids.Add(e.Id)
        c.Commit()
        for eid in eids do
            c.Destroy eid
        c.Commit()
        eids.Clear()

let runCreateDestroyMultipleComponents count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to iterations do
        for i = 1 to count do
            let e = c.Create().With(1).With("a").With(1.2).With('b')
            eids.Add(e.Id)
        c.Commit()
        for eid in eids do
            c.Destroy eid
        c.Commit()
        eids.Clear()

let runAddRemoveComponent count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create()
        eids.Add(e.Id)
    for i = 1 to iterations do
        for eid in eids do
            c.Get(eid).Add(1)
        c.Commit()
        for eid in eids do
            c.Get(eid).Remove<int>()
        c.Commit()

let runAddRemoveComponentDirect count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create()
        eids.Add(e.Id)
    let cmp = c.Get<int>()
    for i = 1 to iterations do
        for eid in eids do
            cmp.Add(eid, 1)
        c.Commit()
        for eid in eids do
            cmp.Remove(eid)
        c.Commit()

let runAddRemoveMultipleComponents count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create()
        eids.Add(e.Id)
    for i = 1 to iterations do
        for eid in eids do
            c.Get(eid).With(1).With("a").With(1.2).Add('b')
        c.Commit()
        for eid in eids do
            c.Get(eid).Without<int>().Without<string>().Without<double>().Remove<char>()
        c.Commit()

let runIterateComponents1 count iterations =
    let c = create1 count
    let mutable sum = 0L
    let iter =
        fun _ (x1 : int) ->
            sum <- sum + int64 x1
        |> Join.iter1
        |> Join.over c
    for i = 1 to iterations do
        iter()
    sum

let runIterateComponents2 count iterations =
    let c = create2 count
    let mutable sum = 0L
    let iter =
        fun _ struct(x1 : int, x2 : double) ->
            sum <- sum + int64 x1 + int64 x2
        |> Join.iter2
        |> Join.over c
    for i = 1 to iterations do
        iter()
    sum

let runIterateComponents3 count iterations =
    let c = create3 count
    let mutable sum = 0L
    let iter =
        fun _ struct(x1 : int, x2 : double, x3 : Vector2) ->
            sum <- sum + int64 x1 + int64 x2 + int64 x3.X + int64 x3.Y
        |> Join.iter3
        |> Join.over c
    for i = 1 to iterations do
        iter()
    sum
    
let runIterateComponents4 count iterations =
    let c = create4 count
    let mutable sum = 0L
    let iter =
        fun _ struct(x1 : int, x2 : double, x3 : Vector2, x4 : string) ->
            sum <- sum + int64 x1 + int64 x2 + int64 x3.X + int64 x3.Y + int64 x4.Length
        |> Join.iter4
        |> Join.over c
    for i = 1 to iterations do
        iter()
    sum
    
let runQuerySegments1 count iterations =
    let c = create1 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for seg, s1 in c.QuerySegments<int>() do
            for i in seg do
                let x = s1.[i]
                sum <- sum + int64 x
    sum

let runQuerySegments2 count iterations =
    let c = create2 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for seg, s1, s2 in c.QuerySegments<int, double>() do
            for i in seg do
                sum <- sum + int64 s1.[i] + int64 s2.[i]
    sum

let runQuerySegments3 count iterations =
    let c = create3 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for seg, s1, s2, s3 in c.QuerySegments<int, double, Vector2>() do
            for i in seg do
                sum <- sum + int64 s1.[i] + int64 s2.[i] + int64 s3.[i].X + int64 s3.[i].Y
    sum

let runQuerySegments4 count iterations =
    let c = create4 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for seg, s1, s2, s3, s4 in c.QuerySegments<int, double, Vector2, string>() do
            for i in seg do
                sum <- sum + int64 s1.[i] + int64 s2.[i] + int64 s3.[i].X + int64 s3.[i].Y + int64 s4.[i].Length
    sum

let runQueryComponents1 count iterations =
    let c = create1 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for r in c.Query<int>() do
            sum <- sum + int64 r.Value
    sum

let runQueryComponentsTuple2 count iterations =
    let c = create2 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for r in c.Query<int, double>() do
            let struct(x1, x2) = r.Values
            sum <- sum + int64 x1 + int64 x2
    sum

let runQueryComponentsTuple3 count iterations =
    let c = create3 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for r in c.Query<int, double, Vector2>() do
            let struct(x1, x2, x3) = r.Values
            sum <- sum + int64 x1 + int64 x2 + int64 x3.X + int64 x3.Y
    sum

let runQueryComponentsTuple4 count iterations =
    let c = create4 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for r in c.Query<int, double, Vector2, string>() do
            let struct(x1, x2, x3, x4) = r.Values
            sum <- sum + int64 x1 + int64 x2 + int64 x3.X + int64 x3.Y + int64 x4.Length
    sum

let runQueryComponents2 count iterations =
    let c = create2 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for r in c.Query<int, double>() do
            sum <- sum + int64 r.Value1 + int64 r.Value2
    sum

let runQueryComponents3 count iterations =
    let c = create3 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for r in c.Query<int, double, Vector2>() do
            sum <- sum + int64 r.Value1 + int64 r.Value2 + int64 r.Value3.X + int64 r.Value3.Y
    sum

let runQueryComponents4 count iterations =
    let c = create4 count
    let mutable sum = 0L
    for i = 1 to iterations do
        for r in c.Query<int, double, Vector2, string>() do
            sum <- sum + int64 r.Value1 + int64 r.Value2 + int64 r.Value3.X + int64 r.Value3.Y + int64 r.Value4.Length
    sum
