module Garnet.Benchmarks.Containers

open System.Collections.Generic
open Garnet.Composition

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
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create()
        eids.Add(e.Id)
    c.Commit()
    let iter =
        fun _ (_ : Eid) ->
            ()
        |> Join.iter1
        |> Join.over c
    for i = 1 to iterations do
        iter()

let runIterateComponents2 count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create().With(1)
        eids.Add(e.Id)
    c.Commit()
    let mutable sum = 0L
    let iter =
        fun _ struct(x : int, eid : Eid) ->
            sum <- sum + int64 x + int64 eid.Value
        |> Join.iter2
        |> Join.over c
    for i = 1 to iterations do
        iter()
    sum

let runIterateComponents4 count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create().With(1).With("a").With(1.2).With('b')
        eids.Add(e.Id)
    c.Commit()
    let iter =
        fun _ struct(_ : int, _ : string, _ : double, _ : char) ->
            ()
        |> Join.iter4
        |> Join.over c
    for i = 1 to iterations do
        iter()

let runQueryComponents1 count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create().With(1)
        eids.Add(e.Id)
    c.Commit()
    let mutable sum = 0L
    for i = 1 to iterations do
        for batch, values in c.Query<int>() do
            for i in batch do
                let x = values.[i]
                sum <- sum + int64 x
    sum

let runQueryComponents2 count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create().With(1)
        eids.Add(e.Id)
    c.Commit()
    let mutable sum = 0L
    for i = 1 to iterations do
        for batch, values, eids in c.Query<int, Eid>() do
            for i in batch do
                let x = values.[i]
                let eid = eids.[i]
                sum <- sum + int64 x + int64 eid.Value
    sum

let runQueryComponents3 count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create().With(1).With(0.1)
        eids.Add(e.Id)
    c.Commit()
    let mutable sum = 0L
    for i = 1 to iterations do
        for batch, s1, s2, s3 in c.Query<int, Eid, double>() do
            for i in batch do
                sum <- sum + int64 s1.[i] + int64 (s3.[i] * 10.0) + int64 s2.[i].Value
    sum
