module Garnet.Benchmarks.Containers

open System.Collections.Generic
open Garnet.Composition

let runCreateDestroyEntities count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to iterations do
        for i = 1 to count do
            let e = c.Create()
            eids.Add e.id
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
            eids.Add e.id
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
        eids.Add e.id
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
        eids.Add e.id
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
        eids.Add e.id
    for i = 1 to iterations do
        for eid in eids do
            c.Get(eid).With(1).With("a").With(1.2).Add('b')
        c.Commit()
        for eid in eids do
            c.Get(eid).Without<int>().Without<string>().Without<double>().Remove<char>()
        c.Commit()

let runIterateEntities count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create()
        eids.Add e.id
    c.Commit()
    let iter =
        fun param (_ : Eid) ->
            ()
        |> Join.iter1
        |> Join.over c
    for i = 1 to iterations do
        iter()

let runIterateMultipleComponents count iterations =
    let c = Container()
    let eids = List<Eid>()
    for i = 1 to count do
        let e = c.Create().With(1).With("a").With(1.2).With('b')
        eids.Add e.id
    c.Commit()
    let iter =
        fun _ struct(_ : int, _ : string, _ : double, _ : char) ->
            ()
        |> Join.iter4
        |> Join.over c
    for i = 1 to iterations do
        iter()
