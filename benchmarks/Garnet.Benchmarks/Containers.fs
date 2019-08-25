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
        eids.Clear()
        c.Commit()

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
        eids.Clear()
        c.Commit()

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
