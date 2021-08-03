namespace Garnet.Composition

open System
open System.Collections.Generic
open Garnet.Formatting

type IRegistrant =
    abstract member Register : Container -> IDisposable

type Registrant(register : Container -> IDisposable) =
    member _.Register(c) = register c
    interface IRegistrant with
        member _.Register(c) = register c

[<AbstractClass>]
type RegistrantBase() =
    abstract member Register : Container -> IDisposable
    interface IRegistrant with
        member this.Register(c) = this.Register(c)

type RegistrantCollection() =
    let dict = Dictionary<string, DisposableReference<IDisposable>>()
    member private c.GetSubscription(name) =
        Disposable.Create(fun () -> 
            dict.[name].Dispose()
            dict.Remove(name) |> ignore)
    member c.Contains(name) =
        dict.ContainsKey(name)
    member c.Add(name, create) =
        match dict.TryGetValue(name) with
        | true, entry -> entry.Set(create)
        | false, _ -> 
            let entry = new DisposableReference<_>(create())
            dict.Add(name, entry)
        c.GetSubscription(name)
    member c.Dispose() =
        for entry in dict.Values do
            entry.Dispose()
        dict.Clear()
    member c.ToString(writer : IStringBlockWriter) =
        if writer.BeginList("Systems", dict.Count) then
            for name in Seq.sort dict.Keys do
                writer.Write(name)
            writer.End()
    override c.ToString() =
        StringBlockWriter.Format(c.ToString)

[<AutoOpen>]
module RegistrantCollection =
    type Container with
        /// Register or replace a named system
        member c.Register(name : string, register : Container -> IDisposable) =
            let reg = c.GetValue<RegistrantCollection>()
            reg.Add(name, fun () -> register c)

        member c.Register(t : Type, register : Container -> IDisposable) =
            c.Register(t.Name, register)

        /// Register or replace a named system
        member c.Register(name : string, system : IRegistrant) =
            c.Register(name, fun c -> system.Register(c))

        member c.Register(t : Type, system : IRegistrant) =
            c.Register(t.Name, system)

        /// Register or replace a system with the name of the type
        member c.Register<'a when 'a :> IRegistrant>(system : 'a) =
            c.Register(nameof<'a>, system)
            
        /// Register or replace systems, each using the name of its type
        member c.RegisterAll(systems : seq<IRegistrant>) =
            Disposable.Create [
                for system in systems do
                    c.Register(system.GetType(), system)
                ]
