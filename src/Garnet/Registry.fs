namespace Garnet.Composition

open System
open System.Runtime.InteropServices
open Garnet.Comparisons
open Garnet.Formatting
open Garnet.Collections

type IHandler<'p> =
    abstract member Handle<'a> : 'p * int * 'a -> unit

/// Provides methods to register and resolve single-instance
/// objects by type
type IRegistry =
    /// Registers a factory for creating values of a specific type
    abstract member Register<'a> : (unit -> 'a) -> unit
    /// Registers a specific instance of a type
    abstract member RegisterInstance<'a> : 'a -> unit
    /// Attempts to resolve a type, returning true if successful
    abstract member TryGetInstance<'a> : [<Out>] value : byref<'a> -> bool
    abstract member IterInstances<'p> : 'p * IHandler<'p> -> unit
    
type private IRegistryEntryHandler =
    abstract member Handle<'p> : 'p * int * IHandler<'p> -> unit

type private RegistryEntryHandler<'a>(instance : 'a) =
    interface IRegistryEntryHandler with 
        member c.Handle<'p>(param : 'p, index, handler) =
            handler.Handle(param, index, instance)

[<Struct>]
type private RegistryEntry = {
    handler : IRegistryEntryHandler
    instance : obj
    }

module private RegistryEntry =
    let init<'a> (instance : 'a) = {
        handler = RegistryEntryHandler<'a>(instance)
        instance = instance
        }        

/// Provides methods to register and resolve single-instance
/// objects by type
type Registry() =
    let instances = IndexedLookup<Type, RegistryEntry>()
    let resolvers = IndexedLookup<Type, obj>()
    // This is lazy-eval and requires parameterless constructor
    // The intent is to provide a single, immutable reference that does
    // not depend on order of access or dynamic params such as settings.
    // Dynamic values should be handled separately.
    member c.Register<'a>(f : unit -> 'a) =
        resolvers.Add(typeof<'a>, f) |> ignore
    member c.RegisterInstance<'a>(x : 'a) =
        instances.Add(typeof<'a>, RegistryEntry.init x) |> ignore
    member c.TryGetInstance<'a>([<Out>] r : byref<'a>) =
        // first try to get instance
        match instances.TryGet(typeof<'a>) with
        | true, x -> r <- x.instance :?> 'a; true
        | false, _ ->
            // if none, try to create with resolver if present
            match resolvers.TryGetIndex(typeof<'a>) with
            | true, i -> 
                let x = resolvers.[i]
                if obj.ReferenceEquals(x, null) 
                then failwithf "Cycle detected for %s" (typeToString typeof<'a>)
                else
                    // immediately mark null to detect cycles
                    resolvers.[i] <- null
                    let resolve = x :?> (unit -> 'a)
                    let instance = resolve()
                    instances.Add(typeof<'a>, RegistryEntry.init instance) |> ignore
                    r <- instance
                    true
            | false, _ -> false
    member c.IterInstances(param, handler) =
        c.IterInstances(param, handler, 0)
    member c.IterInstances(param, handler : IHandler<'p>, offset) =
        for i = 0 to instances.Count - 1 do
            instances.[i].handler.Handle(param, offset + i, handler)
    interface IRegistry with
        member c.Register f = c.Register f
        member c.RegisterInstance x = c.RegisterInstance x
        member c.TryGetInstance<'a>([<Out>] r : byref<_>) = 
            c.TryGetInstance<'a>(&r)
        member c.IterInstances(param, handler) =
            c.IterInstances(param, handler)
    override c.ToString() =
        let prefix = ""
        instances.Items
        |> Seq.map (fun item -> item.ToString().Replace("\n", "\n  "))
        |> listToString (prefix + "  ") (prefix + "Types")
        
type private CopyRegistryHandler() =
    interface IHandler<IRegistry> with
        member c.Handle<'a>(registry, index, instance : 'a) =
            registry.RegisterInstance<'a>(instance)

[<AutoOpen>]
module Registry =
    type IRegistry with
        member c.GetInstance<'a>() : 'a =
            match c.TryGetInstance<'a>() with
            | true, x -> x
            | false, _ -> 
                let x = Activator.CreateInstance<'a>()
                c.RegisterInstance<'a>(x)
                x

        member c.CopyTo(dest : IRegistry) =
            let handler = CopyRegistryHandler()
            c.IterInstances(dest, handler)