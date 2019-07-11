namespace Garnet.Ecs

open System
open System.Runtime.InteropServices
open Garnet.Comparisons
open Garnet.Formatting
open Garnet.Collections

type IRegistry =
    abstract member Register<'a> : (unit -> 'a) -> unit
    abstract member RegisterInstance<'a> : 'a -> unit
    abstract member TryGetInstance<'a> : [<Out>] value : byref<'a> -> bool
    
/// Bag/locator of single-instance objects
type Registry() =
    let instances = IndexedLookup<Type, obj>()
    let resolvers = IndexedLookup<Type, obj>()
    // This is lazy-eval and requires parameterless constructor
    // The intent is to provide a single, immutable reference that does
    // not depend on order of access or dynamic params such as settings.
    // Dynamic values should be handled separately.
    member c.Register<'a>(f : unit -> 'a) =
        resolvers.Add(typeof<'a>, f) |> ignore
    member c.RegisterInstance<'a>(x : 'a) =
        instances.Add(typeof<'a>, x) |> ignore
    member c.TryGetInstance<'a>([<Out>] r : byref<'a>) =
        // first try to get instance
        match instances.TryGet(typeof<'a>) with
        | true, x -> r <- x :?> 'a; true
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
                    let resource = resolve()
                    instances.Add(typeof<'a>, resource) |> ignore
                    r <- resource
                    true
            | false, _ -> false
    interface IRegistry with
        member c.Register f = c.Register f
        member c.RegisterInstance x = c.RegisterInstance x
        member c.TryGetInstance<'a>([<Out>] r : byref<_>) = 
            c.TryGetInstance<'a>(&r)
    override c.ToString() =
        let prefix = ""
        instances.Items
        |> Seq.map (fun item -> item.ToString().Replace("\n", "\n  "))
        |> listToString (prefix + "  ") (prefix + "Types")
        
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
