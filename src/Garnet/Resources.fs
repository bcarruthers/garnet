namespace Garnet.Ecs

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open Garnet.Comparisons
open Garnet.Formatting

/// Allows indexed access and lookup by key
type internal IndexedLookup<'k, 'v when 'k : equality>() =
    let items = List<'v>()
    let idToIndex = Dictionary<'k, int>()
    member c.Entries = idToIndex
    member c.Items = items
    member c.Count = items.Count
    member inline c.Item 
        with get i = items.[i]
        and set i x = items.[i] <- x
    member inline c.TryGetIndex(id, [<Out>] i : byref<_>) = 
        idToIndex.TryGetValue(id, &i)
    member c.TryGet(id, [<Out>] x : byref<_>) = 
        let mutable index = 0
        let result = c.TryGetIndex(id, &index)
        if result then x <- items.[index]
        result
    /// Returns index of new item
    member c.Add(id, x) =
        let index = items.Count
        items.Add(x)
        idToIndex.Add(id, index)
        index
    member c.Clear() =
        items.Clear()
        idToIndex.Clear()

type IResourceLookup =
    abstract member RegisterResource<'a> : (unit -> 'a) -> unit
    abstract member AddResource<'a> : 'a -> unit
    abstract member TryGetResource<'a> : [<Out>] value : byref<'a> -> bool
    
/// Bag/locator of single-instance objects
type ResourceLookup() =
    let resources = IndexedLookup<Type, obj>()
    let resolvers = IndexedLookup<Type, obj>()
    // This is lazy-eval and requires parameterless constructor
    // The intent is to provide a single, immutable reference that does
    // not depend on order of access or dynamic params such as settings.
    // Dynamic values should be handled separately.
    member c.RegisterResource<'a>(f : unit -> 'a) =
        resolvers.Add(typeof<'a>, f) |> ignore
    member c.AddResource<'a>(x : 'a) =
        resources.Add(typeof<'a>, x) |> ignore
    member c.TryGetResource<'a>([<Out>] r : byref<'a>) =
        // first try to get instance
        match resources.TryGet(typeof<'a>) with
        | true, x -> r <- x :?> 'a; true
        | false, _ ->
            // if none, try to create with resolver if present
            match resolvers.TryGet(typeof<'a>) with
            | true, x -> 
                let resolve = x :?> (unit -> 'a)
                let resource = resolve()
                resources.Add(typeof<'a>, resource) |> ignore
                r <- resource
                true
            | false, _ -> false
    interface IResourceLookup with
        member c.RegisterResource f = c.RegisterResource f
        member c.AddResource x = c.AddResource x
        member c.TryGetResource<'a>([<Out>] r : byref<_>) = 
            c.TryGetResource<'a>(&r)
    override c.ToString() =
        let prefix = ""
        resources.Items
        |> Seq.map (fun item -> item.ToString().Replace("\n", "\n  "))
        |> listToString (prefix + "  ") (prefix + "Types")

/// Different from ref because no initial value required
type State<'a>() =
    let mutable v = Unchecked.defaultof<'a>
    let mutable hasValue = false
    member c.Value = v
    member c.Set newValue =
        hasValue <- true
        v <- newValue
    member c.GetOrDefault fallback =
        if hasValue then c.Value else fallback
    override c.ToString() =
        sprintf "%s\n  %s" (c.GetType() |> typeToString) (formatRecord "  " c.Value)

[<AutoOpen>]
module ResourceExtensions =
    type IResourceLookup with
        member c.GetResource<'a>(resolve : unit -> 'a) : 'a =
            match c.TryGetResource<'a>() with
            | true, x -> x
            | false, _ -> 
                let x = resolve()
                c.AddResource<'a>(x)
                x
        member c.GetResource<'a>() : 'a =
            match c.TryGetResource<'a>() with
            | true, x -> x
            | false, _ -> 
                let x = Activator.CreateInstance<'a>()
                c.AddResource<'a>(x)
                x
        member c.GetResource<'a>(fallback) : 'a =
            match c.TryGetResource<'a>() with
            | true, x -> x
            | false, _ -> 
                let x = fallback
                c.AddResource<'a>(x)
                x
        member c.CreateResource x =
            c.AddResource x
            x
        member c.GetState<'a>() = c.GetResource<State<'a>>()
        member c.GetValue<'a>() = c.GetState<'a>().Value
        member c.SetValue<'a>(value) = c.GetState<'a>().Set value        
