﻿namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Threading
open Garnet.Composition.Comparisons

type IRegistryHandler<'p> =
    /// Serves as a callback when iterating over typed instances in registry.
    /// Takes a custom param, index, and instance
    abstract member Handle<'a> : 'p * int * 'a byref-> unit

/// Provides methods to register and resolve single-instance objects by type
type IRegistry =
    /// Registers a factory for creating values of a specific type
    abstract member SetFactory<'a> : (unit -> 'a) -> unit
    /// Adds or replaces a specific instance of a type
    abstract member Set<'a> : 'a -> unit
    /// Gets or creates a reference to a typed value
    abstract member Get<'a> : unit -> 'a byref
    /// Attempts to resolve a type, returning true if successful
    abstract member TryGet<'a> : [<Out>] value : byref<'a> -> bool
    /// Iterates over all instances, calling handler for each
    abstract member Iter<'p> : 'p * IRegistryHandler<'p> -> unit
    
type private IRegistryEntryHandler =
    abstract member Handle<'p> : 'p * int * IRegistryHandler<'p> -> unit

type private RegistryEntryHandler<'a>(instance : 'a ref) =
    interface IRegistryEntryHandler with 
        member c.Handle<'p>(param : 'p, index, handler) =
            handler.Handle(param, index, &instance.contents)

[<Struct>]
type private RegistryEntry = {
    Handler : IRegistryEntryHandler
    Reference : obj
    } with
    static member Create<'a>(instance : 'a ref) = {
        Handler = RegistryEntryHandler<'a>(instance)
        Reference = instance
        }        

type internal RegistryTypeId() =
    static let mutable id  = 0
    static member GetNext() = Interlocked.Increment(&id)

type internal RegistryTypeId<'a>() =
    static let mutable id = MessageTypeId.GetNext()
    static member Id = id

/// Provides methods to register and resolve single-instance objects by type
type Registry() =
    let mutable factories = Array.zeroCreate<Func<RegistryEntry>>(8)
    let mutable lookup = Array.zeroCreate<obj>(8)
    let instances = List<RegistryEntry>()
    member private c.TryGetReference<'a>([<Out>] reference : byref<'a ref>) =
        let id = RegistryTypeId<'a>.Id
        if id >= lookup.Length then
            Buffer.resizeArray (id + 1) &lookup    
        let value = lookup.[id]
        if isNotNull value then
            // Value is present already
            reference <- value :?> 'a ref
            true
        elif id < factories.Length && isNotNull factories.[id] then
            // Use factory to create
            let factory = factories.[id]
            let entry = factory.Invoke()
            lookup.[id] <- entry.Reference        
            instances.Add(entry)
            reference <- entry.Reference :?> 'a ref
            true
        else false
    member c.SetFactory<'a>(create : unit -> 'a) =
        let id = RegistryTypeId<'a>.Id
        if id >= factories.Length then
            Buffer.resizeArray (id + 1) &factories
        factories.[id] <- Func<_>(fun () ->
            // Entry is temporarily marked null to detect cycles
            if obj.ReferenceEquals(factories.[id], null) 
                then failwithf "Cycle detected for %s" (Format.typeToString typeof<'a>)
                else
                    // Mark null to detect cycles
                    let factory = factories.[id]
                    factories.[id] <- null
                    // Instantiate type
                    let value = create()
                    let cell = ref value
                    let entry = RegistryEntry.Create(cell)
                    // Restore factory
                    factories.[id] <- factory
                    entry)    
    member c.Set<'a>(newValue : 'a) =
        let id = RegistryTypeId<'a>.Id
        if id >= lookup.Length then
            Buffer.resizeArray (id + 1) &lookup
        if isNull lookup.[id] then
            let cell = ref newValue
            lookup.[id] <- cell :> obj
            instances.Add(RegistryEntry.Create(cell))
        let cell = lookup.[id] :?> 'a ref
        cell.Value <- newValue        
    member c.Get<'a>() =
        let mutable cell = Unchecked.defaultof<_>
        if c.TryGetReference(&cell) then &cell.contents
        else
            // No factory, create default value
            let value = Activator.CreateInstance<'a>()
            let cell = ref value
            let id = RegistryTypeId<'a>.Id
            lookup.[id] <- cell :> obj
            instances.Add(RegistryEntry.Create(cell))
            &cell.contents
    member c.TryGet<'a>([<Out>] value : byref<'a>) =
        let mutable cell = Unchecked.defaultof<_>
        let result = c.TryGetReference(&cell)
        if result then value <- cell.contents
        result
    member c.Iter(param, handler) =
        c.Iter(param, handler, 0)
    member c.Iter(param, handler : IRegistryHandler<'p>, offset) =
        for i = 0 to instances.Count - 1 do
            instances.[i].Handler.Handle(param, offset + i, handler)
    interface IRegistry with
        member c.SetFactory(x) = c.SetFactory(x)
        member c.Set(x) = c.Set(x)
        member c.Get<'a>() = &c.Get<'a>()
        member c.TryGet<'a>([<Out>] value) = c.TryGet<'a>(&value)
        member c.Iter(param, handler) =
            c.Iter(param, handler)
    member c.ToString(writer : IStringBlockWriter) =
        if writer.BeginList("Types", instances.Count) then
            let sorted = 
                instances 
                |> Seq.sortBy (fun x -> x.Reference.GetType().Name)
            for item in sorted do
                writer.Write(item.Reference.ToString().Replace("\n", "\n  "))
            writer.End()
    override c.ToString() =
        StringBlockWriter.Format(c.ToString)
        
type private CopyRegistryHandler() =
    interface IRegistryHandler<IRegistry> with
        member c.Handle<'a>(registry, _, instance : 'a byref) =
            registry.Set<'a>(instance)

[<AutoOpen>]
module Registry =
    type IRegistry with
        member c.CopyTo(dest : IRegistry) =
            let handler = CopyRegistryHandler()
            c.Iter(dest, handler)
            
        member c.GetOrDefault<'a>(fallback : 'a) =
            match c.TryGet<'a>() with
            | true, x -> x
            | false, _ -> fallback

        member c.GetOrDefault<'a>() =
            c.GetOrDefault(Unchecked.defaultof<'a>)

        member c.GetOrSetDefault<'a>(fallback : 'a) =
            match c.TryGet<'a>() with
            | true, value -> value 
            | false, _ -> c.Set(fallback); fallback
