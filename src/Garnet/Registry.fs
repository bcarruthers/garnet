namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Threading
open Garnet.Comparisons
open Garnet.Formatting

type IRegistryHandler<'p> =
    /// Serves as a callback when iterating over typed instances in registry.
    /// Takes a custom param, index, and instance
    abstract member Handle<'a when 'a : not struct> : 'p * int * 'a -> unit

/// Provides methods to register and resolve single-instance objects by type
type IRegistry =
    /// Registers a factory for creating values of a specific type
    abstract member RegisterFactory<'a when 'a : not struct> : (unit -> 'a) -> unit
    /// Adds or replaces a specific instance of a type
    abstract member RegisterInstance<'a when 'a : not struct> : 'a -> unit
    /// Attempts to resolve a type, returning true if successful
    abstract member TryGetInstance<'a when 'a : not struct> : [<Out>] value : byref<'a> -> bool
    /// Attempts to resolve a type, throwing exception if unsuccessful
    abstract member GetInstance<'a when 'a : not struct> : unit -> 'a
    /// Iterates over all instances, calling handler for each
    abstract member IterInstances<'p> : 'p * IRegistryHandler<'p> -> unit
    
type private IRegistryEntryHandler =
    abstract member Handle<'p> : 'p * int * IRegistryHandler<'p> -> unit

type private RegistryEntryHandler<'a when 'a : not struct>(instance : 'a) =
    interface IRegistryEntryHandler with 
        member c.Handle<'p>(param : 'p, index, handler) =
            handler.Handle(param, index, instance)

[<Struct>]
type private RegistryEntry = {
    Handler : IRegistryEntryHandler
    Instance : obj
    } with
    static member Create<'a when 'a : not struct>(instance : 'a) = {
        Handler = RegistryEntryHandler<'a>(instance)
        Instance = instance
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
    member c.RegisterFactory<'a when 'a : not struct>(create : unit -> 'a) =
        let id = RegistryTypeId<'a>.Id
        if id >= factories.Length then
            Garnet.Buffer.resizeArray (id + 1) &factories
        factories.[id] <- Func<_>(fun () ->
            // Entry is temporarily marked null to detect cycles
            if obj.ReferenceEquals(factories.[id], null) 
                then failwithf "Cycle detected for %s" (Format.typeToString typeof<'a>)
                else
                    // Mark null to detect cycles
                    let factory = factories.[id]
                    factories.[id] <- null
                    // Instantiate type
                    let value = RegistryEntry.Create(create())
                    // Restore factory
                    factories.[id] <- factory
                    value)    
    member c.RegisterInstance<'a when 'a : not struct>(newValue : 'a) =
        let id = RegistryTypeId<'a>.Id
        if id >= lookup.Length then
            Garnet.Buffer.resizeArray (id + 1) &lookup
        if isNotNull lookup.[id] then
            // If replacing is a common scenario, then can optimize this
            instances.RemoveAll(fun e -> e.Instance.GetType().Equals(typeof<'a>)) |> ignore
        lookup.[id] <- newValue :> obj        
        instances.Add(RegistryEntry.Create(newValue))
    member c.TryGetInstance<'a when 'a : not struct>([<Out>] r : byref<'a>) =
        let id = RegistryTypeId<'a>.Id
        if id >= lookup.Length then
            Garnet.Buffer.resizeArray (id + 1) &lookup    
        let value = lookup.[id]
        if isNotNull value then r <- value :?> 'a; true
        elif id >= factories.Length then false
        else
            let factory = factories.[id]
            if isNull factory then false
            else
                let entry = factory.Invoke()
                lookup.[id] <- entry.Instance        
                instances.Add(entry)
                r <- entry.Instance :?> 'a
                true                
    member c.GetInstance<'a when 'a : not struct>() : 'a =
        match c.TryGetInstance<'a>() with
        | true, x -> x
        | false, _ -> 
            let x = Activator.CreateInstance<'a>()
            c.RegisterInstance<'a>(x)
            x
    member c.IterInstances(param, handler) =
        c.IterInstances(param, handler, 0)
    member c.IterInstances(param, handler : IRegistryHandler<'p>, offset) =
        for i = 0 to instances.Count - 1 do
            instances.[i].Handler.Handle(param, offset + i, handler)
    interface IRegistry with
        member c.RegisterFactory(x) = c.RegisterFactory(x)
        member c.RegisterInstance(x) = c.RegisterInstance(x)
        member c.TryGetInstance([<Out>] r : byref<_>) = c.TryGetInstance(&r)
        member c.GetInstance<'a when 'a : not struct>() : 'a = c.GetInstance<'a>()
        member c.IterInstances(param, handler) =
            c.IterInstances(param, handler)
    member c.ToString(writer : IStringBlockWriter) =
        if writer.BeginList("Types", instances.Count) then
            let sorted = 
                instances 
                |> Seq.sortBy (fun x -> x.Instance.GetType().Name)
            for item in sorted do
                writer.Write(item.Instance.ToString().Replace("\n", "\n  "))
            writer.End()
    override c.ToString() =
        StringBlockWriter.Format(c.ToString)
        
type private CopyRegistryHandler() =
    interface IRegistryHandler<IRegistry> with
        member c.Handle<'a when 'a : not struct>(registry, _, instance : 'a) =
            registry.RegisterInstance<'a>(instance)

[<AutoOpen>]
module Registry =
    type IRegistry with
        member c.CopyTo(dest : IRegistry) =
            let handler = CopyRegistryHandler()
            c.IterInstances(dest, handler)