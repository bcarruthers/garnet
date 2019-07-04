namespace Garnet.Ecs

open System
open System.Collections.Generic
open Garnet.Comparisons
open Garnet.Formatting

[<Struct>]
type ComponentKey<'k when 'k :> IComparable<'k> and 'k : equality> = {
    segmentId : 'k
    componentIndex : int
    }

/// Adds convenience methods to access individual components
type Components<'k, 'c, 'a 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality 
    and 'c :> IComparable<'c>>(idToKey) =    
    inherit Segments<'k, 'a>()
    member c.Contains(id) =
        let key = idToKey id
        c.Contains(key.segmentId, key.componentIndex)
    member c.Get(id : 'c) =
        let key = idToKey id
        c.Get(key.segmentId, key.componentIndex)
    member c.Set(id, value) =
        let key = idToKey id
        c.Set(key.segmentId, key.componentIndex, value)
    member c.GetOrDefault(id, fallback) =
        let key = idToKey id
        c.GetOrDefault(key.segmentId, key.componentIndex, fallback)
    member c.Add(id, value) =
        let key = idToKey id
        c.Add(key.segmentId, key.componentIndex, value)
    member c.AddOrSet(id, value) =
        let key = idToKey id
        c.AddOrSet(key.segmentId, key.componentIndex, value)
    /// Removes single component
    member c.Remove(id) =
        let key = idToKey id
        c.Remove(key.segmentId, key.componentIndex)
    
type ISegmentsLookup<'k, 'c 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality 
    and 'c :> IComparable<'c>> =
    inherit ISegmentStore<'k>
    abstract member Get<'b> : unit -> Components<'k, 'c, 'b>
    abstract member Handle : 'c -> ISegmentHandler -> unit
    abstract member Destroy : 'c -> unit
    
type ComponentStore<'k, 'c 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality
    and 'c :> IComparable<'c> >(idToKey : 'c -> ComponentKey<'k>) =
    let lookup = Dictionary<Type, ISegments<'k>>()
    member c.Lists = lookup.Values
    member c.Get<'a>() =
        let t = typeof<'a>
        match lookup.TryGetValue(t) with
        | true, segs -> segs :?> Components<'k, 'c, 'a>
        | false, _ ->
            let segs = Components<'k, 'c, 'a>(idToKey)
            lookup.Add(t, segs)
            segs
    member c.Clear() =
        for segs in lookup.Values do
            segs.Clear()
    member c.Remove(sid, mask) =
        for segs in lookup.Values do
            segs.RemoveMask(sid, mask)
    member c.Handle id handler =        
        let key = idToKey id
        let mask = 1UL <<< key.componentIndex
        for s in lookup.Values do
            s.Handle handler key.segmentId mask
    /// Removes all components associated with a given ID
    member c.Destroy(id) =
        let key = idToKey id
        c.Remove(key.segmentId, 1UL <<< key.componentIndex)
    member c.Commit() =
        for segs in lookup.Values do
            segs.Commit()
    interface ISegmentStore<'k> with
        member c.Get<'a>() = c.Get<'a>() :> Segments<_,_>
    interface ISegmentsLookup<'k, 'c> with
        member c.Get<'a>() = c.Get<'a>()
        member c.Destroy(id) = c.Destroy id
        member c.Handle id handler = c.Handle id handler
    override c.ToString() =
        let prefix = ""
        c.Lists
        |> Seq.map (fun item -> item.ToString().Replace("\n", "\n  "))
        |> listToString (prefix + "  ") (c.GetType() |> typeToString)
        
[<Struct>]
type Entity<'k, 'c, 'lookup 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality  
    and 'c :> IComparable<'c> 
    and 'lookup :> ISegmentsLookup<'k, 'c>> = {
    id : 'c
    container : 'lookup
    } with
    member e.Add x = e.container.Get<_>().Add(e.id, x)
    member e.Set x = e.container.Get<_>().Set(e.id, x)
    member e.AddOrSet x = e.container.Get<_>().AddOrSet(e.id, x)
    member e.Remove<'a>() = e.container.Get<'a>().Remove(e.id)
    member e.Get<'a>() = e.container.Get<'a>().Get(e.id)    
    member e.GetOrDefault<'a>(fallback) = e.container.Get<'a>().GetOrDefault(e.id, fallback)
    member e.Contains<'a>() = e.container.Get<'a>().Contains(e.id)
    member e.Destroy() = e.container.Destroy(e.id)
    member e.With x = e.Add x; e
    override e.ToString() = 
        let printer = PrintHandler(UInt64.MaxValue)
        e.container.Handle e.id printer
        "Entity " + e.id.ToString() + ": " + printer.ToString()
