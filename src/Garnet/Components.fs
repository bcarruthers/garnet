namespace Garnet.Ecs

open System
open System.Collections.Generic
open Garnet.Comparisons
open Garnet.Formatting

/// Adds convenience methods to access individual components
type Components<'k, 'c, 'a 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality 
    and 'c :> IComparable<'c>>(idToKey) =    
    inherit Segments<'k, 'a>()
    member c.Contains(id) =
        let struct(sid, ci) = idToKey id
        let mask = c.GetMask sid
        (mask &&& (1UL <<< ci)) <> 0UL
    member c.Get(id : 'c) =
        let struct(sid, ci) = idToKey id
        let seg = c.GetSegment(sid)
        if seg.mask &&& (1UL <<< ci) = 0UL then 
            failwithf "Cannot get %s component %d in segment %A" (typeToString typeof<'a>) ci sid
        seg.data.[ci]
    member c.Set(id, value) =
        let struct(sid, ci) = idToKey id
        let seg = c.GetSegment(sid)
        if seg.mask &&& (1UL <<< ci) = 0UL then 
            failwithf "Cannot set %s component %d in segment %A" (typeToString typeof<'a>) ci sid
        seg.data.[ci] <- value
    member c.Get(id, fallback) =
        let struct(sid, ci) = idToKey id
        match c.TryFind(sid) with
        | false, _ -> fallback
        | true, si ->
            let s = c.[si]
            if s.mask &&& (1UL <<< ci) = 0UL then fallback
            else s.data.[ci]                
    member c.Add(id, value) =
        let struct(sid, ci) = idToKey id
        let data = c.AddMask(sid, 1UL <<< ci)
        data.[ci] <- value
    /// Removes single component
    member c.Remove(id) =
        let struct(sid, ci) = idToKey id
        c.RemoveMask(sid, 1UL <<< ci)
    
type ComponentStore<'k, 'c 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality
    and 'c :> IComparable<'c>>(idToKey : 'c -> struct('k * int)) =
    let lookup = Dictionary<Type, ISegments<'k>>()
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
    member c.Handle(id, handler) =        
        let struct(sid, ci) = idToKey id
        let mask = 1UL <<< ci
        for s in lookup.Values do
            s.Handle handler sid mask
    /// Removes all components associated with a given ID
    member c.Destroy(id) =
        let struct(sid, ci) = idToKey id
        c.Remove(sid, 1UL <<< ci)
    member c.Commit() =
        for segs in lookup.Values do
            segs.Commit()
    interface ISegmentStore<'k> with
        member c.Get<'a>() = c.Get<'a>() :> Segments<_,_>
    override c.ToString() =
        let prefix = ""
        lookup.Values
        |> Seq.map (fun item -> item.ToString().Replace("\n", "\n  "))
        |> listToString (prefix + "  ") (c.GetType() |> typeToString)
