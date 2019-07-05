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
        let sid = key.segmentId
        let ci = key.componentIndex
        let mask = c.GetMask sid
        (mask &&& (1UL <<< ci)) <> 0UL
    member c.Get(id : 'c) =
        let key = idToKey id
        let sid = key.segmentId
        let ci = key.componentIndex
        let seg = c.GetSegment(sid)
        if seg.mask &&& (1UL <<< ci) = 0UL then 
            failwithf "Cannot get %s component %d in segment %A" (typeToString typeof<'a>) ci sid
        seg.data.[ci]
    member c.Set(id, value) =
        let key = idToKey id
        let sid = key.segmentId
        let ci = key.componentIndex
        let seg = c.GetSegment(sid)
        if seg.mask &&& (1UL <<< ci) = 0UL then 
            failwithf "Cannot set %s component %d in segment %A" (typeToString typeof<'a>) ci sid
        seg.data.[ci] <- value
    member c.Get(id, fallback) =
        let key = idToKey id
        let sid = key.segmentId
        let ci = key.componentIndex
        match c.TryFind(sid) with
        | false, _ -> fallback
        | true, si ->
            let s = c.[si]
            if s.mask &&& (1UL <<< ci) = 0UL then fallback
            else s.data.[ci]                
    member c.Add(id, value) =
        let key = idToKey id
        let sid = key.segmentId
        let ci = key.componentIndex
        let data = c.AddMask(sid, 1UL <<< ci)
        data.[ci] <- value
    member c.AddOrSet(id, value) =
        let key = idToKey id
        let sid = key.segmentId
        let ci = key.componentIndex
        let mask = 1UL <<< ci
        let data =
            match c.TryFind(sid) with
            | false, _ -> c.AddMask(sid, mask)
            | true, si ->
                let s = c.[si]
                if s.mask &&& mask = 0UL then c.AddMask(sid, mask)
                else s.data
        data.[ci] <- value            
    /// Removes single component
    member c.Remove(id) =
        let key = idToKey id
        let sid = key.segmentId
        let ci = key.componentIndex
        c.RemoveMask(sid, 1UL <<< ci)
    
type ComponentStore<'k, 'c 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality
    and 'c :> IComparable<'c> >(idToKey : 'c -> ComponentKey<'k>) =
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
    override c.ToString() =
        let prefix = ""
        lookup.Values
        |> Seq.map (fun item -> item.ToString().Replace("\n", "\n  "))
        |> listToString (prefix + "  ") (c.GetType() |> typeToString)
