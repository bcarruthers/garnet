namespace Garnet.Ecs

open System
open Garnet.Comparisons
open Garnet.Formatting

/// Adds convenience methods to access individual components
[<Struct>]
type Components<'k, 'c, 'a 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality 
    and 'c :> IComparable<'c>>
    (
        segs : Segments<'k, 'a>,
        idToKey : 'c -> struct('k * int)
    ) =
    new(idToKey) =
        Components(Segments(), idToKey)
    member c.Segments = segs
    member c.Count = 
        let mutable total = 0
        for i = 0 to segs.Count - 1 do
            total <- total + bitCount64 segs.[i].mask
        total
    member internal c.Components = segs.Components
    member c.Clear() =
        segs.Clear()
    member c.Commit() =
        segs.Commit()
    member c.Contains(id) =
        let struct(sid, ci) = idToKey id
        let mask = segs.GetMask sid
        (mask &&& (1UL <<< ci)) <> 0UL
    member c.Get(id : 'c) =
        let struct(sid, ci) = idToKey id
        let seg = segs.Get(sid)
        if seg.mask &&& (1UL <<< ci) = 0UL then 
            failwithf "Cannot get %s component %d in segment %A" (typeToString typeof<'a>) ci sid
        seg.data.[ci]
    member c.Set(id, value) =
        let struct(sid, ci) = idToKey id
        let seg = segs.Get(sid)
        if seg.mask &&& (1UL <<< ci) = 0UL then 
            failwithf "Cannot set %s component %d in segment %A" (typeToString typeof<'a>) ci sid
        seg.data.[ci] <- value
    member c.Get(id, fallback) =
        let struct(sid, ci) = idToKey id
        match segs.TryFind(sid) with
        | false, _ -> fallback
        | true, si ->
            let s = segs.[si]
            if s.mask &&& (1UL <<< ci) = 0UL then fallback
            else s.data.[ci]                
    member c.Add(id, value) =
        let struct(sid, ci) = idToKey id
        let data = segs.Add(sid, 1UL <<< ci)
        data.[ci] <- value
    /// Removes single component
    member c.Remove(id) =
        let struct(sid, ci) = idToKey id
        segs.Remove(sid, 1UL <<< ci)

type IComponentStore<'k, 'c 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality
    and 'c :> IComparable<'c>> =
    abstract member Get<'b> : unit -> Components<'k, 'c, 'b>

type ComponentStore<'k, 'c 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality
    and 'c :> IComparable<'c>>
    (
        segs : SegmentStore<'k>,
        idToKey : 'c -> struct('k * int)
    ) =
    new(idToKey) =
        ComponentStore(SegmentStore(), idToKey)
    member c.Segments = segs
    member c.GetSegments<'a>() = 
        segs.GetSegments<'a>()
    member c.Get<'a>() =
        Components(segs.GetSegments<'a>(), idToKey)
    member c.Clear() =
        segs.Clear()
    member c.Remove(sid, mask) =
        segs.Remove(sid, mask)
    member c.Handle(id, handler) =        
        let struct(sid, ci) = idToKey id
        let mask = 1UL <<< ci
        segs.Handle(sid, mask, handler)
    /// Removes all components associated with a given ID
    member c.Destroy(id) =
        let struct(sid, ci) = idToKey id
        c.Remove(sid, 1UL <<< ci)
    member c.Commit() =
        segs.Commit()
    interface IComponentStore<'k, 'c> with
        member c.Get<'a>() = 
            c.Get<'a>()
    interface ISegmentStore<'k> with
        member c.GetSegments<'a>() = 
            segs.GetSegments<'a>()
    override c.ToString() =
        segs.ToString()
