﻿namespace Garnet.Composition

open System
open System.Runtime.InteropServices
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
            failwithf "Cannot get %s %s" (typeToString typeof<'a>) (id.ToString())
        seg.data.[ci]
    member c.Set(id, value) =
        let struct(sid, ci) = idToKey id
        let seg = segs.Get(sid)
        if seg.mask &&& (1UL <<< ci) = 0UL then 
            failwithf "Cannot set %s %s" (typeToString typeof<'a>) (id.ToString())
        seg.data.[ci] <- value
    member c.TryGet(id, [<Out>] value : byref<_>)=
        let struct(sid, ci) = idToKey id
        match segs.TryFind(sid) with
        | false, _ ->
            value <- Unchecked.defaultof<_>
            false
        | true, si ->
            let s = segs.[si]
            if s.mask &&& (1UL <<< ci) = 0UL then 
                value <- Unchecked.defaultof<_>
                false
            else 
                value <- s.data.[ci]                
                true
    member c.GetOrDefault(id, fallback) =
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
    override c.ToString() =
        segs.ToString()

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
    member c.Handle(param, id, handler) =        
        let struct(sid, ci) = idToKey id
        let mask = 1UL <<< ci
        segs.Handle(param, sid, mask, handler)
    member c.Handle(param, handler) =      
        segs.Handle(param, handler)
    /// Removes ID component and assumes other components
    /// will be cleaned up in commit
    member c.Destroy(id) =
        let struct(sid, ci) = idToKey id
        segs.GetSegments<'c>().Remove(sid, 1UL <<< ci)
    member c.Commit() =
        segs.Commit()
    interface IComponentStore<'k, 'c> with
        member c.Get<'a>() = 
            c.Get<'a>()
    interface ISegmentStore<'k> with
        member c.Handle(param, handler) =      
            c.Handle(param, handler)
        member c.GetSegments<'a>() = 
            segs.GetSegments<'a>()
    override c.ToString() =
        segs.ToString()

[<Struct>]
type Entity<'k, 'c
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality
    and 'c :> IComparable<'c>> = {
    id : 'c
    container : ComponentStore<'k, 'c>
    } with
    member c.Add x = c.container.Get<_>().Add(c.id, x)
    member c.Set x = c.container.Get<_>().Set(c.id, x)
    member c.Remove<'a>() = c.container.Get<'a>().Remove(c.id)
    member c.Get<'a>() = c.container.Get<'a>().Get(c.id)    
    member c.TryGet<'a>([<Out>] value : byref<_>) = c.container.Get<'a>().TryGet(c.id, &value)
    member c.GetOrDefault<'a>(fallback) = c.container.Get<'a>().GetOrDefault(c.id, fallback)
    member c.Contains<'a>() = c.container.Get<'a>().Contains(c.id)
    member c.Destroy() = c.container.Destroy(c.id)
    member c.With x = c.Add x; c
    member c.Without<'a>() = c.Remove<'a>(); c
    override c.ToString() = 
        let printer = PrintHandler<'k>(UInt64.MaxValue)
        c.container.Handle((), c.id, printer)
        "Entity " + c.id.ToString() + ": " + printer.ToString()

type ComponentStore<'k, 'c
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality
    and 'c :> IComparable<'c>> with
    member c.Get(id) = {
        id = id
        container = c 
        }
