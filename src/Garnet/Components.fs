namespace Garnet.Composition

open System
open System.Runtime.InteropServices
open Garnet.Comparisons
open Garnet.Formatting

type ISegmentKeyMapper<'k, 'c> = 
    abstract GetSegmentKey : 'c -> 'k
    abstract GetComponentIndex : 'c -> int

module IdSegmentMapper =
    let inline getSegmentKey<'k, 'c, 'm when 'm : struct and 'm :> ISegmentKeyMapper<'k, 'c>> (id : 'c) =
        Unchecked.defaultof<'m>.GetSegmentKey(id)

    let inline getComponentIndex<'k, 'c, 'm when 'm : struct and 'm :> ISegmentKeyMapper<'k, 'c>> (id : 'c) =
        Unchecked.defaultof<'m>.GetComponentIndex(id)

/// Adds convenience methods to access individual components
[<Struct>]
type Components<'k, 'c, 'm, 'a 
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality 
        and 'c :> IComparable<'c>
        and 'm : struct and 'm :> ISegmentKeyMapper<'k, 'c>>(segments : Segments<'k, 'a>) =
    member c.Segments = segments
    member c.Count = segments.GetComponentCount()
    member internal c.Components = segments.Components
    member c.Clear() =
        segments.Clear()
    member c.Commit() =
        segments.Commit()
    member c.Contains(id) =
        let sid = IdSegmentMapper.getSegmentKey<'k, 'c, 'm> id
        let ci = IdSegmentMapper.getComponentIndex<'k, 'c, 'm> id
        let mask = segments.GetMask sid
        (mask &&& (1UL <<< ci)) <> 0UL
    member c.Get(id : 'c) =
        let sid = IdSegmentMapper.getSegmentKey<'k, 'c, 'm> id
        let ci = IdSegmentMapper.getComponentIndex<'k, 'c, 'm> id
        let seg = segments.Get(sid)
        if seg.Mask &&& (1UL <<< ci) = 0UL then 
            failwithf "Cannot get %s %s" (Format.typeToString typeof<'a>) (id.ToString())
        seg.Data.[ci]
    member c.Set(id, value) =
        let sid = IdSegmentMapper.getSegmentKey<'k, 'c, 'm> id
        let ci = IdSegmentMapper.getComponentIndex<'k, 'c, 'm> id
        let seg = segments.Get(sid)
        if seg.Mask &&& (1UL <<< ci) = 0UL then 
            failwithf "Cannot set %s %s" (Format.typeToString typeof<'a>) (id.ToString())
        seg.Data.[ci] <- value
    member c.TryGet(id, [<Out>] value : byref<_>)=        
        let sid = IdSegmentMapper.getSegmentKey<'k, 'c, 'm> id
        let ci = IdSegmentMapper.getComponentIndex<'k, 'c, 'm> id
        match segments.TryFind(sid) with
        | false, _ ->
            value <- Unchecked.defaultof<_>
            false
        | true, si ->
            let s = segments.[si]
            if s.Mask &&& (1UL <<< ci) = 0UL then 
                value <- Unchecked.defaultof<_>
                false
            else 
                value <- s.Data.[ci]                
                true
    member c.GetOrDefault(id, fallback) =        
        let sid = IdSegmentMapper.getSegmentKey<'k, 'c, 'm> id
        let ci = IdSegmentMapper.getComponentIndex<'k, 'c, 'm> id
        match segments.TryFind(sid) with
        | false, _ -> fallback
        | true, si ->
            let s = segments.[si]
            if s.Mask &&& (1UL <<< ci) = 0UL then fallback
            else s.Data.[ci]                
    member c.Copy(srcId, destId) =
        let mutable value = Unchecked.defaultof<_>
        if c.TryGet(srcId, &value) 
            then c.Add(destId, value)
            else c.Remove(destId)               
    member c.Add(id, value) =
        let sid = IdSegmentMapper.getSegmentKey<'k, 'c, 'm> id
        let ci = IdSegmentMapper.getComponentIndex<'k, 'c, 'm> id
        let data = segments.Add(sid, 1UL <<< ci)
        data.[ci] <- value
    /// Removes single component
    member c.Remove(id) =
        let sid = IdSegmentMapper.getSegmentKey<'k, 'c, 'm> id
        let ci = IdSegmentMapper.getComponentIndex<'k, 'c, 'm> id
        segments.Remove(sid, 1UL <<< ci)
    override c.ToString() =
        segments.ToString()
    static member Create() = 
        Components<'k, 'c, 'm, 'a>(Segments<'k, 'a>())        

type IComponentStore<'k, 'c, 'm 
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality
    and 'c :> IComparable<'c>
    and 'm : struct and 'm :> ISegmentKeyMapper<'k, 'c>> =
    abstract member Get<'b> : unit -> Components<'k, 'c, 'm, 'b>

type ComponentStore<'k, 'c, 'm 
        when 'k :> IComparable<'k> 
        and 'k :> IEquatable<'k> 
        and 'k : equality
        and 'c :> IComparable<'c>
        and 'm : struct and 'm :> ISegmentKeyMapper<'k, 'c>>(segments : SegmentStore<'k>) =
    new() =
        ComponentStore(SegmentStore())
    member c.Segments = segments
    member c.GetSegments<'a>() = 
        segments.GetSegments<'a>()
    member c.Get<'a>() =
        Components(segments.GetSegments<'a>())
    member c.Clear() =
        segments.Clear()
    member c.Handle(param, id, handler) =        
        let sid = IdSegmentMapper.getSegmentKey<'k, 'c, 'm> id
        let ci = IdSegmentMapper.getComponentIndex<'k, 'c, 'm> id
        let mask = 1UL <<< ci
        segments.Handle(param, sid, mask, handler)
    member c.Handle(param, sid, mask, handler) =        
        segments.Handle(param, sid, mask, handler)
    member c.Handle(param, handler) =      
        segments.Handle(param, handler)
    /// Removes ID component and assumes other components
    /// will be cleaned up in commit
    member c.Destroy(id) =
        let sid = IdSegmentMapper.getSegmentKey<'k, 'c, 'm> id
        let ci = IdSegmentMapper.getComponentIndex<'k, 'c, 'm> id
        segments.GetSegments<'c>().Remove(sid, 1UL <<< ci)
    member c.Commit() =
        segments.Commit()
    interface IComponentStore<'k, 'c, 'm> with
        member c.Get<'a>() = 
            c.Get<'a>()
    interface ISegmentStore<'k> with
        member c.Handle(param, handler) =      
            c.Handle(param, handler)
        member c.Handle(param, sid, mask, handler) =        
            c.Handle(param, sid, mask, handler)
        member c.GetSegments<'a>() = 
            segments.GetSegments<'a>()
    override c.ToString() =
        segments.ToString()

[<Struct>]
type Entity<'k, 'c, 'm
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality
    and 'c :> IComparable<'c>
    and 'm : struct and 'm :> ISegmentKeyMapper<'k, 'c>> = {
    Id : 'c
    Components : ComponentStore<'k, 'c, 'm>
    } with
    member c.Add x = c.Components.Get<_>().Add(c.Id, x)
    member c.Set x = c.Components.Get<_>().Set(c.Id, x)
    member c.Remove<'a>() = c.Components.Get<'a>().Remove(c.Id)
    member c.Get<'a>() = c.Components.Get<'a>().Get(c.Id)    
    member c.CopyTo<'a> destId = c.Components.Get<'a>().Copy(c.Id, destId)    
    member c.TryGet<'a>([<Out>] value : byref<_>) = c.Components.Get<'a>().TryGet(c.Id, &value)
    member c.GetOrDefault<'a> fallback = c.Components.Get<'a>().GetOrDefault(c.Id, fallback)
    member c.Contains<'a>() = c.Components.Get<'a>().Contains(c.Id)
    member c.Destroy() = c.Components.Destroy(c.Id)
    member c.With x = c.Add x; c
    member c.Without<'a>() = c.Remove<'a>(); c
    override c.ToString() = 
        let printer = PrintHandler<'k>(UInt64.MaxValue)
        c.Components.Handle((), c.Id, printer)
        "Entity " + c.Id.ToString() + ": " + printer.ToString()

type ComponentStore<'k, 'c, 'm
    when 'k :> IComparable<'k> 
    and 'k :> IEquatable<'k> 
    and 'k : equality
    and 'c :> IComparable<'c>
    and 'm : struct and 'm :> ISegmentKeyMapper<'k, 'c>> with
    member c.Get(id) = {
        Id = id
        Components = c 
        }
