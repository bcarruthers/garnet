namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open Garnet.Composition.Comparisons

type DisposableReference<'a when 'a :> IDisposable>(init : 'a) =
    let mutable current = init
    member c.Value = current
    member c.Set(create) =
        current.Dispose()
        current <- create()
    member c.Dispose() =
        current.Dispose()
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()

/// Wrapper over resource lookup with default types for ECS
type Container() =
    let reg = Registry()
    let channels = reg.GetValue<Channels>()
    let scheduler = reg.GetValue<CoroutineScheduler>()
    let segments = reg.GetValue<SegmentStore<int>>()
    let outbox = reg.GetValue<Outbox>()
    let eidPools = reg.GetValue<EidPools>()
    let resources = reg.GetValue<ResourceCache>()
    let components = ComponentStore(segments)
    let eids = segments.GetSegments<Eid>()
    member c.Get<'a>() = components.Get<'a>()
    member c.GetSegments<'a>() = segments.GetSegments<'a>()
    member c.GetChannel<'a>() = channels.GetChannel<'a>()
    member c.SetFactory(x) = reg.SetFactory(x)
    member c.SetValue(x) = reg.SetValue(x)
    member c.GetValue() = &reg.GetValue()
    member c.TryGetValue<'a>([<Out>] value : byref<'a>) =
        reg.TryGetValue<'a>(&value)
    member c.AddResource<'a>(key, resource) =
        resources.AddResource<'a>(key, resource)
    member c.TryGetResource<'a>(key, [<Out>] value : byref<'a>) =
        resources.TryGetResource(key, &value)
    member c.LoadResource<'a> key =
        resources.LoadResource<'a>(key)
    member c.IterValues(param, handler) =
        reg.IterValues(param, handler)
    member c.GetAddresses() =
        outbox.Current.Addresses
    member internal c.Clear() =
        channels.Clear()
        components.Clear()
        eidPools.Clear()
        scheduler.Clear()
    member c.Commit() =
        // Order of commits doesn't matter since we're just moving data
        // into committed state and not calling any handlers.
        channels.Commit()
        // Copy removals from eids to other component types first,
        // then apply eid changes to partition cache, then after all
        // this commit all resulting component changes.
        segments.ApplyRemovalsFrom(eids)   
        eidPools.Apply(eids)
        components.Commit()
    /// Returns true if events were handled
    member private c.DispatchOnce() = 
        c.Commit()
        channels.Publish()
    member private c.DispatchAll() = 
        while c.DispatchOnce() do ()
    member private c.RunOnce() = 
        c.Commit()
        scheduler.RunOnce()
    member c.Run() = 
        c.DispatchAll()
        while c.RunOnce() do
            c.DispatchAll()
    member c.Contains(eid : Eid) =
        let sid = eid.SegmentIndex
        let ci = eid.ComponentIndex
        let mask = eids.GetMask sid
        (mask &&& (1UL <<< ci)) <> 0UL
    member c.Get(eid) = { 
        Id = eid
        Components = components 
        }
    member internal c.CreateEid(partition) =
        let eid = eidPools.Next(partition)
        let sid = eid.SegmentIndex
        let ci = eid.ComponentIndex
        let data = eids.Add(sid, 1UL <<< ci)
        data.[ci] <- eid
        eid
    member c.Handle(param, handler : ISegmentListHandler<_, int>) =
        segments.Handle(param, handler)
    member c.Handle(param, sid, mask, handler) =        
        segments.Handle(param, sid, mask, handler)
    member c.Handle(param, id, handler) =
        components.Handle(param, id, handler)
    member c.Destroy(eid : Eid) =
        // Only removing from eids and relying on commit to remove
        // other components.
        let sid = eid.SegmentIndex
        let ci = eid.ComponentIndex
        eids.Remove(sid, 1UL <<< ci)
    member c.Step deltaTime =
        scheduler.Step deltaTime
    member c.Start coroutine = 
        scheduler.Schedule coroutine
    member c.SetPublisher pub =
        channels.SetPublisher pub
    member c.SetPublisher pub =
        c.SetPublisher (ValueSome pub)
    member c.UnsubscribeAll() =
        channels.Clear()
    interface IRegistry with
        member c.SetFactory(x) = c.SetFactory(x)
        member c.SetValue(x) = c.SetValue(x)
        member c.GetValue() = &c.GetValue()
        member c.TryGetValue<'a>([<Out>] value) = c.TryGetValue<'a>(&value)
        member c.IterValues(param, handler) =
            c.IterValues(param, handler)
    interface IResourceCache with
        member c.TryGetResource<'a>(key, [<Out>] value : byref<'a>) =
            c.TryGetResource<'a>(key, &value)
        member c.LoadResource<'a> key = c.LoadResource<'a>(key)
        member c.AddResource(key, resource) = c.AddResource(key, resource)
    interface IChannels with
        member c.GetChannel<'a>() = channels.GetChannel<'a>()
    interface IComponentStore<int, Eid, EidSegmentKeyMapper> with
        member c.Get<'a>() = 
            components.Get<'a>()
    interface ISegmentStore<int> with
        member c.Handle(param, handler) =      
            c.Handle(param, handler)
        member c.Handle(param, sid, mask, handler) =        
            c.Handle(param, sid, mask, handler)
        member c.GetSegments<'a>() = 
            segments.GetSegments<'a>()
    member c.BeginSend() =
        outbox.BeginSend()
    interface IOutbox with
        member c.BeginSend() =
            outbox.BeginSend()
    member c.Receive (e : Envelope<_>) =
        // assign outbox for duration of call
        use s = outbox.Push e
        let channel = c.GetChannel<'a>()
        channel.PublishAll e.Message
        c.Run()
    interface IInbox with
        member c.Receive e =
            c.Receive(e)
    override c.ToString() = 
        reg.ToString()

type SystemRegistry() =
    let dict = Dictionary<string, DisposableReference<IDisposable>>()
    member private c.GetSubscription(name) =
        Disposable.Create(fun () -> 
            dict.[name].Dispose()
            dict.Remove(name) |> ignore)
    member c.Contains(name) =
        dict.ContainsKey(name)
    member c.Add(name, create) =
        match dict.TryGetValue(name) with
        | true, entry -> entry.Set(create)
        | false, _ -> 
            let entry = new DisposableReference<_>(create())
            dict.Add(name, entry)
        c.GetSubscription(name)
    member c.Dispose() =
        for entry in dict.Values do
            entry.Dispose()
        dict.Clear()
    member c.ToString(writer : IStringBlockWriter) =
        if writer.BeginList("Systems", dict.Count) then
            for name in Seq.sort dict.Keys do
                writer.Write(name)
            writer.End()
    override c.ToString() =
        StringBlockWriter.Format(c.ToString)

type Container with
    member c.GetSourceId() =
        c.GetAddresses().SourceId

    member c.GetDestinationId() =
        c.GetAddresses().DestinationId

    member c.Create(partition) =
        let eid = c.CreateEid(partition)
        c.Get eid

    member c.Create() = c.Create(0)

    member c.Create(eid : Eid) =
        c.Get(eid).With(eid)

    member c.DestroyAll() =
        c.GetSegments<Eid>().RemoveAll()

    member c.Run(msg) = 
        c.Send(msg)
        c.Run()

    member c.BeginRespond() =
        c.BeginSend(c.GetAddresses().SourceId)

    member c.Respond(msg) =
        c.Send(c.GetAddresses().SourceId, msg)

    member c.AddSystems(systems) =
        systems
        |> Seq.map (fun sys -> sys c)
        |> Disposable.Create 

    member c.AddSystem(name : string, register : Container -> IDisposable) =
        let reg = c.GetValue<SystemRegistry>()
        reg.Add(name, fun () -> register c)

    member c.AddSystem(actorId, actorOutbox, register : Container -> IDisposable) =
        let outbox = c.GetValue<Outbox>()
        use s = outbox.Push {
            Outbox = actorOutbox
            SourceId = ActorId.Undefined
            DestinationId = actorId
            Message = ()
            }
        let sub = register c
        c.Commit()
        sub

    /// Stores a state value in registry and calls registration when state events occur.
    /// This is useful for allowing a container to have multiple states with their own
    /// subscriptions and transition logic.
    member c.AddStateMachine<'a>(initState, registerState) =
        let state = new DisposableReference<IDisposable>(Disposable.Null)
        let setState e =
            c.SetValue<'a>(e)
            // Register subscriptions specific to new state, replacing prior
            state.Set(fun () -> registerState e c)
        setState initState
        Disposable.Create [
            // when state message arrives
            c.On<'a> setState
            state :> IDisposable
            ]        
    
    static member Create(register : Container -> IDisposable) =
        let c = Container()
        register c |> ignore
        c.Commit()
        c

/// Allows container creation in actor thread instead of main thread
type LazyContainerInbox(actorId, register) =
    let container = Container()
    let mutable sub = Disposable.Null
    let mutable isCreated = false
    interface IInbox with
        member c.Receive(e) =
            if not isCreated then                
                sub <- container.AddSystem(actorId, e.Outbox, register)
                isCreated <- true
            container.Receive(e)
    member c.Dispose() = sub.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()
