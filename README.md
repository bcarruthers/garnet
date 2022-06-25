# Garnet

[![Build status](https://ci.appveyor.com/api/projects/status/g82kak7btxp48rnd?svg=true)](https://ci.appveyor.com/project/bcarruthers/garnet)

[NuGet package](https://www.nuget.org/packages/Garnet/)

Garnet is a lightweight game composition library for F# with entity-component-system (ECS) and actor-like messaging features.

```fsharp
open Garnet.Composition

// events
[<Struct>] type Update = { dt : float32 }

// components
[<Struct>] type Position = { x : float32; y : float32 }
[<Struct>] type Velocity = { vx : float32; vy : float32 }

// create a world
let world = Container()

// register a system that updates position
let system =
    world.On<Update> <| fun e ->
        for r in world.Query<Position, Velocity>() do
            let p = &r.Value1
            let v = r.Value2
            p <- {
                x = p.x + v.vx * e.dt
                y = p.y + v.vy * e.dt
                }

// add an entity to world
let entity = 
    world.Create()
        .With({ x = 10.0f; y = 5.0f })
        .With({ vx = 1.0f; vy = 2.0f })

// run updates and print world state
for i = 1 to 10 do
    world.Run <| { dt = 0.1f }
    printfn "%O\n\n%O\n\n" world entity
```

## Table of contents
* Introduction
    * [Getting started](#gettingstarted)
    * [Background](#background)
    * [Goals](#goals)
* Guide
    * [Containers](#containers)
    * [Entities](#entities)
    * [Components](#components)
    * [Systems](#systems)
    * [Actors](#actors)
    * [Integration](#integration)
* [FAQ](#faq)
* [License](#license)
* [Maintainers](#maintainers)

## Getting started

1. Create either a .NET Framework, Core, or 6.0+ application.
2. Reference the [Garnet NuGet package](https://www.nuget.org/packages/Garnet/). 
3. For sample code, see unit tests or [sample projects](https://github.com/bcarruthers/garnet/tree/master/samples).

## Background

ECS is a common architecture for games, often contrasted with OOP inheritance. It focuses on separation of data and behavior and is typically implemented in a data-oriented way to achieve high performance. It's similar to a database, where component tables are related using a common entity ID, allowing systems to query and iterate over entities with specific combinations of components present. EC (entity-component) is a related approach that attaches behavior to components and avoids systems.

While ECS focuses on managing shared state, the actor model isolates state into separate actors which communicate only through messages. Actors can send and receive messages, change their behavior as a result of messages, and create new actors. This approach offers scaleability and an abstraction layer over message delivery, and games can use it at a high level to model independent processes, worlds, or agents.

## Goals

- **Lightweight**: Garnet is essentially a simplified in-memory database and messaging system suitable for games. No inheritance, attributes, or interface implementations are required in your code. It's more of a library than a framework or engine, and most of your code shouldn't depend on it.

- **Fast**: Garbage collection spikes can cause dropped frames and inconsistent performance, so Garnet minimizes allocations and helps library users do so too. Component storage is data-oriented for fast iteration.

- **Minimal**: The core library focuses on events, scheduling, and storage, and anything game-specific like physics, rendering, or update loops should be implemented separately.

## Containers

ECS containers provide a useful bundle of functionality for working with shared game state, including event handling, component storage, entity ID generation, coroutine scheduling, and resource resolution.

```fsharp
// create a container/world
let c = Container()
```

### Registry

Containers store single instances of types such as component lists, ID pools, settings, and any other arbitrary type. You can access instances by type, with optional lazy resolution. This is the service locator (anti-)pattern.

```fsharp
// option 1: add specific instance
c.SetValue(defaultWorldSettings)
// option 2: register a factory
c.SetFactory(fun () -> defaultWorldSettings)
// resolve type
let settings = c.GetValue<WorldSettings>()
```

This works for value types as well:

```fsharp
c.SetValue { zoomLevel = 0.5f }
let zoom = c.GetValue<Zoom>>()
```

### Object pooling

Avoiding GC generally amounts to use of structs, pooling, and avoiding closures. Almost all objects are either pooled within a container or on the stack, so there's little or no GC impact or allocation once maximum load is reached. If needed, warming up or provisioning buffers ahead of time is possible for avoiding GC entirely during gameplay.

### Commits

Certain operations on containers, such as sending events or adding/removing components, are staged until a commit occurs, allowing any running event handlers to observe the original state. Commits occur automatically after all subscribers have completed handling a list of events, so you typically shouldn't need to explicitly commit.

```fsharp
// create an entity
let e = c.Create().With("test")
// not yet visible
c.Commit()
// now visible
```

## Entities

An entity is any identifiable thing in your game which you can attach components to. At minimum, an entity consists only of an entity ID.

### Entity ID

Entity IDs are 32 bits and stored in a component list. This means they can be accessed and iterated over like any other component type without special handling. IDs use a special Eid type rather than a raw int32, which offers better type safety but means you need a direct dependency on Garnet if you want to define types with an Eid (or you can manage converting to your own ID type if this is an issue). 

```fsharp
let entity = c.Create()
printfn "%A" entity.id
```

### Generations

A portion of an ID is dedicated to its generation number. The purpose of a generation is to avoid reusing IDs while still allowing buffer slots to be reused, keeping components stored as densely as possible.

### Partitioning

Component storage could become inefficient if it grows too sparse (i.e. the average number of occupied elements per segment becomes low). If this is a concern (or you just want to organize your entities), you can optionally use partitions to specify a high bit mask in ID generation. For example, if ship and bullet entities shared the same ID space, they may become mixed over time and the ship components would become sparse. Instead, with separate partitions, both entities would remain dense. Note: this will likely be replaced with groups in the future.

### Generic storage

Storage should work well for both sequential and sparse data and support generic key types. Entity IDs are typically used as keys, but other types like grid location should be possible as well.

### Inspecting

You can print the components of an entity at any time, which is useful in REPL scenarios as an alternative to using a debugger.

```fsharp
printfn "%s" <| c.Get(Eid 64).ToString()
```
```
Entity 0x40: 20 bytes
Eid 0x40
Loc {x = 10;
 y = 2;}
UnitType Archer
UnitSize {unitSize = 5;}
```

## Components

Components are any arbitrary data type associated with an entity. Combined with systems that operate on them, components provide a way to specify behavior or capabilities of entities.

### Data types

Components should ideally be pure data rather than classes with behavior and dependencies. They should typically be structs to avoid jumping around in memory or incurring allocations and garbage collection. Structs should almost always be immutable, but mutable structs (with their gotchas) are possible too.

```fsharp
[<Struct>] type Position = { x : float32; y : float32 }
[<Struct>] type Velocity = { vx : float32; vy : float32 }

// create an entity and add two components to it
let entity = 
    c.Create()
        .With({ x = 10.0f; y = 5.0f })
        .With({ vx = 1.0f; vy = 2.0f })
```

### Storage

Components are stored in 64-element segments with a mask, ordered by ID. This provides CPU-friendly iteration over densely stored data while retaining some benefits of sparse storage. Some ECS implementations provide a variety of specialized data structures, but Garnet attempts a middle ground that works moderately well for both sequential entity IDs and sparse keys such as grid locations.

Only a single component of a type is allowed per entity, but there is no hard limit on the total number of different component types used (i.e. there is no fixed-size mask defining which components an entity has).

### Iteration

You can iterate over entities with specific combinations of components using queries. In this way you could define a system that updates all entities with a position and velocity, and iteration would skip over any entities with only a position and not velocity.

```fsharp
let healthSub =
    c.On<DestroyZeroHealth> <| fun e ->
        for r in c.Query<Eid, Position, Health>() do
            let h = r.Value3
            if h.hp <= 0 then
                let eid = r.Value1
                c.Destroy(eid)
```

For batch operations or to improve performance further, you can iterate over segments:

```fsharp
let healthSub =
    c.On<DestroyZeroHealth> <| fun e ->
        for seg, eids, _, hs in c.QuerySegments<Eid, Position, Health>() do
            for i in seg do
                let h = hs.[i]
                if h.hp <= 0 then
                    let eid = eids.[i]
                    c.Destroy(eid)
```

Note that writes to existing components during iteration occur immediately, unlike adding or removing components.

### Adding

Additions are deferred until a commit occurs, so any code dependent on those operations completing needs to be implemented as a coroutine.

```fsharp
let e = c.Get(Eid 100)
e.Add<Position> { x = 1.0f; y = 2.0f }
// change not yet visible
```

### Removing

Like additions, removals are also deferred until commit. Note that you can repeatedly add and remove components for the same entity ID before a commit if needed.

```fsharp
e.Remove<Velocity>()
// change not yet visible
```

### Updating

Unlike additions and removals, updating/replacing an existing component can be done directly at the risk of affecting subsequent subscribers. This way is convenient if the update operation is commutative or there are no other subscribers writing to the same component type during the same event. You can alternately just use addition if you don't know whether a component is already present.

```fsharp
let e = c.Get(Eid 100)
e.Set<Position> { x = 1.0f; y = 2.0f }
// change immediately visible
```

### Markers

You can define empty types for use as flags or markers, in which case only 64-bit masks need to be stored per segment. Markers are an efficient way to define static groups for querying.

```fsharp
type PowerupMarker = struct end
```

## Systems

Systems are essentially event subscribers with an optional name. System event handlers often iterate over entities, such as updating position based on velocity, but they can do any other kind of processing too.

```fsharp
module MovementSystem =     
    // separate methods as needed
    let registerUpdate (c : Container) =
        c.On<UpdatePositions> <| fun e ->
            printfn "%A" e

    // combine all together
    let register (c : Container) =
        Disposable.Create [
            registerUpdate c
            ]
```

Alternately, you can define systems as extension methods. This way is more OOP-centric and avoids some redundancy in declarations.

```fsharp
[<AutoOpen>]
module MovementSystem =
    type Container with
        member c.AddMovementUpdate() =
            c.On<UpdatePositions> <| fun e ->
                printfn "%A" e
                
        member c.AddMovementSystems() =  
            Disposable.Create [
                c.AddMovementUpdate()
                ]
```

### Execution

When any code creates or modifies entities, sends events, or starts coroutines, it's only staging those things. To actually set all of it into motion, you need to run the container, which would typically happen as part of the game loop. Each time you run the container, it commits all changes, publishes events, and advances coroutines, repeating this process until no work remains to do. This means you should avoid introducing cycles like two systems responding to each other unless they are part of a timed coroutine.

```fsharp
// run the container
c.Process()
```

### Events

Like components, you can use any arbitrary type for an event, but structs are generally preferable to avoid GC. When events are published, subscribers receive batches of events with no guaranteed ordering among the subscribers or event types. Any additional events raised during event handling are run after all the original event handlers complete, thereby avoiding any possibility of reentrancy but complicating synchronous behavior. 

```fsharp
[<Struct>] type UpdateTime = { dt : float32 }

// call sub.Dispose() to unsubscribe
let sub =
    c.On<UpdateTime> <| fun e ->
        // [do update here]
        printfn "%A" e

// send event        
c.Send { dt = 0.1f }
```

Events intentionally decouple publishers and subscribers, and since dispatching events is typically not synchronous within the ECS, it can be difficult to trace the source of events when something goes wrong (no callstack).

### Coroutines

Coroutines allow capturing state and continuing processing for longer than the handling of a single event. They are implemented as sequences and can be used to achieve synchronous behavior despite the asynchronous nature of event handling. This is one of the few parts of the code which incurs allocation.

Coroutines run until they encounter a yield statement, which can tell the coroutine scheduler to either wait for a time duration or to wait until all nested processing has completed. Nested processing refers to any coroutines created as a result of events sent by the current coroutine, allowing a stack-like flow and ordering of events.

```fsharp
let system =
    c.On<Msg> <| fun e ->
        printf "2 "

// start a coroutine
c.Start <| seq {
    printf "1 "
    // send message and defer execution until all messages and
    // coroutines created as a result of this have completed
    c.Send <| Msg()
    yield Wait.All
    printf "3 "
    }

// run until completion
// output: 1 2 3
c.Process()
```

Time-based coroutines are useful for animations or delayed effects. You can use any unit of time as long as it's consistent.

```fsharp
// start a coroutine
c.Start <| seq {
    for i = 1 to 5 do
        printf "[%d] " i
        // yield execution until time units pass
        yield Wait.time 3L
    }

// run update loop
// output: [1] 1 2 3 [2] 4 5 6 [3] 7 8 9
for i = 1 to 9 do
    // increment time units and run pending coroutines
    c.Step 1L
    c.Process()
    printf "%d " i
```

### Multithreading

It's often useful to run physics in parallel with other processing that doesn't depend on its output, but the event system currently has no built-in features to facilitate multiple threads reading or writing. Instead, you can use the actor system for parallel execution at a higher level, or you can implement your own multithreading at the container level.

### Event ordering

For systems that subscribe to the same event and access the same resources or components, you need to consider whether one is dependent on the other and should run first.

One way to guarantee ordering is to define individual sub-events for the systems and publish those events in the desired order as part of a coroutine started from the original event (with waits following each event to ensure all subscribers are run before proceeding).

```fsharp
// events
type Update = struct end
type UpdatePhysicsBodies = struct end
type UpdateHashSpace = struct end

// systems
let updateSystem =
    c.On<Update> <| fun e -> 
        c.Start <| seq {
            // sending and suspending execution to 
            // achieve ordering of sub-updates
            c.Send <| UpdatePhysicsBodies()
            yield Wait.All
            c.Send <| UpdateHashSpace()
            yield Wait.All
        }
let system1 = 
    c.On<UpdatePhysicsBodies> <| fun e ->
        // [update positions]
        printfn "%A" e
let system2 = 
    c.On<UpdateHashSpace> <| fun e ->
        // [update hash space from positions]
        printfn "%A" e
```

### Composing systems

Since systems are just named event subscriptions, you can compose them into larger systems. This allows for bundling related functionality.

```fsharp
module CoreSystems =        
    let register (c : Container) =
        Disposable.Create [
            MovementSystem.register c
            HashSpaceSystem.register c
        ]
```

## Actors

While ECS containers provide a simple and fast means of storing and updating shared memory state using a single thread, actors share no common state and communicate only through messages, making them suitable for parallel processing.

### Definitions

Actors are identified by an actor ID. They are statically defined and created on demand when a message is sent to a nonexistent actor ID. At that point, an actor consisting of a message handler is created based on any definitions registered in the actor system that match the actor ID. It's closer to a mailbox processor than a complete actor model since these actors can't dynamically create arbitrary actors or control actor lifetimes.

```fsharp
// message types
type Ping = struct end
type Pong = struct end

// actor definitions
let a = new ActorSystem()
a.Register(ActorId 1, fun (c : Container) ->
    c.On<Ping> <| fun e -> 
        printf "ping "
        c.Respond(Pong())
    )
a.Register(ActorId 2, fun (c : Container) ->
    c.On<Pong> <| fun e -> 
        printf "pong "
    )
    
// send a message and run until all complete
// output: ping pong
a.Send(ActorId 1, Ping(), sourceId = ActorId 2)
a.ProcessAll()
```

### Actor messages versus container events

Containers already have their own internal event system, but the semantics are a bit different from actors because container events are always stored in separate channels by event type rather than a single serialized channel for all actor message types. The use of separate channels within containers allows for efficient batch processing in cases where event types have no ordering dependencies, but ordering by default is preferable in many other cases involving actors.

### Wrapping containers

It's useful to wrap a container within an actor, where incoming messages to the actor automatically dispatched to the container, and systems within the container have access to an outbox for sending messages to other actors. This approach allows keeping isolated worlds, such as a subset of world state for AI forces or UI state.

### Replay debugging

If you can write logic where your game state is fully determined by the sequence of incoming messages, you can log these messages and replay them to diagnose bugs. This works best if you can isolate the problem to a single actor, such as observing incorrect state or incorrect outgoing messages given a correct input sequence.

### Message ordering

Messages sent from one actor to another are guaranteed to arrive in the order they were sent, but they may be interleaved with messages arriving from other actors. In general, multiple actors and parallelism can introduce complexity similar to the use of microservices, which address scaleability but can introduce race conditions and challenges in synchronization.

### Multithreading

You can designate actors to run on either the main thread (for UI if needed) or a background thread. Actors run when a batch of messages is delivered, resembling task-based parallelism. In addition to running designated actors, the main thread also delivers messages among actors, although this could change in the future if it becomes a bottleneck. Background actors currently run using a fixed pool of worker threads.

## Integration

How does Garnet integrate with frameworks or engines like Unity, MonoGame, or Veldrid? You have a few options depending on how much you want to depend on Garnet, your chosen framework, and your own code. This approach also works for integrating narrower libraries like physics or networking.

See [sample projects](https://github.com/bcarruthers/garnet/tree/master/samples) for integration with Veldrid and OpenAL.

### Abstracting framework calls

When you need to call the framework (e.g. MonoGame) from your code, you can choose to insulate your code from the framework with an abstraction layer. This reduces your dependency on it, but it takes more effort and may result in less power to use framework-specific features and more overhead in marshaling data. If you decide to abstract, you have several options for defining the abstraction layer:

- **Services**: Register an interface for a subsystem and provide an implemention for the specific framework, e.g. *ISpriteRenderer* with *MonoGameSpriteRenderer*. This makes sense if you want synchronous calls or an explicit interface.

- **Events**: Define interface event types and framework-specific systems which subscribe to them, e.g. a sprite rendering system subscribing to *DrawSprite* events. This way is more decoupled, but the interface may not be so clear.

- **Components**: Define interface component types and implement framework-specific systems which iterate over them, e.g. a sprite rendering system which iterates over entities with a *Sprite* component. 

### Sending framework events

For the reverse direction, when you want the framework to call your code, you can simply send interface event types and run the container or actors.

```fsharp
type Game() =
    // ...
    let world = Container()
    // [configure container here]
    override c.Update gt = 
        world.Run { deltaTime = gt.ElapsedGameTime }
    override c.Draw gameTime = 
        world.Run <| Draw()
```

## FAQ

- **Why F#?** F# offers conciseness, functional-first defaults like immutability, an algebraic type system, interactive code editing, and pragmatic support for other paradigms like OOP. Strong type safety makes it more likely that code is correct, which is especially helpful for tweaking game code that changes frequently enough to make unit testing impractical.

- **What about performance?** Functional code often involves allocation, which sometimes conflicts with the goal of consistent performance when garbage collection occurs. A goal of this library is to reduce the effort in writing code that minimizes allocation. But for simple games, this is likely a non-issue and you should start with idiomatic code.

- **Why use ECS over MVU?** You probably shouldn't start with ECS for a simple game, at least not when prototyping, unless you already have a good understanding of where it might be beneficial. MVU avoids a lot of complexity and has stronger type safety and immutability guarantees than ECS, but you may encounter issues if your project has demanding performance requirements or needs more flexibility than it allows. 

## License
This project is licensed under the [MIT license](https://github.com/bcarruthers/garnet/blob/master/LICENSE).

## Maintainer(s)

- [@bcarruthers](https://github.com/bcarruthers)