# Garnet

Garnet is a lightweight game composition library for F# with entity-component-system (ECS) and actor-like messaging features.

## Example
```fsharp
open Garnet.Ecs

// events
[<Struct>] type Update = { dt : float32 }

// components
[<Struct>] type Position = { x : float32; y : float32 }
[<Struct>] type Velocity = { vx : float32; vy : float32 }

// create a world
let world = Container()

// register a system that updates position
let system =
    world.On<Update> (
        fun e struct(p : Position, v : Velocity) -> {
            x = p.x + v.vx * e.dt
            y = p.y + v.vy * e.dt
            }
        |> Iter.update2
        |> Iter.over world)

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

## History

Garnet emerged from [Triverse](http://cragwind.com/blog/posts/grid-projectiles/), a 2D game under development where players build and command drone fleets in a large tri-grid world. The game serves as a performance testbed and ensures the library meets the actual needs of at least one moderately complex game.

## Background

ECS is a common architecture for games, often contrasted with OOP inheritance. It focuses on separation of data and behavior and is typically implemented in a data-oriented way to achieve high performance. It's similar to a database, where component tables are related using a common entity ID, allowing systems to query and iterate over entities with specific combinations of components present. EC (entity-component) is a related approach that attaches behavior to components and avoids systems.

While ECS focuses on managing shared state, the actor model isolates state into separate actors which communicate only through messages. Actors can send and receive messages, change their behavior as a result of messages, and create new actors. This approach offers scaleability and an abstraction layer over message delivery, and games can use it at a high level to model independent processes, worlds, or agents.

## Goals

- **Lightweight**: Garnet is essentially a simplified in-memory database and messaging system suitable for games. No inheritance, attributes, or interface implementations are required in your code. It's more of a library than a framework or engine, and most of your code shouldn't depend on it.

- **Consistent performance**: Garbage collection spikes can cause dropped frames and unpredictable performance, so Garnet minimizes allocations and helps library users do so too. This generally amounts to use of structs, pooling, and avoiding closures.

- **Generic storage**: Storage should work well for both sequential and sparse data and support generic key types. Entity IDs are typically used as keys, but other types like grid location should be possible as well.

- **Data-oriented**: Entities and components are stored as a struct of arrays rather than an array of structs, sorted and in blocks of memory, making them suitable for fast iteration and batch operations.

- **Multiple worlds**: In addition to traditional ECS, Garnet provides actor-like messaging for scenarios where multiple ECS worlds are beneficial, such as AI agents or networking.

## Building

1. Install [.NET Core SDK](https://dotnet.microsoft.com/download)
2. Install [FAKE](https://fake.build/fake-gettingstarted.html)
3. Run build.cmd

## Containers

ECS containers provide a useful bundle of functionality for working with shared game state, including event handling, component storage, entity ID generation, coroutine scheduling, and resource resolution.

- **Resources**: Containers store resources such as component lists, ID pools, settings, and any other arbitrary type. You can access resources by type (i.e. service locator pattern) with some limited dependency resolution. This is great for extensibility, but it also introduces new kinds of runtime errors that could not occur with a hardwired approach. Reliance on explicit service locator calls also means dependencies are hidden within implementation code rather than clearly indicated as part of an interface. 

- **Object pooling**: Almost all objects are either pooled within a container or on the stack, so there's little or no GC impact or allocation once maximum load is reached. If needed, warming up or provisioning buffers ahead of time is possible for avoiding GC entirely during gameplay. 

- **Commits**: Certain operations on containers, such as sending events or adding/removing components, are staged until a commit occurs, allowing any running event handlers to observe the original state. Commits occur automatically after all processors have completed handling a list of events.

## Entities

An entity is any identifiable thing in your game which you can attach components to. At minimum, an entity consists only of an entity ID.

- **Entity ID**: Entity IDs are 32 bits and stored in a component list. This means they can be accessed and iterated over like any other component type without special handling. IDs use a special Eid type rather than a raw int32, which offers better type safety but means you need a direct dependency on Garnet if you want to define types with an Eid (or you can manage converting to your own ID type if this is an issue). 

- **Generations**: A portion of an ID is dedicated to its generation number. The purpose of a generation is to avoid reusing IDs while still allowing buffer slots to be reused, keeping components stored as densely as possible.

- **Partitioning**: Component storage could become inefficient if it grows too sparse (i.e. the average number of occupied elements per segment becomes low). If this is a concern (or you just want to organize your entities), you can optionally use partitions to specify a high bit mask in ID generation. For example, if ship and bullet entities shared the same ID space, they may become mixed over time and the ship components would become sparse. Instead, with separate partitions, both entities would remain dense. Note: this will likely be replaced with groups in the future.

## Components

Components are any arbitrary data type associated with an entity.

- **Data types**: Components should ideally be pure data rather than classes with behavior and dependencies. They should typically be structs to avoid jumping around in memory or incurring allocations and garbage collection. Structs should almost always be immutable, but mutable structs (with their gotchas) are possible too.

- **Storage**: Components are stored separately from each other in 64-element segments with a mask, ordered by ID. This provides CPU-friendly iteration over densely stored data while retaining some benefits of sparse storage. Some ECS implementations provide a variety of specialized data structures, but Garnet attempts a middle ground that works moderately well for both sequential entity IDs and sparse keys such as world grid locations.

- **Iteration**: You can iterate over entities with specific combinations of components using joins/queries. In this way you could define a system that updates all entities with a position and velocity, but iteration would skip over any entities with only a position and not velocity. Currently, only a fixed set of predefined joins are provided rather than allowing arbitrary queries.

- **Adding/removing**: Additions or removals are deferred until a commit occurs. Any code dependent on those operations completing needs to be implemented as a coroutine. Note that you can repeatedly add and remove components for the same entity ID before a commit if needed.

- **Updating**: Unlike additions and removals, updating/replacing an existing component can be done directly at the risk of affecting subsequent subscribers. This way is convenient if the changes are incremental/commutative or there are no other subscribers writing to the component type during the same event.

- **Markers**: Empty (0-byte, e.g. "Marker = struct end") flag types can be defined, in which case only 64-bit masks need to be stored per segment. Markers are an efficient way to define static groups for querying.

- **Limits**: Only a single component of a type is allowed per entity, but there is no hard limit on the total number of different component types used (i.e. there is no fixed-size mask defining which components an entity has).

## Systems

Systems typically subscribe to events and iterate over components, such as updating position based on velocity, but they can do any other kind of processing too. You can optionally name systems to allow hot reloading.

- **Events**: Events can be arbitrary types, but preferably structs. Subscribers such as systems receive batches of events with no guaranteed ordering among the subscribers. Any additional events raised during event handling are run after all the original event handlers complete, thereby avoiding any possibility of reentrancy but complicating synchronous behavior. Also note that events intentionally decouple publishers and subscribers, and since dispatching events is typically not synchronous within the ECS, it can be difficult to trace the source of events when something goes wrong (no callstack).

- **Coroutines**: Coroutines allow capturing state and continuing processing for longer than the handling of a single event. They can be used to achieve synchronous behavior despite the asynchronous nature of event handling. This is one of the few parts of the code which incurs allocation.

- **Multithreading**: It's often useful to run physics in parallel with other processing that doesn't depend on its output, but the event system currently has no built-in features to facilitate multiple threads reading or writing. Instead, parallel execution is implemented at a higher level actor system, or you can implement your own multithreaded systems.

- **Event ordering**: For systems that subscribe to the same event and access the same resources or components, you need to consider whether one is dependent on the other and should run first. One way to guarantee ordering is to define individual sub-events for the systems and publish those events in the desired order as part of a coroutine started from the original event (with waits following each event to ensure all subscribers are run before proceeding).

## Actors

While ECS containers provide a simple and fast means of storing and updating shared memory state using a single thread, actors share no common state and communicate only through messages, making them suitable for parallel processing.

- **Definitions**: Actors are identified by an actor ID. They are statically defined and created on demand when a message is sent to a nonexistent actor ID. At that point, an actor consisting of a message handler is created based on any definitions registered in the actor system that match the actor ID. It's closer to a mailbox processor than a complete actor model since these actors can't dynamically create arbitrary actors or control actor lifetimes.

- **Actor messages versus container events**: "Events" and "messages" are often used interchangeably, but here we use separate terms to distinguish container 'events' from actor 'messages'. Containers already have their own internal event system, but the semantics are a bit different from actors because container events are always stored in separate channels by event type rather than a single serialized channel for all actor message types. The use of separate channels within containers allows for efficient batch processing in cases where event types have no ordering dependencies, but ordering by default is preferable in many other cases involving actors.

- **Wrapping containers**: It's useful to wrap a container within an actor, where incoming messages to the actor automatically dispatched to the container, and systems within the container have access to an outbox for sending messages to other actors. This approach allows keeping isolated worlds for AI forces or a UI container.

- **Replay debugging**: If you can write logic where your game state is fully determined by the sequence of incoming messages, you can log these messages and replay them to diagnose bugs. This works best if you can isolate the problem to a single actor, such as observing incorrect state or incorrect outgoing messages given a reasonable input sequence.

- **Message ordering**: Messages sent from one actor to another are guaranteed to arrive in the order they were sent, but they may be interleaved with messages arriving from other actors. In general, multiple actors and parallelism can introduce complexity similar to the use of microservices, which address scaleability but can introduce race conditions and challenges in synchronization.

- **Multithreading**: You can designate actors to run on either the main thread or a background thread. Actors are run whenever outstanding messages arrive, resembling task-based parallelism. In addition to running actors, the main thread also delivers messages among actors. Background actors are currently run using a fixed pool of worker threads.

## Roadmap

- Performance improvements
- Container snapshots and entity replication 
- More samples
- More test coverage
- Benchmarks
- Comprehensive docs
- Urho scripts with hot reloading
- Extensions or samples for networking and compression
- Fault tolerance in actor system
- Tighten public API

## FAQ

- **Why F#?** F# offers conciseness, functional-first defaults like immutability, an algebraic type system, interactive code editing, and pragmatic support for other paradigms like OOP. Strong type safety makes it more likely that code is correct, which is especially helpful for tweaking game code that changes frequently enough to make unit testing impractical.

- **What about performance?** Functional code often involves allocation, which sometimes conflicts with the goal of consistent performance when garbage collection occurs. Part of the reason for this library is to reduce the effort in writing code that minimizes allocation. But for simple games, this is likely a non-issue and you should start with idiomatic code.

- **Why use ECS over MVU?** You probably shouldn't start with ECS for a simple game, at least not when initially prototyping, unless you already have a good understanding of where it might be beneficial. MVU avoids a lot of complexity and has stronger type safety and immutability guarantees than ECS, but you may encounter issues if your project has demanding performance requirements or needs more flexibility than it allows. 

## License
This project is licensed under the [MIT license](https://github.com/bcarruthers/garnet/blob/master/LICENSE).

## Maintainer(s)

- [@bcarruthers](https://github.com/bcarruthers)