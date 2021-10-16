module Garnet.Tests.Examples

open Garnet.Composition

// Common

type Msg = struct end
type UpdatePositions = struct end
type DestroyZeroHealth = struct end
type EnemyMarker = struct end

[<Struct>] type Position = { x : float32; y : float32 }
[<Struct>] type Velocity = { vx : float32; vy : float32 }

// create a world
let world = Container()

module Example1a =
    // events
    [<Struct>] type Update = { dt : float32 }

    // register a system that updates position
    let system =
        world.On<Update> (
            fun e struct(p : Position, v : Velocity) -> {
                x = p.x + v.vx * e.dt
                y = p.y + v.vy * e.dt
                }
            |> Join.update2
            |> Join.over world)

module Example1b =
    // events
    [<Struct>] type Update = { dt : float32 }

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

[<Struct>]
type Health = {
    hp : int
}

module HashSpaceSystem =     
    let register (c : Container) =
        Disposable.Create [
            ]

let c = Container()

// Subscribing to events

[<Struct>] type UpdateTime = { dt : float32 }

// sub is IDisposable, which can be used to unsubscribe
let sub =
    c.On<UpdateTime> <| fun e ->
        // [do update here]
        printfn "%A" e

// Defining systems

// a system is just a group of related subscriptions,
// optionally with a name
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

[<AutoOpen>]
module MovementSystemExtensions =
    type Container with
        member c.AddMovementUpdate() =
            c.On<UpdatePositions> <| fun e ->
                printfn "%A" e
                
        member c.AddMovementSystems() =  
            Disposable.Create [
                c.AddMovementUpdate()
                ]
            
// Iterating over entities

let runIter =
    // first define an iteration callback:
    // (1) param can be any type or just ignored
    // (2) use struct record for component types
    fun param struct(eid : Eid, p : Position, h : Health) ->
        if h.hp <= 0 then 
            // [start animation at position]
            // destroy entity
            c.Destroy(eid)
    // iterate over all entities with all components
    // present (inner join)
    |> Join.iter3
    // iterate over container
    |> Join.over c
let healthSub =
    c.On<DestroyZeroHealth> <| fun e ->
        runIter()

let healthSubQuery =
    c.On<DestroyZeroHealth> <| fun e ->
        for r in c.Query<Eid, Position, Health>() do
            let h = r.Value3
            if h.hp <= 0 then
                let eid = r.Value1
                c.Destroy(eid)

let healthSubBatchQuery =
    c.On<DestroyZeroHealth> <| fun e ->
        for seg, eids, _, hs in c.QuerySegments<Eid, Position, Health>() do
            for i in seg do
                let h = hs.[i]
                if h.hp <= 0 then
                    let eid = eids.[i]
                    c.Destroy(eid)

// Composing systems

module CoreSystems =        
    let register (c : Container) =
        Disposable.Create [
            MovementSystem.register c
            HashSpaceSystem.register c
        ]

// Running stack-like coroutines

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
c.Run()

// Updating in stages

// events
type Update = struct end
type UpdatePhysicsBodies = struct end
type UpdateHashSpace = struct end

// systems
let updateSystem =
    c.On<Update> <| fun e -> 
        c.Start <| seq {
            // using shorthand 'send and defer' to suspend
            // execution here to achieve ordering of 
            // sub-updates
            yield c.Wait <| UpdatePhysicsBodies()
            yield c.Wait <| UpdateHashSpace()
        }
let system1 = 
    c.On<UpdatePhysicsBodies> <| fun e ->
        // [update positions]
        printfn "%A" e
let system2 = 
    c.On<UpdateHashSpace> <| fun e ->
        // [update hash space from positions]
        printfn "%A" e

// Running time-based coroutines

// start a coroutine
c.Start <| seq {
    for i = 1 to 5 do
        printf "[%d] " i
        // yield execution until time units pass
        yield Wait 3L
    }

// simulate update loop
// output: [1] 1 2 3 [2] 4 5 6 [3] 7 8 9
for i = 1 to 9 do
    // increment time units and run pending coroutines
    c.Step 1L
    c.Run()
    printf "%d " i

// Creating actors

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

// Subscribing to event batch

[<Struct>] type AddShip = { x : float32; y : float32 }

let batchSub =
    c.OnAll<AddShip> <| fun list ->
        for e in list.Span do
            // [do update here]
            printfn "%A" e

// Creating an entity

let entity = 
    c.Create()
        .With({ x = 10.0f; y = 5.0f })
        .With({ vx = 1.0f; vy = 2.0f })
        
// Defining a container actor
let test() =
    use a = new ActorSystem()
    a.Register(ActorId 1, fun (c : Container) ->
        c.On<Ping> <| fun e ->
            c.Respond(Pong()))
    a.Register(ActorId 2, fun (c : Container) ->
        c.On<Ping> <| fun e ->
            c.Respond(Pong()))
    a.Send(ActorId 1, Ping())
    a.ProcessAll()