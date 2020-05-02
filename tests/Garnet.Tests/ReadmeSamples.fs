module Garnet.Tests.Examples

open Garnet.Composition

// Common

type Msg = struct end
type UpdatePositions = struct end
type DestroyZeroHealth = struct end
type EnemyMarker = struct end

[<Struct>] type Position = { x : float32; y : float32 }
[<Struct>] type Velocity = { vx : float32; vy : float32 }

[<Struct>]
type Health = {
    hp : int
}

module HashSpaceSystem =     
    let definition =
        Registration.listNamed "HashSpace" [
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
    let definition =
        // give a name so we can hot reload
        Registration.listNamed "Movement" [
            registerUpdate
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

// Composing systems

module CoreSystems =        
    let definition =
        Registration.combine [
            MovementSystem.definition
            HashSpaceSystem.definition
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
    yield Wait.defer
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
        yield Wait.time 3L
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
a.Register(ActorId 1, fun c ->
    c.On<Ping> <| fun e -> 
        printf "ping "
        c.Respond(Pong())
    )
a.Register(ActorId 2, fun c ->
    c.On<Pong> <| fun e -> 
        printf "pong "
    )
    
// send a message and run until all complete
// output: ping pong
a.Send(ActorId 1, Ping(), sourceId = ActorId 2)
a.RunAll()

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
    a.Register(ActorId 1, fun h ->
        h.On<Ping> <| fun e ->
            h.Respond(Pong()))
    a.Register(ActorId 2, 
        let c = Container()
        let sub =
            c.On<Ping> <| fun e ->
                c.Respond(Pong())        
        c)
    a.Send(ActorId 1, Ping())
    a.RunAll()