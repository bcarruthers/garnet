#r "netstandard"
#r "bin/Release/netcoreapp2.1/Falias.dll"

open Falias.Ecs

// events
[<Struct>] type Update = { dt : float32 }

// components
[<Struct>] type Velocity = { vx : float32; vy : float32 }
[<Struct>] type Position = { x : float32; y : float32 }

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
