open Garnet.Composition
open Garnet.Samples
open Garnet.Samples.Veldrid

[<EntryPoint>]
let main argv =
    let c = Container()
    Flocking.Systems.definition 
    |> Registration.registerTo c 
    |> ignore
    Window.run "Flocking" "assets" c
    0
