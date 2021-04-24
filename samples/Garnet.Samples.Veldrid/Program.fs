open Garnet.Composition
open Garnet.Samples
open Garnet.Samples.Veldrid

[<EntryPoint>]
let main argv =
    let c = Container.Create(Flocking.Systems.register)
    Window.run "Flocking" "assets" c
    0
