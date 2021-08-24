open Garnet.Composition
open Garnet.Samples.Flocking

[<EntryPoint>]
let main argv =
    use fs = new FileFolder("assets")
    Game.Run(fs)
    0