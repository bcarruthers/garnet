open Garnet.Engine
open Garnet.Samples.Flocking

[<EntryPoint>]
let main argv =
    let fs = FileStreamSource("assets")
    Game.Run(fs)
    0