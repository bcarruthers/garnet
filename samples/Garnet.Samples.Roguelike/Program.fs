open Garnet.Samples.Engine
open Garnet.Samples.Roguelike

[<EntryPoint>]
let main argv =
    let fs = FileStreamSource("assets")
    Game.Run(fs)
    0