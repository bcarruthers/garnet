open Garnet.Samples.Engine
open Garnet.Samples.Trixel

[<EntryPoint>]
let main argv =
    let fs = FileStreamSource("assets")
    Game.Run(fs)
    0