open Garnet.Samples.Engine
open Garnet.Samples.Trixel

[<EntryPoint>]
let main argv =
    use fs = new FileFolder("assets")
    Game.Run(fs)
    0