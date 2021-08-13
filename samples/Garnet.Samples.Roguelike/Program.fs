open Garnet.Samples.Engine
open Garnet.Samples.Roguelike

[<EntryPoint>]
let main argv =
    use fs = new FileFolder("assets")
    Game.Run(fs)
    0