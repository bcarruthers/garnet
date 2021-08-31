open Garnet.Composition
open Garnet.Samples.Flocking

[<EntryPoint>]
let main _ =
    let c = Container()
    use s = c.AddSystems [
        StartupSystem.add
        SimulationSystems.add
        DrawingSystems.add
        DebugSystem.add
        ]
    c.RunLoop()
    0