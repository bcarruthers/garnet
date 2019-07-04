open Garnet.Ecs
open Garnet.Samples.Flocking.Domain
open Garnet.Samples.Flocking.Systems
open Garnet.Samples.Flocking.MonoGameWrapper

let settings = {
    seed = 1
    vehicleCount = 200
    spawnRange = 300.0f
    maxVehicleSpeed = 50.0f
    trailLifespan = 0.6f
    steering = {
        forwardWeight = 20.0f
        cohesionWeight = 3.0f
        tetherWeight = 1.0f
        separationWeight = 3.0f
        alignmentWeight = 1.0f
        maxAlignmentDistance = 100.0f
        maxSeparationDistance = 70.0f
        maxCohesionDistance = 400.0f
        maxTetherDistance = 300.0f
    }
}

let test() =
    let c = Container()    
    CoreSystems.definition.register c |> ignore
    c.AddResource settings//{ settings with vehicleCount = 10 }
    c.Run <| Reset()
    for i = 1 to 10 do
        c.Run <| { Update.deltaTime = 0.1f }
    c.Get(Eid 64).ToString()

let setCurrentDir() =
    let path = System.IO.Path.GetDirectoryName(typeof<TextureEntry>.Assembly.Location)
    printfn "Path: %s" path
    System.IO.Directory.SetCurrentDirectory path

[<EntryPoint>]
let main argv =
    setCurrentDir()
    use g = new SampleGame(settings)
    g.Run()
    0
