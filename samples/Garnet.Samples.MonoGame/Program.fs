open Garnet.Ecs
open Garnet.Samples
open Garnet.Samples.FrameworkTypes
open Garnet.Samples.MonoGame.MonoGameWrapper

let setCurrentDir() =
    let path = System.IO.Path.GetDirectoryName(typeof<TextureEntry>.Assembly.Location)
    printfn "Path: %s" path
    System.IO.Directory.SetCurrentDirectory path

[<EntryPoint>]
let main argv =
    setCurrentDir()
    use g = new SampleGame(Flocking.Systems.definition)
    g.Run()
    0
