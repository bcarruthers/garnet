//#r "paket:
//nuget FSharp.Core 4.5.0.0
//nuget Fake.DotNet
//nuget Fake.DotNet.AssemblyInfoFile
//nuget Fake.DotNet.Cli
//nuget Fake.Core.Target
//nuget Fake.Core.ReleaseNotes
//nuget Fake.IO.FileSystem
//nuget Fake.IO.Zip
//nuget Fake.DotNet.Paket
//nuget Fake.Core.Xml //"
//#load "./.fake/build.fsx/intellisense.fsx"
#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.Cli
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.Core.ReleaseNotes
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

// https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#if !FAKE
  #r "Facades/netstandard"
#endif

open Fake
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

let title = "Garnet"
let description = "Game composition library"
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let tags = "game ecs actor"
let authors = "Ben Carruthers"
let owners = "Ben Carruthers"
let projectUrl = "https://github.com/bcarruthers/garnet"
let copyright = "Copyright © 2019 Ben Carruthers"
let buildDir = "./build/"
let libDir = buildDir + "lib/"
let testsDir = buildDir + "tests/"
let samplesDir = buildDir + "samples/"

Target.create "Clean" (fun _ ->
    Shell.cleanDir buildDir
)

Target.create "AssemblyInfo" (fun _ ->
    [
        "src/Garnet", "Garnet"
        "tests/Garnet.Tests", "Garnet.Tests"
        "samples/Garnet.Samples.Common", "Garnet.Samples.Common"
        "samples/Garnet.Samples.Flocking", "Garnet.Samples.Flocking"
    ]
    |> List.iter (fun (path, name) ->
        let filename = path + "/AssemblyInfo.fs"
        let ns = name + ".AssemblyInfo"
        AssemblyInfoFile.createFSharpWithConfig filename [
            AssemblyInfo.Title name
            AssemblyInfo.Product name
            AssemblyInfo.Copyright copyright
            AssemblyInfo.Description description
            AssemblyInfo.Version release.AssemblyVersion
            AssemblyInfo.FileVersion release.AssemblyVersion
        ] (AssemblyInfoFileConfig(true, false, ns))
    )
)

Target.create "BuildLibrary" (fun _ ->
  !! "src/**/*.fsproj"
    |> MSBuild.runRelease id libDir "Build"
    |> Trace.logItems "LibraryBuild-Output: "
)


Target.create "BuildTests" (fun _ ->
  !! "tests/**/*.fsproj"
    |> MSBuild.runRelease id testsDir "Build"
    |> Trace.logItems "TestBuild-Output: "
)

Target.create "BuildSamples" (fun _ ->
  !! "samples/**/*.fsproj"
    |> MSBuild.runRelease id samplesDir "Build"
    |> Trace.logItems "TestBuild-Output: "
)

Target.create "Tests" (fun _ ->
    DotNet.exec id (testsDir + "Garnet.Tests.dll --summary") |> ignore
)

Target.create "TestsOnly" (fun _ ->
    let path = testsDir + "Garnet.Tests.dll --summary"
    printfn "Path: %s" path
    DotNet.exec id path |> ignore
)
//Target.create "Tests" <| fun _ ->
//    let result =
//        Process.execSimple (fun info -> 
//            { info with
//                FileName = "./src/Garnet.Tests/bin/Release/Garnet.Tests.exe"
//            }
//        ) (System.TimeSpan.FromMinutes 1.0)
//    if result <> 0 then failwith "Failed result from fsyacc"

Target.create "Default" (fun _ ->
    Trace.trace "Completed"
)    

"Clean"
    ==> "AssemblyInfo"
    ==> "BuildLibrary"
    ==> "BuildTests"
    ==> "BuildSamples"
    ==> "Tests"
    ==> "Default"

Target.runOrDefault "Default"