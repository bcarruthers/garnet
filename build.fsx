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

open System
open System.IO
open Fake
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

let title = "Garnet"
let summary = "Garnet is a lightweight game composition library for F# with entity-component-system (ECS) and actor-like messaging features."
let description = "F# game composition library"
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let tags = "fsharp game ecs actor"
let authors = "Ben Carruthers"
let owners = "Ben Carruthers"
let projectUrl = "https://github.com/bcarruthers/garnet"
let copyright = "Copyright © 2019 Ben Carruthers"
let license = "MIT"
let buildDir = Path.getFullName "./artifacts/"
let libDir = buildDir + "lib/"
let testsDir = buildDir + "tests/"
let samplesDir = buildDir + "samples/"
let nugetDir = buildDir + "nuget/"
let configuration = DotNet.BuildConfiguration.Release

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
    let result = DotNet.exec id (testsDir + "Garnet.Tests.dll") "--summary"
    printfn "%A" result
)

Target.create "Pack" (fun _ ->
    let nugetProjects = !! "src/**/*.??proj"
    for proj in nugetProjects do
        DotNet.pack (fun p ->
            { p with
                OutputPath = Some nugetDir
                Configuration = configuration
                NoBuild = true
                MSBuildParams = { 
                    p.MSBuildParams with
                        Properties =
                        [
                            "Version", release.NugetVersion
                            "PackageReleaseNotes", (String.concat Environment.NewLine release.Notes) 
                            "Title", title
                            "PackageDescription", summary
                            "PackageTags", tags
                            "Copyright", copyright
                            "Authors", authors
                            "Owners", owners
                            "Description", description
                            "RepositoryUrl", projectUrl
                            "PackageLicenseExpression", license
                        ]
                }
            }
        ) proj
)

Target.create "Default" (fun _ ->
    Trace.trace "Completed"
)    

"Clean"
    ==> "AssemblyInfo"
    ==> "BuildLibrary"
    ==> "BuildTests"
    ==> "BuildSamples"
    ==> "Tests"
    ==> "Pack"
    ==> "Default"

Target.runOrDefault "Default"