#load ".fake/build.fsx/intellisense.fsx"

open System
open System.IO
open Fake
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
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
    !! "**/bin"
    ++ "**/obj"
    ++ buildDir
    |> Shell.cleanDirs 
)

Target.create "AssemblyInfo" (fun _ ->
    [
        "src/Garnet", "Garnet"
        "tests/Garnet.Tests", "Garnet.Tests"
        "samples/Garnet.Samples.Common", "Garnet.Samples.Common"
        "samples/Garnet.Samples.MonoGame", "Garnet.Samples.MonoGame"
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

// needed to restore netstandard, etc
Target.create "RestoreSln" (fun _ ->
    DotNet.restore id "Garnet.sln"
)

let build outputDir = 
    DotNet.build (fun p -> { 
        p with
            OutputPath = Some outputDir
            Configuration = configuration
    })

// use default output folder because pack expects DLL there
Target.create "BuildLibrary" (fun _ ->
    !! "src/**/*.fsproj"
    |> Seq.iter (DotNet.build id)
)

Target.create "BuildTests" (fun _ ->
    !! "tests/**/*.fsproj"
    |> Seq.iter (build testsDir)
)

Target.create "BuildSamples" (fun _ ->
    !! "samples/**/*.fsproj"
    |> Seq.iter (build samplesDir)
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
    ==> "RestoreSln"
    ==> "BuildLibrary"
    ==> "BuildTests"
    ==> "BuildSamples"
    ==> "Tests"
    ==> "Pack"
    ==> "Default"

Target.runOrDefault "Default"