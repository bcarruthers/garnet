# Samples

## Flocking

[[Code](Garnet.Samples.Flocking)]

Boids-style flocking and clustering using Garnet ECS and Veldrid.

![Flocking](Garnet.Samples.Flocking/flocking-screenshot.png "Flocking")

## Trixel

[[Code](Garnet.Samples.Trixel)]

Trixel editor using Veldrid and Dear ImGUI.

![Trixel](Garnet.Samples.Trixel/trixel-screenshot.png "Trixel")

## Roguelike

[[Code](Garnet.Samples.Roguelike)]

Classic roguelike using Veldrid. Core logic and types are idiomatic F# with no ECS. FSI script allows for replay and testing.

![Roguelike](Garnet.Samples.Roguelike/roguelike-screenshot.png "Roguelike")

## Processor

[[Code](Garnet.Processor)]

This experimental utility is intended for asset processing. Currently it's just a packing utility for either assets or game files, using zip as the archive format.

Garnet.Processor is available as a dotnet tool Nuget package [here](https://www.nuget.org/packages/Garnet.Processor/) or via the command: `dotnet tool install Garnet.Processor`.

**Background:** Asset files (textures, sounds, etc) are often converted and packed into an archive when released with the game. Converting files to an optimal format for the target platform or hardware can make loading faster. Having a small number of packed asset files reduces the overhead of opening individual files and reduces wasted space on the file system.

## Numerics

[[Code](Garnet.Numerics)]

This experimental library has a variety of game-centric numerics or related code that isn't already covered by System.Numerics. It has no dependency on other Garnet libraries.

Garnet.Numerics is available as a Nuget package [here](https://www.nuget.org/packages/Garnet.Numerics).

## Toolkit

[[Code](Garnet.Toolkit)]

Experimental shared code used by samples, including sprite drawing, particles, noise, audio playback, and more. Depends on [Veldrid](https://github.com/mellinoe/veldrid) for graphics, [OpenTK.OpenAL](https://opentk.net/) for audio, [ZLogger](https://github.com/Cysharp/ZLogger) for logging, and Garnet.Numerics.

Garnet.Toolkit is available as a Nuget package [here](https://www.nuget.org/packages/Garnet.Toolkit).

This library has two levels of integration:
- Graphics/audio/etc: Wrapper or convenience methods over other libraries with no dependence on Garnet ECS
- Composition: For use with Garnet ECS, the library includes predefined plugin-like systems for the various functionality

```fsharp
static member AddDefaultSystems(c : Container) =
    Disposable.Create [
        c.AddAssetsFolder()
        c.AddTextLoaders()
        c.AddGraphics()
        c.AddAudioDevice()
        c.AddAudioLoaders()
        c.AddTickUpdate()
        ]
```

The asset system in Garnet.Toolkit can load assets either from a folder (first choice) or from an asset archive file (if no folder is present). The folder option can be convenient for editing assets during development or for allowing modding.

**Known issue:** Microsoft.CodeAnalysis.* assemblies may be included in build output, but they are not needed and add around 10 MB. They are included because Garnet.Toolkit depends on Veldrid, which depends on SharpDX, which depends on NETCore.App, which depends on the CodeAnalysis assemblies. Workaround: Use [Paket](https://github.com/fsprojects/Paket), which uninstalls transitive dependencies if no direct dependencies still depend on them.