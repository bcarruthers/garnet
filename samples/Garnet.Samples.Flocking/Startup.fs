namespace Garnet.Samples.Flocking

open System
open Veldrid
open Garnet.Composition
open Garnet.Graphics

module StartupSystem =
    type Container with
        member c.LoadResources() =
            // Manually load textures into atlas. Note other resources like
            // shaders can be loaded on demand and don't need to be explicitly
            // loaded here.
            let device = c.Get<GraphicsDevice>()
            let cache = c.Get<ResourceCache>()
            use fs = new FileFolder("assets")
            fs.LoadTextureAtlasFromFolder(device, Resources.atlas, 512, 512, cache)
            Disposable.Null

    let add (c : Container) =
        // Set global window settings, which will be used by default systems
        c.Set {
            WindowSettings.Default with
                Title = "Flocking"
                Width = 800
                Height = 600
                Background = RgbaFloat(0.0f, 0.1f, 0.2f, 1.0f)
                }
        // Set global settings used by simulation
        c.Set(WorldSettings.defaults)
        Disposable.Create [
            // Default systems for window, sprite drawing, updates, etc
            c.AddDefaultSystems()
            c.LoadResources()
        ]
