namespace Garnet.Samples.Trixel

open System
open System.Numerics
open System.Threading
open System.IO
open Veldrid
open SixLabors.ImageSharp
open Garnet.Numerics
open Garnet.Resources
open Garnet.Graphics
open Garnet.Input

module Resources =
    let squareTex = "square.png"

    let shaderSet = {
        VertexShader = "texture-dual-color.vert"
        FragmentShader = "texture-dual-color.frag"
        Layout = PositionTextureDualColorVertex.Description
        }

    let pipeline = {
        Blend = Blend.Alpha
        Filtering = Filtering.Linear
        ShaderSet = shaderSet
        Texture = squareTex
        }

    let cellLayer = {
        LayerId = 4
        CameraId = 0
        Primitive = Triangle
        FlushMode = NoFlush
        Pipeline = pipeline 
        }

    let gridLineLayer = {
        LayerId = 3
        CameraId = 0
        Primitive = Quad
        FlushMode = NoFlush
        Pipeline = pipeline 
        }

[<AutoOpen>]
module DrawingExtensions =
    type SpriteRenderer with
        member c.DrawGrid(state) =
            let mesh = c.GetVertices(Resources.cellLayer)
            mesh.DrawGridCells(state.Current)
            mesh.Flush()        

type Game(fs : IReadOnlyFolder) =
    // Create window and graphics device
    let ren =
        new WindowRenderer( 
            title = "Trixel", 
            width = 800,
            height = 600,
            Background = RgbaFloat(0.0f, 0.1f, 0.2f, 1.0f))
    // Initialize rendering
    let shaders = new ShaderSetCache()
    let textures = new TextureCache()
    let sprites = new SpriteRenderer(ren.Device, shaders, textures)
    let gui = new Gui(ren.Device, ren.ImGui)
    do
        shaders.Load(ren.Device, fs, Resources.shaderSet)
        textures.Load(ren.Device, fs, Resources.squareTex)
    member c.Run() =
        let mutable state = UndoState.init GridState.empty
        sprites.DrawGrid(state)
        // Grid lines
        let mesh = sprites.GetVertices(Resources.gridLineLayer)
        mesh.DrawGridLines()
        mesh.Flush()
        // Cells
        let mesh = sprites.GetVertices(Resources.cellLayer)
        mesh.DrawGridCells(state.Current)
        mesh.Flush()
        // Start loop
        let inputs = InputCollection()
        let cameras = CameraSet()
        while ren.Update(0.0f, inputs) do
            // Calculate transforms
            let sizeInTiles = Viewport.getViewSize 0 ren.WindowSize.X ren.WindowSize.Y
            let proj = Matrix4x4.CreateOrthographic(sizeInTiles.X, sizeInTiles.Y, -100.0f, 100.0f)
            let view = Matrix4x4.Identity
            let camera = cameras.[0]
            camera.ProjectionTransform <- proj
            camera.ViewTransform <-view
            // Draw GUI and collect any user command
            let projView = proj * view
            let invProjView = Viewport.getInverseOrIdentity projView
            let result = gui.Draw(state, inputs, invProjView)            
            // Apply command to state or read/write files
            match result with
            | None -> ()
            | Some cmd ->
                match cmd with
                | GridCommand cmd ->
                    // Update state from command
                    state <- Command.apply state cmd
                    sprites.DrawGrid(state)
                | Export cmd ->
                    let image = Image.createRenderedGridImage cmd.SamplingParams state.Current
                    use fs = File.OpenWrite(cmd.ExportFile)
                    image.SaveAsPng(fs)
                | FileCommand cmd ->
                    match cmd with
                    | Load file -> 
                        let cmd =
                            File.ReadAllText(file) 
                            |> GridState.deserialize
                            |> Replace
                        state <- Command.apply state cmd
                        sprites.DrawGrid(state)
                    | Save file -> 
                        Directory.CreateDirectory(Path.GetDirectoryName(file)) |> ignore
                        File.WriteAllText(file, GridState.serialize state.Current)
            // Draw to window
            if ren.BeginDraw() then
                sprites.Draw(ren.RenderContext, cameras)
                ren.EndDraw()
            // Sleep to avoid spinning CPU
            Thread.Sleep(1)
    interface IDisposable with
        member c.Dispose() =
            textures.Dispose()
            shaders.Dispose()
            sprites.Dispose()
            gui.Dispose()
            ren.Dispose()
    static member Run(fs) =
        use game = new Game(fs)
        game.Run()
