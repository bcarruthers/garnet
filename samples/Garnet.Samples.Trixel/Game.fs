namespace Garnet.Samples.Trixel

open System
open System.Numerics
open System.Threading
open System.IO
open Veldrid
open SixLabors.ImageSharp
open Garnet.Samples.Engine
open Garnet.Samples.Trixel.Types

type Game(fs : IStreamSource) =
    let ren = new WindowRenderer { 
        CreateWindow.defaults with 
            title = "Trixel" 
            windowWidth = 800
            windowHeight = 600
        }
    let shaders = 
        fs.LoadShaderSet(ren.Device, 
            "texture-dual-color.vert", 
            "texture-dual-color.frag", 
            PositionTextureDualColorVertex.Description)
    let texture = fs.LoadTexture(ren.Device, "square.png")
    let layers = new ColorTextureQuadLayers(ren.Device, shaders, texture, ren.Device.LinearSampler)
    let gui = new Gui(ren.Device, ren.ImGui)
    do 
        ren.Background <- RgbaFloat(0.0f, 0.1f, 0.2f, 1.0f)
        ren.Add(layers)
    member private c.UpdateGrid(state) =
        let mesh = layers.GetLayer(4)
        mesh.DrawGridCells(state.current)
        mesh.FlushTriangles()        
    member c.Run() =
        let mutable state = UndoState.init GridState.empty
        c.UpdateGrid(state)
        // grid lines
        let mesh = layers.GetLayer(3)
        mesh.DrawGridLines()
        mesh.Flush()
        // cells
        let mesh = layers.GetLayer(4)
        mesh.DrawGridCells(state.current)
        mesh.FlushTriangles()
        // Start loop
        ren.Invalidate()
        while ren.Update(0.0f) do
            // Calculate transforms
            let center = Vector2i.Zero
            let eucCenter = TriCoords.vertexToEuc center
            let sizeInTiles = Viewport.getViewSize 0 ren.WindowSize.X ren.WindowSize.Y
            let proj = Matrix4x4.CreateOrthographic(sizeInTiles.X, sizeInTiles.Y, -100.0f, 100.0f)
            let view = Matrix4x4.Identity
            layers.ProjectionTransform <- proj
            layers.ViewTransform <-view
            // Draw GUI and collect any user command
            let projView = proj * view
            let invProjView = Viewport.getInverseOrIdentity projView
            let result = gui.Draw(state, ren.Inputs, invProjView)            
            // Apply command to state or read/write files
            match result with
            | None -> ()
            | Some cmd ->
                match cmd with
                | GridCommand cmd ->
                    // Update state from command
                    state <- Command.apply state cmd
                    c.UpdateGrid(state)
                | Export cmd ->
                    let image = Image.createRenderedGridImage cmd.samplingParams state.current
                    use fs = File.OpenWrite(cmd.exportFile)
                    image.SaveAsPng(fs)
                | FileCommand cmd ->
                    match cmd with
                    | Load file -> 
                        let cmd =
                            File.ReadAllText(file) 
                            |> GridState.deserialize
                            |> Replace
                        state <- Command.apply state cmd
                        c.UpdateGrid(state)
                    | Save file -> 
                        Directory.CreateDirectory(Path.GetDirectoryName(file)) |> ignore
                        File.WriteAllText(file, GridState.serialize state.current)
            // Draw to window
            ren.Invalidate()
            ren.Draw()            
            // Sleep to avoid spinning CPU (note sleep(1) typically takes ~15 ms)
            Thread.Sleep(1)
    interface IDisposable with
        member c.Dispose() =
            texture.Dispose()
            shaders.Dispose()
            gui.Dispose()
            ren.Dispose()
    static member Run(fs) =
        use game = new Game(fs)
        game.Run()
