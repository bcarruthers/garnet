namespace Garnet.Samples.Trixel

open System
open System.IO
open System.Numerics
open Veldrid
open ImGuiNET
open Garnet.Samples.Engine
open Garnet.Samples.Trixel.Types

type CursorGui() =
    member c.Draw(inputs : InputCollection, invProjView : Matrix4x4) =    
        let pos = ImGui.GetMousePos()
        let normPos = inputs.NormalizedMousePosition
        let viewPos = Vector2.Transform(normPos, invProjView)
        let cellPos = TriCoords.eucToContainingTriCell viewPos
        ImGui.Text($"Position: {pos}")
        ImGui.Text($"Normalized: {normPos}")
        ImGui.Text($"Viewport: {viewPos}")
        ImGui.Text($"Cell: {cellPos}")
        ImGui.Text($"Buttons: {inputs.IsMouseDown(0)} {inputs.IsMouseDown(1)}")
            
type ViewGui() =
    let mutable centerX = 35
    let mutable centerY = -18
    let mutable cellMargin = 0.1f
    let mutable zoom = 0
    member c.Draw(state : UndoState<GridState>) =    
        let grid = state.current
        ImGui.Text($"Cells: {grid.cells.Count}")
        let inv = false
        let inv = ImGui.InputInt("Zoom", &zoom) || inv
        let inv = ImGui.InputInt("X", &centerX) || inv
        let inv = ImGui.InputInt("Y", &centerY) || inv
        ()
        
type EditGui() =
    let mutable primary = Vector4(0.5f, 0.0f, 0.3f, 1.0f)
    let mutable secondary = Vector4(0.3f, 0.0f, 0.6f, 1.0f)
    member c.Draw(state : UndoState<GridState>, inputs : InputCollection, invProjView : Matrix4x4) =    
        if ImGui.Begin("Edit") then
            ImGui.ColorEdit4("Primary", &primary) |> ignore
            ImGui.ColorEdit4("Secondary", &secondary) |> ignore
            let undo = ImGui.Button $"Undo (%d{state.prev.Length})"
            let redo = ImGui.Button $"Redo (%d{state.next.Length})"
            let leftButton = inputs.IsMouseDown(0)
            let rightButton = inputs.IsMouseDown(2)
            let drawCommand =
                let canDraw =
                    not (ImGui.GetIO().WantCaptureMouse) &&
                    (leftButton || rightButton)
                if canDraw then
                    let normPos = inputs.NormalizedMousePosition
                    let viewPos = Vector2.Transform(normPos, invProjView)
                    let modifiers = inputs.Modifiers
                    let p = TriCoords.eucToContainingTriCell viewPos
                    let cp = { x = p.X; y = p.Y }
                    let current = state.current
                    let newState =
                        if leftButton then
                            let v = if modifiers.HasShift() then secondary else primary
                            let color = RgbaFloat(v.X, v.Y, v.Z, v.W).ToRgbaByte()
                            GridState.draw cp color current
                        else
                            GridState.erase cp current                        
                    if newState <> current then Some (Replace newState) else None
                else None
            let result =
                if drawCommand.IsSome then drawCommand
                elif undo then Some Undo
                elif redo then Some Redo
                else None
            ImGui.End()
            result
        else None

type PreviewGui(device : GraphicsDevice, renderer : ImGuiRenderer) =
    let previewTex = new PreviewTexture(device, renderer)
    let mutable zoom = 8
    let mutable width = 35
    let mutable height = 30
    let mutable centerX = 0
    let mutable centerY = 0
    let mutable multisample = 4
    let mutable viewSize = 30
    let mutable resolution = 1
    let mutable file = "preview.png"
    member c.Draw(state : GridState) =
        if ImGui.Begin("Preview") then
            ImGui.InputInt("Zoom", &zoom) |> ignore
            ImGui.InputInt("View size", &viewSize) |> ignore
            ImGui.InputInt("Width", &width) |> ignore
            ImGui.InputInt("Height", &height) |> ignore
            ImGui.InputInt("Multiplier", &resolution) |> ignore
            ImGui.InputInt("X", &centerX) |> ignore
            ImGui.InputInt("Y", &centerY) |> ignore
            ImGui.InputInt("Multisample", &multisample) |> ignore
            ImGui.InputText("File", &file, 128u) |> ignore
            let export = ImGui.Button("Export")
            let viewWidth = float32 viewSize
            let viewHeight = viewWidth * TriCoords.edgeToHeight
            let center = TriCoords.vertexToEuc (Vector2i(centerX, centerY))
            let param = {
                outputWidth = width * resolution
                outputHeight = height * resolution
                sampleFactor = multisample
                bounds = Range2.Sized(center, Vector2(viewWidth, viewHeight))
                background = RgbaByte.Black
                }
            previewTex.Draw(param, state, zoom, resolution)
            ImGui.End()
            if export then Some {
                exportFile = file
                samplingParams = param
                }
            else None
        else None
    member c.Dispose() =
        previewTex.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()

type FileGui() =
    let filter = "*.json"
    let mutable dir = @"trixel-grids"
    let mutable file = ""
    let mutable files = [||]
    let mutable fileIndex = 0
    let mutable filesValid = false
    member c.Draw() =
        if ImGui.Begin("File") then
            if ImGui.Button("Refresh") then filesValid <- false
            if ImGui.InputText("Folder", &dir, 128u) || not filesValid then
                files <- 
                    if Directory.Exists(dir) 
                        then Directory.GetFiles(dir, filter) |> Array.map Path.GetFileName
                        else [||]
                fileIndex <- 0
                filesValid <- true
            if ImGui.ListBox("", &fileIndex, files, files.Length) then
                file <- if fileIndex < files.Length then files.[fileIndex] else ""
            let load =
                if ImGui.Button("Load") && file.Length > 0 then
                    let path = Path.Combine(dir, file)
                    Some (Load path)
                else None
            let save = 
                ImGui.InputText("File", &file, 128u) |> ignore
                if ImGui.Button("Save") && file.Length > 0 then
                    let path = Path.Combine(dir, file)
                    Some (Save path)
                else None
            let result = if load.IsSome then load else save
            ImGui.End()
            result
        else None
        
type StatusGui() =
    let cursorGui = CursorGui()
    let viewGui = ViewGui()
    member c.Draw(state, inputs, invProjView) =
        if ImGui.Begin("Status") then
            cursorGui.Draw(inputs, invProjView)
            viewGui.Draw(state)
            ImGui.End()
        
    
type Gui(device : GraphicsDevice, renderer : ImGuiRenderer) =
    let fileGui = FileGui()
    let editGui = EditGui()
    let previewGui = new PreviewGui(device, renderer)
    let statusGui = StatusGui()
    member c.Draw(state, inputs, invProjView) =
        // draw GUI
        statusGui.Draw(state, inputs, invProjView)
        let editCommand = editGui.Draw(state, inputs, invProjView)
        let fileCommand = fileGui.Draw()
        let previewCommand = previewGui.Draw(state.current)
        // resolve command
        if editCommand.IsSome then Some (GridCommand editCommand.Value)
        elif previewCommand.IsSome then Some (Export previewCommand.Value)
        elif fileCommand.IsSome then Some (FileCommand fileCommand.Value)
        else None
    member c.Dispose() =
        previewGui.Dispose()
    interface IDisposable with
        member c.Dispose() = c.Dispose()
