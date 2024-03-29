﻿namespace Garnet.Samples.Trixel

open System
open System.Numerics
open Newtonsoft.Json
open Garnet.Numerics
open Veldrid

module CellLocation =
    let origin = { X = 0; Y = 0 }
    
module TriCoords =
    /// Height of an equilateral triangle with edge length one. Multiplier to determine simplex
    /// height given an edge length (sqrt(3/4) = 0.866025403784f).
    let edgeToHeight = 0.866025403784f
    
    /// Edge length of an equilateral triangle with height one. Multiplier to determine simplex
    /// edge length given a height (sqrt(4/3) = 1.154700538379f).
    let heightToEdge = 1.154700538379f

    let inline eucToVertexf (v : Vector2) = 
        let y = -heightToEdge * v.Y
        let x = v.X - 0.5f * y
        Vector2(x, y)
        
    /// Rhombus/vertex to tri cell
    let inline vertexToTri (p : Vector2i) side =
        Vector2i(p.X * 2 + side, p.Y)

    /// Gets location of cell in tri coords that contains rhombus point.
    let vertexToContainingTriCell (p : Vector2) =
        let bx = floor p.X
        let by = floor p.Y
        let fx = p.X - bx
        let fy = p.Y - by
        let side = if fy < 1.0f - fx then 0 else 1
        vertexToTri (Vector2i(int bx, int by)) side
    
    let eucToContainingTriCell = 
        eucToVertexf >> vertexToContainingTriCell

    let inline vertexToEucf (v : Vector2) =
        Vector2(v.X + v.Y * 0.5f, v.Y * -edgeToHeight)
        
    let inline vertexToEuc (v : Vector2i) =
        v.ToVector2() |> vertexToEucf

module UndoState =
    let init value = { 
        Previous = []
        Next = []
        Current = value 
        }

module Command =
    let undo state =
        match state.Previous with
        | head :: tail -> {
            Previous = tail
            Next = state.Current :: state.Next
            Current = head 
            }
        | _ -> state

    let redo state =
        match state.Next with
        | head :: tail -> {
            Previous = state.Current :: state.Previous
            Next = state.Next.Tail
            Current = state.Next.Head 
            }
        | _ -> state

    let replace state value = {
        Previous = state.Current :: state.Previous
        Next = []
        Current = value 
        }

    let apply state cmd =
        match cmd with
        | Identity -> state
        | Undo -> undo state
        | Redo -> redo state
        | Replace c -> replace state c
        | Apply f -> replace state (f state.Current)

module GridState =
    let empty = {
        Cells = Map.empty
        }

    let draw p color state = {
        state with Cells = Map.add p color state.Cells
        }  
        
    let erase p state = {
        state with Cells = Map.remove p state.Cells
        }  

    type SavedGrid = {
        cells : string list
        }
        
    let toHexString (c : RgbaByte) =
       let x = (int c.R <<< 24) ||| (int c.G <<< 16) ||| (int c.B <<< 8) ||| (int c.A <<< 0)
       $"%08x{x}"

    let serialize (state : GridState) =
        JsonConvert.SerializeObject({
            cells =
                state.Cells 
                |> Seq.sortBy (fun kvp -> kvp.Key.Y, kvp.Key.X)
                |> Seq.map (fun kvp -> $"%d{kvp.Key.X} %d{kvp.Key.Y} %s{toHexString kvp.Value}")
                |> Seq.toList
            }, Formatting.Indented)
        
    /// RGBA from high to low bits
    let uint32ToRgbaByte (x : uint32) = 
        RgbaByte(
            byte ((x >>> 24) &&& (uint32 0xff)),
            byte ((x >>> 16) &&& (uint32 0xff)),
            byte ((x >>> 8) &&& (uint32 0xff)),
            byte ((x >>> 0) &&& (uint32 0xff)))

    let parseRgbaHex str =
        UInt32.Parse(str, Globalization.NumberStyles.HexNumber)
        |> uint32ToRgbaByte

    let deserialize str =
        let g = JsonConvert.DeserializeObject<SavedGrid>(str)
        { 
            GridState.Cells = 
                g.cells 
                |> Seq.map (fun c -> 
                    let parts = c.Split(' ')
                    { X = Int32.Parse(parts.[0]); Y = Int32.Parse(parts.[1]) }, 
                    parseRgbaHex parts.[2])
                |> Map.ofSeq
        }

    let sample param (grid : GridState) =
        let w = max 0 param.OutputWidth
        let h = max 0 param.OutputHeight
        let r = param.Bounds
        let s = max 1 param.SampleFactor
        let samplesPerPixel = s * s
        let data = Array.zeroCreate (w * h * 4)
        // uniform supersampling of pixels
        // |.....x.....|.....x.....|
        // |..x.....x..|..x.....x..|
        for y = 0 to h - 1 do
            for x = 0 to w - 1 do
                let mutable sr = 0
                let mutable sg = 0
                let mutable sb = 0
                let mutable sa = 0
                for sy = 0 to s - 1 do
                    for sx = 0 to s - 1 do
                        let nx = (float32 (x * s + sx) + 0.5f) / float32 (w * s)
                        let ny = (float32 (y * s + sy) + 0.5f) / float32 (h * s)
                        let np = Vector2(nx, ny)
                        let ep = Range2.Lerp(r, np)
                        let cp = TriCoords.eucToContainingTriCell ep
                        let color =
                            let cp = { X = cp.X; Y = cp.Y }
                            match grid.Cells.TryGetValue(cp) with
                            | false, _ -> param.Background
                            | true, x -> x
                        sr <- sr + int color.R
                        sg <- sg + int color.G
                        sb <- sb + int color.B
                        sa <- sa + int color.A
                // reverse y for texture
                let i = (h - 1 - y) * w + x
                data.[i * 4 + 0] <- sr / samplesPerPixel |> byte
                data.[i * 4 + 1] <- sg / samplesPerPixel |> byte
                data.[i * 4 + 2] <- sb / samplesPerPixel |> byte
                data.[i * 4 + 3] <- sa / samplesPerPixel |> byte
        data

module Viewport =    
    let getViewSize zoom width height =
        let tileSize = 24.0f
        let aspect = float32 width / float32 height
        let widthInTiles = float32 width / tileSize
        let heightInTiles = widthInTiles / aspect
        let scale = MathF.Pow(2.0f, float32 -zoom * 0.5f) |> float32
        Vector2(widthInTiles, heightInTiles) * scale

    let getInverseOrIdentity (m : Matrix4x4) =
        let mutable mInv = Matrix4x4.Identity
        if Matrix4x4.Invert(m, &mInv) then mInv else Matrix4x4.Identity

module TriPositions =
    let inline fromTriCellScaled (tileSize : Vector2) (p : Vector2i) =
        let y0 = float32 p.Y
        let y1 = float32 (p.Y + 1)
        let py0 = Vector2(y0 * 0.5f, y0 * -tileSize.Y)
        let py1 = Vector2(y1 * 0.5f, y1 * -tileSize.Y)
        let ax = p.X >>> 1
        let x0 = float32 ax
        let x1 = float32 (ax + 1)
        let p01 = Vector2((py1.X + x0) * tileSize.X, py1.Y)
        let p10 = Vector2((py0.X + x1) * tileSize.X, py0.Y)
        let side = p.X &&& 1
        if side = 0 
            then { P0 = Vector2((py0.X + x0) * tileSize.X, py0.Y); P1 = p10; P2 = p01 }
            else { P0 = Vector2((py1.X + x1) * tileSize.X, py1.Y); P1 = p01; P2 = p10 }

    let inline fromTriCell (p : Vector2i) =
        fromTriCellScaled (Vector2(1.0f, TriCoords.edgeToHeight)) p
