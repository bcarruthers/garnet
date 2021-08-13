namespace Garnet.Samples.Trixel

open System.Numerics
open Garnet.Samples.Engine
open Veldrid

[<AutoOpen>]
module Grids =
    [<Struct>]
    type CellLocation = {
        x : int
        y : int
        }

    type GridState = {
        cells : Map<CellLocation, RgbaByte>
        }

    type GridStateChanged = {
        newGridState : GridState
        }

    [<Struct>]
    type SamplingParams = {
        outputWidth : int
        outputHeight : int
        sampleFactor : int
        bounds : Range2
        background : RgbaByte
        }

    type GridLineState = {
        lineSpacing : int
        }

    type ViewportSize = {
        viewportSize : int
        }

    [<Struct>]
    type TriPositions = {
        p0 : Vector2
        p1 : Vector2
        p2 : Vector2
        }

[<AutoOpen>]
module Commands =
    type Command<'a> =
        | Identity
        | Undo
        | Redo
        | Replace of 'a
        | Apply of ('a -> 'a)

    type UndoState<'a> = {
        prev : 'a list
        next : 'a list
        current : 'a 
        }

    type FileCommand =
        | Load of string
        | Save of string
        
    type ExportCommand = {
        exportFile : string
        samplingParams : SamplingParams
        }

    type Command =
        | Export of ExportCommand
        | FileCommand of FileCommand
        | GridCommand of Command<GridState>