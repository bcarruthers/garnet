namespace Garnet.Samples.Trixel

open System.Numerics
open Garnet.Numerics
open Veldrid

[<AutoOpen>]
module Grids =
    [<Struct>]
    type CellLocation = {
        X : int
        Y : int
        }

    type GridState = {
        Cells : Map<CellLocation, RgbaByte>
        }

    type GridStateChanged = {
        NewGridState : GridState
        }

    [<Struct>]
    type SamplingParams = {
        OutputWidth : int
        OutputHeight : int
        SampleFactor : int
        Bounds : Range2
        Background : RgbaByte
        }

    type GridLineState = {
        LineSpacing : int
        }

    type ViewportSize = {
        ViewportSize : int
        }

    [<Struct>]
    type TriPositions = {
        P0 : Vector2
        P1 : Vector2
        P2 : Vector2
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
        Previous : 'a list
        Next : 'a list
        Current : 'a 
        }

    type FileCommand =
        | Load of string
        | Save of string
        
    type ExportCommand = {
        ExportFile : string
        SamplingParams : SamplingParams
        }

    type Command =
        | Export of ExportCommand
        | FileCommand of FileCommand
        | GridCommand of Command<GridState>