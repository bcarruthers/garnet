namespace Garnet.Samples.Assorted

open System
open System.Numerics
open Veldrid
open Garnet.Composition
open Garnet.Graphics
open Garnet.Input
open Garnet.Numerics

[<AutoOpen>]
module Extensions =
    type Container with
        member c.AddPixelCoordinateCamera(cameraId) =
            c.On<Draw> <| fun e ->
                // Set projection to use pixel coords
                let cameras = c.Get<CameraSet>()
                cameras.[cameraId].ProjectionTransform <- Matrix4x4.CreateOrthographicOffCenter(
                    0.0f, float32 e.ViewSize.X, float32 e.ViewSize.Y, 0.0f, -1.0f, 1.0f)
                
        member c.AddEscapeToClose() =
            c.On<KeyDown> <| fun e ->
                match e.KeyCode with
                | Key.Escape -> c.Get<WindowRenderer>().Close()
                | _ -> ()

        static member Run(register : Container -> IDisposable) =
            let c = Container()
            use sys = register c
            c.RunLoop()

