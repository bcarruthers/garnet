namespace Garnet.Samples.Trixel

open System
open System.Numerics
open Garnet.Samples.Trixel.Types
open SixLabors.ImageSharp
open Veldrid
open ImGuiNET
open Garnet.Samples.Engine

module Image =
    let createRenderedGridImage param state =
        let data = GridState.sample param state
        let rgbaData = Array.zeroCreate (data.Length / 4)
        for i = 0 to rgbaData.Length - 1 do
            rgbaData.[i] <- 
                PixelFormats.Rgba32(
                    data.[i * 4],
                    data.[i * 4 + 1],
                    data.[i * 4 + 2],
                    data.[i * 4 + 3])
        Image.WrapMemory(Memory(rgbaData), param.outputWidth, param.outputHeight)
        
type PreviewTexture(device : GraphicsDevice, renderer : ImGuiRenderer) =
    let mutable texture : Texture = null
    let mutable lastState = GridState.empty
    let mutable lastParam = Unchecked.defaultof<SamplingParams>
    member c.Draw(param, state, zoom, resolution) =
        let canUpdate =
            lastParam <> param ||
            lastState <> state ||
            texture = null
        if canUpdate then
            if texture <> null
                then texture.Dispose()
            let data = GridState.sample param state
            texture <- device.CreateTextureRgba(param.outputWidth, param.outputHeight, ReadOnlyMemory(data))        
        if texture <> null then
            let texId = renderer.GetOrCreateImGuiBinding(device.ResourceFactory, texture)
            let width = float32 (int texture.Width * zoom / resolution |> max 1)
            let height = float32 (int texture.Height * zoom / resolution |> max 1)
            ImGui.Image(texId, Vector2(width, height))
    member c.Dispose() =
        if texture <> null then
            texture.Dispose()
            texture <- null
    interface IDisposable with
        member c.Dispose() = c.Dispose()
        