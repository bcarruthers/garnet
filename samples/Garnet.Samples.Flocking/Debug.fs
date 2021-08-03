namespace Garnet.Samples.Flocking

open System
open System.Numerics
open System.Diagnostics
open ImGuiNET
open Garnet.Samples.Engine

type FpsHud() =
    let fps = FpsGauge(1.0f)
    let fixedFps = FpsGauge(1.0f)
    member _.OnFixedUpdate() =
        let timestamp = Stopwatch.GetTimestamp()
        fixedFps.Update(timestamp)
    member _.OnUpdate() =
        let timestamp = Stopwatch.GetTimestamp()
        fps.Update(timestamp)
    member _.Draw() =
        let flags = 
            ImGuiWindowFlags.NoBackground |||
            ImGuiWindowFlags.NoTitleBar |||
            ImGuiWindowFlags.NoResize |||
            ImGuiWindowFlags.NoMove |||
            ImGuiWindowFlags.NoFocusOnAppearing |||
            ImGuiWindowFlags.NoInputs |||
            ImGuiWindowFlags.NoNavFocus
        ImGui.SetNextWindowSize(Vector2(500.0f, 500.0f))
        ImGui.SetNextWindowPos(Vector2(0.0f, 0.0f))
        if ImGui.Begin("Hud", flags) then
            let info = GC.GetGCMemoryInfo()
            ImGui.SetWindowFontScale(1.0f)
            ImGui.Text $"FPS: %d{int fps.FramesPerSec}, mean: %d{int fps.MeanFrameMs} ms, max: %d{int fps.MaxFrameMs} ms, fixed FPS: %d{int fixedFps.FramesPerSec}"
            ImGui.Text $"GC pause: {info.PauseTimePercentage}%%%%, heap size: {info.HeapSizeBytes / 1024L} Kb"
            ImGui.End()
