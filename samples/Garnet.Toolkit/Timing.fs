namespace Garnet.Composition

open System.Diagnostics

type FpsGauge(updateInterval) =
    let mutable count = 0
    let mutable maxValue = 0.0f
    let mutable lastUpdateTimestamp = 0L
    let mutable lastTimestamp = 0L
    let mutable resultMax = 0.0f
    let mutable resultFps = 0.0f
    member c.FramesPerSec = resultFps
    member c.MeanFrameMs = if resultFps > 0.0f then 1000.0f / resultFps else 0.0f
    member c.MaxFrameMs = resultMax * 1000.0f
    member c.Reset() =
        count <- 0
        maxValue <- 0.0f
        lastTimestamp <- 0L
        lastUpdateTimestamp <- 0L
        resultMax <- 0.0f
        resultFps <- 0.0f
    member c.Update(timestamp) =
        let deltaSec = 
            let delta = timestamp - lastTimestamp
            float32 (float delta / float Stopwatch.Frequency)
        let deltaUpdateSec = 
            let delta = timestamp - lastUpdateTimestamp
            float32 (float delta / float Stopwatch.Frequency)
        lastUpdateTimestamp <- timestamp
        maxValue <- max maxValue deltaUpdateSec
        count <- count + 1
        if deltaSec >= updateInterval then
            resultFps <- float32 count / float32 deltaSec
            resultMax <- maxValue
            lastTimestamp <- timestamp
            maxValue <- 0.0f
            count <- 0

type ScalarGauge(updateInterval) =
    let mutable count = 0
    let mutable lastTimestamp = 0L
    let mutable total = 0.0f
    let mutable current = 0.0f
    member c.Current = current
    member c.Reset() =
        count <- 0
        lastTimestamp <- 0L
        total <- 0.0f
        current <- 0.0f
    member c.Update(timestamp, value) =
        let delta = timestamp - lastTimestamp
        let deltaSec = float32 (float delta / float Stopwatch.Frequency)
        count <- count + 1
        total <- total + value
        if deltaSec >= updateInterval then
            lastTimestamp <- timestamp
            current <- total / float32 count
            count <- 0
            total <- 0.0f

[<Struct>]
type TimingSettings = {
    MinDeltaTime : int64
    MaxDeltaTime : int64
    FixedDeltaTime : int64
    IsRunning : bool
    ClockActorId : ActorId
    } with
    static member Default = {
        MinDeltaTime = 0L
        MaxDeltaTime = 250L
        FixedDeltaTime = 16L
        IsRunning = false
        ClockActorId = ActorId.Undefined
        }

type FixedUpdateTimer(settings) =
    let mutable settings = settings
    let mutable lastTime = 0L
    let mutable accumulatedTime = 0L
    let mutable accumulatedFixedTime = 0L
    let mutable variableTime = 0L
    let mutable variableDeltaTime = 0L
    let mutable fixedTime = 0L
    let mutable frameCount = 0L
    member c.HasUpdate =
        accumulatedTime >= settings.MinDeltaTime 
    member c.HasFixedUpdate =
        accumulatedFixedTime >= settings.FixedDeltaTime && settings.IsRunning
    member c.SetSettings(newSettings) =
        settings <- newSettings
    member c.SetTime(time) =
        let frameTime = min (time - lastTime) settings.MaxDeltaTime
        lastTime <- time
        if settings.IsRunning then
            accumulatedTime <- accumulatedTime + frameTime
            accumulatedFixedTime <- accumulatedFixedTime + frameTime
            variableTime <- variableTime + frameTime
            variableDeltaTime <- frameTime
    member c.TakeFixedUpdate() =
        let e = {
            FixedFrameNumber = fixedTime / settings.FixedDeltaTime
            FixedTime = fixedTime
            FixedDeltaTime = settings.FixedDeltaTime
            Time = lastTime
            }
        fixedTime <- fixedTime + settings.FixedDeltaTime
        accumulatedFixedTime <- accumulatedFixedTime - settings.FixedDeltaTime
        e
    /// Should be called after fixed update
    member c.TakeUpdate() =
        let e = {
            FrameNumber = frameCount
            FixedTime = fixedTime - settings.FixedDeltaTime
            FixedDeltaTime = settings.FixedDeltaTime
            Time = variableTime
            DeltaTime = variableDeltaTime
            }
        // Reset variable time rather than reducing by delta
        accumulatedTime <- 0L
        frameCount <- frameCount + 1L
        e
    member c.TryTakeUpdate() =
        if c.HasUpdate then ValueSome (c.TakeUpdate()) else ValueNone
