namespace Garnet.Samples.Engine

open System.Diagnostics

[<Struct>]
type Update = {
    frameNumber : int64
    fixedTime : int64
    fixedDeltaTime : int64
    time : int64
    deltaTime : int64
    }

[<Struct>]
type FixedUpdate = {
    fixedFrameNumber : int64
    fixedTime : int64
    fixedDeltaTime : int64
    time : int64
    }
    
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
        
type FixedUpdateTimer(fixedDeltaTime, maxDeltaTime) =
    let mutable lastTime = 0L
    let mutable accumulatedTime = 0L
    let mutable variableTime = 0L
    let mutable variableDeltaTime = 0L
    let mutable fixedTime = 0L
    let mutable frameCount = 0L
    new(fixedDeltaTime) =
        FixedUpdateTimer(fixedDeltaTime, 250L)
    member val IsRunning = false with get, set
    member c.HasFixedUpdate =
        accumulatedTime >= fixedDeltaTime && c.IsRunning 
    member c.SetTime(time) =
        let frameTime = min (time - lastTime) maxDeltaTime
        lastTime <- time
        if c.IsRunning then
            accumulatedTime <- accumulatedTime + frameTime
            variableTime <- variableTime + frameTime
            variableDeltaTime <- frameTime
    member c.TakeFixedUpdate() =
        let e = {
            fixedFrameNumber = fixedTime / fixedDeltaTime
            fixedTime = fixedTime
            fixedDeltaTime = fixedDeltaTime
            time = lastTime
            }
        fixedTime <- fixedTime + fixedDeltaTime
        accumulatedTime <- accumulatedTime - fixedDeltaTime
        e
    /// Should be called after fixed update
    member c.TakeUpdate() =
        let e = {
            frameNumber = frameCount
            fixedTime = fixedTime - fixedDeltaTime
            fixedDeltaTime = fixedDeltaTime
            time = variableTime
            deltaTime = variableDeltaTime
            }
        frameCount <- frameCount + 1L
        e
