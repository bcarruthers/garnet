module Garnet.Samples.Performance

open System

[<Struct>]
type Stats = {
    samples : int
    weight : float
    total : float
    min : float
    max : float
    mean : float
    m2 : float
}
    
module Stats =
    let empty = {
        samples = 0
        weight = 0.0
        total = 0.0
        min = Double.MaxValue
        max = Double.MinValue
        mean = 0.0
        m2 = 0.0 
        }

    let variance s =
        if s.samples < 2 || s.weight <= 0.0 then 0.0
        else s.m2 / s.weight
        
    let stdDev s = 
        sqrt (variance s)

    let incrementWeighted stats x weight =
        let newWeight = weight + stats.weight
        let delta = x - stats.mean
        let r = delta * weight / newWeight
        {
            samples = stats.samples + 1
            total = stats.total + x * weight
            min = min stats.min x
            max = max stats.max x
            mean = stats.mean + r
            m2 = stats.m2 + stats.weight * delta * r
            weight = newWeight
        }

    let increment stats x = 
        incrementWeighted stats x 1.0

    let fromSeq (s : seq<float>) =
        s |> Seq.fold increment empty

    let format s =
        sprintf "n %A, weight %A, total %A, min %A, max %A, mean %A, sd %A"
            s.samples s.weight s.total s.min s.max s.mean (stdDev s)    

[<Struct>]
type GCState = {
    totalMemory : int64
    gc0 : int
    gc1 : int
    gc2 : int
    }

module GCState =
    let zero = {
        totalMemory = 0L
        gc0 = 0
        gc1 = 0
        gc2 = 0
        }

    let retrieve() = {
        totalMemory = GC.GetTotalMemory(false)
        gc0 = GC.CollectionCount(0)
        gc1 = GC.CollectionCount(1)
        gc2 = GC.CollectionCount(2)
        }

    let isNonZeroGC state =
        state.gc0 > 0 ||
        state.gc1 > 0 ||
        state.gc2 > 0

    let getDelta a b = {
        totalMemory = b.totalMemory - a.totalMemory
        gc0 = b.gc0 - a.gc0
        gc1 = b.gc1 - a.gc1
        gc2 = b.gc2 - a.gc2
        }
            
    let formatWithDelta current delta =
        sprintf "gen0 %d (%+d), gen1 %d (%+d), gen2 %d (%+d), mem %d (%+d)" 
            current.gc0 delta.gc0 current.gc1 delta.gc1 current.gc2 delta.gc2 
            current.totalMemory delta.totalMemory
                
type FpsMonitor() =
    let fpsInterval = 2000
    let mutable lastTime = DateTime.UtcNow
    let mutable elapsed = 0
    let mutable stats = Stats.empty
    let mutable gcState = GCState.retrieve()
    member c.Update() =
        let time = DateTime.UtcNow
        let deltaTime = int (time - lastTime).TotalMilliseconds
        lastTime <- time
        stats <- Stats.increment stats (float deltaTime)
        elapsed <- elapsed + deltaTime
        if elapsed >= fpsInterval then
            let fps = stats.samples * 1000 / elapsed
            let newState = GCState.retrieve()
            let delta = GCState.getDelta gcState newState
            printfn "FPS: %d, mean: %d ms, sd: %d, min: %d, max: %d, GC: %s%s" 
                fps (int stats.mean) (int (Stats.stdDev stats)) 
                (int stats.min) (int stats.max)
                (GCState.formatWithDelta newState delta)
                (if delta.gc2 > 0 then " **" elif delta.gc1 > 0 then " *" else "")
            stats <- Stats.empty
            elapsed <- elapsed - fpsInterval
            gcState <- newState
