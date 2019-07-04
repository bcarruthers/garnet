namespace Garnet

open System
open System.Diagnostics

/// Single recorded timing consisting of N operations in a window of time.
[<Struct>]
type Timing = {
    name : string
    start : int64
    stop : int64
    count : int
    } with
    member t.Duration = t.stop - t.start
    override t.ToString() = sprintf "%A, %A to %A (%A), %A ops" t.name t.start t.stop t.Duration t.count

module Timing =
    let inline getTimestamp() = Stopwatch.GetTimestamp()
    let ticksPerMs = Stopwatch.Frequency / 1000L
    let msPerTick = 1.0f / (float32 ticksPerMs)

    let init name start stop count = {
        name = name
        start = start
        stop = stop
        count = count
        }
        
    let one name start stop  =
        init name start stop 1

    let oneEndingNow name start stop  =
        init name start (getTimestamp()) 1

[<Struct>]
type TimingPoint = {
    name : string
    start : int64
    report : Timing -> unit
    } with
    member p.GetTiming count = {
        name = p.name
        start = p.start
        stop = Timing.getTimestamp()
        count = count
        }
    /// Ends timing point, reporting elapsed time.
    member p.Stop count = 
        p.report (p.GetTiming(count))
    interface IDisposable with
        /// Ends timing point, reporting time for a single sample.
        member p.Dispose() = p.Stop(1)
    override t.ToString() = 
        let stop = Timing.getTimestamp()
        sprintf "%A, %A to %A (%A)" t.name t.start stop (stop - t.start)
