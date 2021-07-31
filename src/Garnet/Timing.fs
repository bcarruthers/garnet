namespace Garnet.Metrics

open System
open System.Diagnostics

/// Single recorded timing consisting of N operations in a window of time.
[<Struct>]
type Timing = {
    Name : string
    Start : int64
    Stop : int64
    Count : int
    } with
    member t.Duration = t.Stop - t.Start
    override t.ToString() = sprintf "%A, %A to %A (%A), %A ops" t.Name t.Start t.Stop t.Duration t.Count

[<Struct>]
type TimingPoint = {
    Name : string
    Start : int64
    Report : Timing -> unit
    } with
    member p.GetTiming count = {
        Name = p.Name
        Start = p.Start
        Stop = Stopwatch.GetTimestamp()
        Count = count
        }
    /// Ends timing point, reporting elapsed time.
    member p.Stop count = 
        p.Report (p.GetTiming(count))
    member p.Dispose() = 
        p.Stop(1)
    interface IDisposable with
        /// Ends timing point, reporting time for a single sample.
        member p.Dispose() = 
            p.Stop(1)
    override t.ToString() = 
        let stop = Stopwatch.GetTimestamp()
        sprintf "%A, %A to %A (%A)" t.Name t.Start stop (stop - t.Start)
