namespace Garnet.Composition

open System
open System.Collections.Generic
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Threading
open System.Runtime.InteropServices
open Garnet.Composition

[<Extension>]
type Looping =
    [<Extension>]
    static member RunLoop(c : Container, [<Optional; DefaultParameterValue(1)>] sleepDuration) =
        let mutable running = true
        use sub = c.On<Closing> <| fun _ -> running <- false
        c.Run(Start())
        let sw = Stopwatch.StartNew()
        while running do
            c.Run<Tick> { Time = sw.ElapsedMilliseconds }
            // Sleep to avoid spinning CPU
            if sleepDuration >= 0 then
                Thread.Sleep(sleepDuration)
        
    [<Extension>]
    static member RunStartup(c : ActorSystem, loopActorId : ActorId, destIds : ActorId seq) =
        // Create error handler to collect exceptions
        let exceptions = List<Exception>()
        use sub = c.RegisterExceptionHandler(fun ex -> exceptions.Add(ex))
        // Send start message to all root actors
        let destIds = ReadOnlySpan(destIds |> Seq.toArray)
        c.SendToAll<Start>(destIds, Start(), loopActorId)        
        // Wait for all threads to complete
        c.ProcessAll()
        if exceptions.Count > 0 then
            raise (AggregateException("Startup failed", exceptions))

    [<Extension>]
    static member RunLoop(c : ActorSystem, loopActorId : ActorId, destActorId : ActorId, [<Optional; DefaultParameterValue(1)>] sleepDuration) =
        let exceptions = List<Exception>()
        let mutable running = true
        // Stop on exception
        use sub = c.RegisterExceptionHandler(fun ex ->
            exceptions.Add(ex)
            running <- false)
        // Stop if closing message received
        c.Register(loopActorId, fun (c : Container) ->
            c.On<Closing> <| fun _ -> running <- false)
        // Run loop
        let sw = Stopwatch.StartNew()
        while running do
            // Send tick to destination actor with source as loop actor to indicate
            // where closing messages should be sent.
            let e = { Time = sw.ElapsedMilliseconds }
            c.Send<Tick>(destActorId, e, loopActorId)
            c.Process()
            // Sleep to avoid spinning CPU
            if sleepDuration >= 0 then
                Thread.Sleep(sleepDuration)
        // Wait for all threads to complete
        c.ProcessAll()
        if exceptions.Count > 0 then
            raise (AggregateException("Updating failed", exceptions))
