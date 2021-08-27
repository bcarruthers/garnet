namespace Garnet.Composition

open System.Collections.Generic
open Garnet.Composition.Comparisons
open Garnet.Composition

/// Useful for linking response messages back to their corresponding
/// request message.
[<Struct>]
type RequestId = RequestId of uint64
    with static member Undefined = RequestId 0UL

[<AutoOpen>]
module RequestTrackingExtensions =
    type private RequestTracker() =
        let requests = Dictionary<RequestId, int>()
        let mutable count = 0UL
        member c.Clear() =
            requests.Clear()
        member c.Create(waitingCount) =
            // start at one
            count <- count + 1UL
            let requestId = RequestId count
            requests.Add(requestId, waitingCount)
            requestId
        member c.Complete(requestId) =
            match requests.TryGetValue(requestId) with
            | false, _ -> false
            | true, waitingCount ->
                if waitingCount = 1 then                     
                    // if this is the last response, request is complete
                    requests.Remove(requestId) |> ignore
                    true
                else
                    // otherwise decrement counter
                    requests.[requestId] <- waitingCount - 1
                    false            
        
    type Container with
        member c.CreateRequest(waitingCount) =
            c.GetValue<RequestTracker>().Create(waitingCount)

        member c.ClearRequests() =
            c.GetValue<RequestTracker>().Clear()

        member c.CompleteRequest(requestId) =
            c.GetValue<RequestTracker>().Complete(requestId)
