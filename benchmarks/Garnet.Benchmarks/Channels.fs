module Garnet.Benchmarks.Channels

open Garnet.Ecs

let run iterations sendFreq =
    let mutable sum = 0L
    let c = Channels()
    let channel = c.GetChannel<int>()
    let sub =
        c.On<int> <| fun e ->
            sum <- sum + int64 e
            if e < iterations then
                if e % sendFreq = 0 then channel.Send (e + 1)
                else channel.Publish (e + 1)
    c.Publish 1
    c.Commit()
    let mutable count = 0
    while c.Publish() do
        c.Commit()
        count <- count + 1
    sum, count

let runBatches iterations batchSize =
    let mutable sum = 0L
    let mutable count = 0
    let c = Channels()
    let channel = c.GetChannel<int>()
    let sub =
        c.OnAll<int> <| fun list ->
            for e in list do
                sum <- sum + int64 e
            if count < iterations then
                count <- count + 1
                for e in list do
                    channel.Send e
    for i = 1 to batchSize do
        c.Send i
    c.Commit()
    while c.Publish() do
        c.Commit()
    sum, count
