module Garnet.Tests.StateMachine

open Garnet.Composition
open Expecto

module Partition =
    let globals = 0
    let objects = 1

[<Struct>]
type WorldStatus =
    | WorldInactive
    | WorldActive

type CreateWorld = struct end
type Update = struct end

module WorldSystems =
    let inactive (c : Container) =
        c.On<CreateWorld> <| fun e ->
            // populate world
            c.Create(Partition.objects).Add<int>(1)
            // switch to active state
            c.Send<WorldStatus> WorldActive
        
    let active (c : Container) =
        let update =
            fun param (x : int) -> x + 1
            |> Join.update1
            |> Join.over c               
        Disposable.Create [
            c.On<Update> <| fun e ->
                update()
        ]

    /// Maps a state value to a registration for the state
    let getState (status : WorldStatus) =
        match status with
        | WorldInactive -> inactive
        | WorldActive -> active
    
    let registerCommon (c : Container) =
        // register systems common across all states here
        Disposable.Null

    let register (c : Container) =
        Disposable.Create [
            registerCommon c
            // register so that state is stored in a globals entity
            // and we start in inactive state
            c.RegisterStateMachine(WorldInactive, getState)
            ]

[<Tests>]
let tests =
    testList "state machine" [
        testCase "create state machine container" <| fun () ->
            let c = Container()
            let sub = WorldSystems.register c
            c.Run(Update())
            c.Get<int>().Count |> shouldEqual 0
            c.Run(CreateWorld())
            c.Get<int>().Count |> shouldEqual 1
    ]