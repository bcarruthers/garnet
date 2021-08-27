namespace Garnet.Composition

open Garnet.Numerics

type Start = struct end    
type Closing = struct end

[<Struct>]
type HandleInput = {
    Time : int64
    }

[<Struct>]
type Schedule = {
    DueTime : int64
    }

[<Struct>]
type Tick = {
    Time : int64
    }

[<Struct>]
type Update = {
    FrameNumber : int64
    FixedTime : int64
    FixedDeltaTime : int64
    Time : int64
    DeltaTime : int64
    }

[<Struct>]
type PreUpdate = {
    Update : Update
    }
    
[<Struct>]
type PostUpdate = {
    Update : Update
    }

[<Struct>]
type FixedUpdate = {
    FixedFrameNumber : int64
    FixedTime : int64
    FixedDeltaTime : int64
    Time : int64
    }

[<Struct>]
type Draw = {
    ViewSize : Vector2i
    Update : Update
    }
    
type PushDrawCommands = struct end
