namespace Garnet.Samples.Engine

type Start = struct end    
type Stop = struct end    
    
[<Struct>]
type Update = {
    frameNumber : int64
    fixedTime : int64
    fixedDeltaTime : int64
    time : int64
    deltaTime : int64
    }

[<Struct>]
type PreUpdate = {
    update : Update
    }
    
[<Struct>]
type PostUpdate = {
    update : Update
    }
    
[<Struct>]
type FixedUpdate = {
    fixedFrameNumber : int64
    fixedTime : int64
    fixedDeltaTime : int64
    time : int64
    }
    
[<Struct>]
type ClockUpdate = {
    time : int64
    }
    
[<Struct>]
type ClockRequest = {
    dueTime : int64
    }

[<Struct>]
type Closing = struct end
