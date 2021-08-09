namespace Garnet.Samples.Engine

open System
open System.Collections.Generic
open System.Numerics
open Veldrid

[<Struct>]
type MouseWheel = {
    modifiers : int
    wheel : int
    }

[<Struct>]
type MouseUpdate = {
    pos : Vector2
    devicePos : Vector2i
    button1 : bool
    button2 : bool
    }

[<Struct>]
type MouseMoved = {
    pos : Vector2
    delta : Vector2
    devicePos : Vector2i
    deviceDelta : Vector2i
    modifiers : ModifierKeys
    }

[<Struct>]
type MouseDown = {
    pos : Vector2
    devicePos : Vector2i
    button : int
    modifiers : ModifierKeys
    }

[<Struct>]
type MouseUp = {
    pos : Vector2
    devicePos : Vector2i
    button : int
    }

[<Struct>]
type KeyDown = {
    keyCode : Key
    modifiers : ModifierKeys
    }

module KeyDown =
    let none = {
        keyCode = Key.Unknown
        modifiers = ModifierKeys.None
        }

[<Struct>]
type KeyUp = {
    keyCode : Key
    modifiers : ModifierKeys
    }

[<AutoOpen>]
module InputExtensions =
    type ModifierKeys with
        member c.IsShift() = int c = int ModifierKeys.Shift
        member c.IsCtrl() = int c = int ModifierKeys.Control
        member c.IsAlt() = int c = int ModifierKeys.Alt
        member c.HasShift() = int (c &&& ModifierKeys.Shift) <> 0
        member c.HasCtrl() = int (c &&& ModifierKeys.Control) <> 0
        member c.HasAlt() = int (c &&& ModifierKeys.Alt) <> 0

type InputCollection() =
    let keys = Array.zeroCreate (int Key.LastKey)
    let keyUpEvents = List<KeyUp>()
    let keyDownEvents = List<KeyDown>()
    let mouseDownEvents = List<int>()
    let mouseUpEvents = List<int>()
    let mouseButtons = Array.zeroCreate 10
    let mutable mousePos = Vector2i.Zero
    let mutable lastMousePos = Vector2i.Zero
    let mutable normMousePos = Vector2.Zero
    let mutable lastNormMousePos = Vector2.Zero
    let mutable wheelDelta = 0.0f
    member c.MousePosition = mousePos
    member c.LastMousePosition = lastMousePos
    member c.MouseDelta = mousePos - lastMousePos
    member c.NormalizedMousePosition = normMousePos
    member c.LastNormalizedMousePosition = lastNormMousePos
    member c.NormalizedMouseDelta = normMousePos - lastNormMousePos
    member c.WheelDelta = wheelDelta
    member c.KeyUpEvents = keyUpEvents
    member c.KeyDownEvents = keyDownEvents
    member c.MouseUpEvents = mouseUpEvents
    member c.MouseDownEvents = mouseDownEvents
    member c.Modifiers =
        let hasAlt =
            c.IsKeyDown(Key.AltLeft) || 
            c.IsKeyDown(Key.AltRight) ||
            c.IsKeyDown(Key.LAlt) ||
            c.IsKeyDown(Key.RAlt)
        let hasCtrl =
            c.IsKeyDown(Key.ControlLeft) || 
            c.IsKeyDown(Key.ControlRight) ||
            c.IsKeyDown(Key.LControl) ||
            c.IsKeyDown(Key.RControl)
        let hasShift =
            c.IsKeyDown(Key.ShiftLeft) || 
            c.IsKeyDown(Key.ShiftRight) ||
            c.IsKeyDown(Key.LShift) ||
            c.IsKeyDown(Key.RShift)
        (if hasAlt then ModifierKeys.Alt else ModifierKeys.None) |||
        (if hasCtrl then ModifierKeys.Control else ModifierKeys.None) |||
        (if hasShift then ModifierKeys.Shift else ModifierKeys.None)
    member c.IsMouseDown(button) =
        mouseButtons.[button]
    member c.IsMousePressed(button) =
        mouseDownEvents.Contains(button)
    member c.IsKeyDown(code : Key) =
        keys.[int code]
    member c.IsKeyPressed(code, modifiers) =
        keyDownEvents.Contains { 
            keyCode = code
            modifiers = modifiers
            }
    member c.UpdateMouse(newPos, newNormPos, newWheelDelta) =
        wheelDelta <- newWheelDelta
        lastMousePos <- mousePos
        mousePos <- newPos
        lastNormMousePos <- normMousePos
        normMousePos <- newNormPos
    member c.SetMouseButton(button : MouseButton, state) =
        if mouseButtons.[int button] <> state then
            if state then mouseDownEvents.Add(int button)
            else mouseUpEvents.Add(int button)
        mouseButtons.[int button] <- state
    member c.Add(code, modifier) =
        keys.[int code] <- true
        keyDownEvents.Add { 
            keyCode = code
            modifiers = modifier
            }
    member c.Remove(code, modifier) =
        keys.[int code] <- false
        keyUpEvents.Add { 
            keyCode = code
            modifiers = modifier
            }
    member c.Clear() =
        Array.Clear(keys, 0, keys.Length)
    member c.ClearEvents() =
        keyUpEvents.Clear()
        keyDownEvents.Clear()
        mouseDownEvents.Clear()
        mouseUpEvents.Clear()
    override c.ToString() =
        String.Join(", ", keys)

type InputCollection with
    member c.IsKeyDown(code, modifier : ModifierKeys) =
        c.IsKeyDown(code) && int modifier = int c.Modifiers

    member c.IsKeyPressed(code) =
        c.IsKeyPressed(code, ModifierKeys.None)

    member c.IsAnyKeyDown(codes : IReadOnlyList<Key>) =
        let mutable down = false
        for i = 0 to codes.Count - 1 do
            let code = codes.[i]
            down <- down || c.IsKeyDown(code)
        down
        
    member c.IsAnyKeyPressed(codes : IReadOnlyList<Key>, modifiers) =
        let mutable down = false
        for i = 0 to codes.Count - 1 do
            let code = codes.[i]
            down <- down || c.IsKeyPressed(code, modifiers)
        down

    member c.IsAnyKeyPressed(codes) =
        c.IsAnyKeyPressed(codes, ModifierKeys.None)

    member c.UpdateKeysFromSnapshot(snapshot : InputSnapshot) =        
        for i = 0 to snapshot.KeyEvents.Count - 1 do
            let e = snapshot.KeyEvents.[i]
            let m = e.Modifiers
            let key = e.Key
            //printfn "Key change: %A %A %A" key e.Down (float (Stopwatch.GetTimestamp()) / float Stopwatch.Frequency)
            if e.Down then c.Add(key, m)
            else c.Remove(key, e.Modifiers)

    member c.UpdateMouseFromSnapshot(snapshot : InputSnapshot, viewSize : Vector2i) =        
        let mousePos = Vector2i(int snapshot.MousePosition.X, int snapshot.MousePosition.Y)
        let normMousePosition =
            let v = snapshot.MousePosition / viewSize.ToVector2() * 2.0f - Vector2.One
            Vector2(v.X, -v.Y)       
        c.UpdateMouse(mousePos, normMousePosition, snapshot.WheelDelta)
        for e in snapshot.MouseEvents do
            let button = e.MouseButton
            c.SetMouseButton(button, e.Down)
