namespace Garnet.Graphics

open System
open System.Collections.Generic
open System.Buffers
open System.Numerics
open Garnet.Numerics

[<Struct>]
type PickLayerDescriptor = {
    LayerId : int
    CameraId : int
    Primitive : Primitive
    FlushMode : SpriteFlushMode
    }

[<Struct>]
type PickResult = {
    LayerId : int
    PrimitiveIndex : int
    WorldPosition : Vector2
    }

[<Struct>]
type PickResult<'a> = {
    Param : 'a
    LayerId : int
    PrimitiveIndex : int
    }

type VertexPicking() =
    /// Returns index of first vertex contained in rect
    static member TryPickPoint(span : ReadOnlySpan<Vector2>, rect : Range2) = 
        let mutable result = ValueNone
        let mutable i = 0
        while result.IsNone && i < span.Length do
            let v = span.[i]
            if rect.Contains(v) then
                result <- ValueSome i
            i <- i + 1
        result

    /// Returns index of first triangle containing point
    static member TryPickTriangle(span : ReadOnlySpan<Vector2>, p : Vector2) = 
        let mutable result = ValueNone
        let mutable i = 0
        while result.IsNone && i < span.Length do
            let v0 = span.[i + 0]
            let v1 = span.[i + 1]
            let v2 = span.[i + 2]
            if p.IsInTriangle(v0, v1, v2) then
                result <- ValueSome (i / 3)
            i <- i + 3
        result
        
    /// Returns index of first quad containing point
    static member TryPickQuad(span : ReadOnlySpan<Vector2>, p : Vector2) = 
        let mutable result = ValueNone
        let mutable i = 0
        while result.IsNone && i < span.Length do
            let v0 = span.[i + 0]
            let v1 = span.[i + 1]
            let v2 = span.[i + 2]
            let v3 = span.[i + 3]
            if p.IsInTriangle(v0, v1, v2) || p.IsInTriangle(v0, v2, v3) then
                result <- ValueSome (i / 4)
            i <- i + 4
        result

type IPickLayer =
    abstract Descriptor : PickLayerDescriptor
    abstract WrittenVertexSpan : ReadOnlySpan<Vector2>
    abstract Clear : unit -> unit
 
type PickLayer<'a>(desc : PickLayerDescriptor) =
    let values = ArrayBufferWriter<'a>()
    let vertices = ArrayBufferWriter<Vector2>()
    member c.Descriptor = desc
    member c.Values = values :> IBufferWriter<'a>
    member c.VertexWriter = vertices :> IBufferWriter<Vector2>
    member c.WrittenValueSpan = values.WrittenSpan
    member c.WrittenVertexSpan = vertices.WrittenSpan
    member c.Clear() =
        values.Clear()
        vertices.Clear()
    interface IPickLayer with
        member c.Descriptor = desc
        member c.WrittenVertexSpan = c.WrittenVertexSpan
        member c.Clear() = c.Clear()

type PickLayerSet() =
    let layers = List<IPickLayer voption>()
    member c.GetLayer<'a>(desc : PickLayerDescriptor) =
        while layers.Count <= desc.LayerId do
            layers.Add(ValueNone)
        match layers.[desc.LayerId] with
        | ValueNone ->
            let layer = PickLayer<'a>(desc)
            layers.[desc.LayerId] <- ValueSome (layer :> IPickLayer)
            layer
        | ValueSome layer -> layer :?> PickLayer<'a>
    member c.GetValue<'a>(layerId, primitiveIndex) =
        let layer = layers.[layerId].Value :?> PickLayer<'a>
        layer.WrittenValueSpan.[primitiveIndex]
    member c.TryPick(cameras : CameraSet, layerId, normPos : Vector2) =
        if layerId >= layers.Count then ValueNone
        else
            match layers.[layerId] with
            | ValueNone -> ValueNone
            | ValueSome layer ->
                let span = layer.WrittenVertexSpan
                let viewport = cameras.[layer.Descriptor.CameraId]
                let worldPos = viewport.NormalizedToWorld(normPos)
                let primitiveResult =
                    match layer.Descriptor.Primitive with
                    | Triangle -> VertexPicking.TryPickTriangle(span, worldPos)
                    | Quad -> VertexPicking.TryPickQuad(span, worldPos)
                match primitiveResult with
                | ValueNone -> ValueNone
                | ValueSome index ->
                    ValueSome {
                        LayerId = layerId
                        PrimitiveIndex = index
                        WorldPosition = worldPos
                        }
    /// Returns index of primitive containing point, if any
    member c.TryPick(cameras : CameraSet, normPos : Vector2) =
        let mutable result = ValueNone
        let mutable i = layers.Count - 1
        while result.IsNone && i >= 0 do
            result <- c.TryPick(cameras, i, normPos)
            i <- i - 1
        result
    member c.PickAll(param, cameras : CameraSet, layerId, normRect : Range2, action) =
        if layerId < layers.Count then
            match layers.[layerId] with
            | ValueNone -> ()
            | ValueSome layer ->
                let span = layer.WrittenVertexSpan
                let vertsPerPrimitive = Primitive.GetVertexCount(layer.Descriptor.Primitive)
                let viewport = cameras.[layer.Descriptor.CameraId]
                let worldRect = viewport.NormalizedToWorld(normRect)
                // Scan vertices
                let mutable vi = 0
                while vi < span.Length do
                    let v = span.[vi]
                    if worldRect.Contains(v) then
                        action {
                            Param = param
                            LayerId = layerId
                            PrimitiveIndex = vi / vertsPerPrimitive
                            }
                    vi <- vi + 1
    /// Returns index of primitive with a vertex contained within rect, if any
    member c.PickAll(param, cameras : CameraSet, normRect : Range2, action) =
        let mutable i = layers.Count - 1
        while i >= 0 do
            c.PickAll(param, cameras, i, normRect, action)
            i <- i - 1
    member c.Flush() =
        for i = 0 to layers.Count - 1 do
            match layers.[i] with
            | ValueNone -> ()
            | ValueSome layer ->
                match layer.Descriptor.FlushMode with
                | NoFlush -> ()
                | FlushOnDraw -> layer.Clear()
