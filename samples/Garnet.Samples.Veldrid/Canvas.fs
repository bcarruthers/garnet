namespace Garnet.Samples.Veldrid

open System
open System.Numerics
open System.Collections.Generic

type VertexElement =
    | Float1 = 0uy
    | Float2 = 1uy
    | Float3 = 2uy
    | Float4 = 3uy
    | Byte2_Norm = 4uy
    | Byte2 = 5uy
    | Byte4_Norm = 6uy
    | Byte4 = 7uy
    | SByte2_Norm = 8uy
    | SByte2 = 9uy
    | SByte4_Norm = 10uy
    | SByte4 = 11uy
    | UShort2_Norm = 12uy
    | UShort2 = 13uy
    | UShort4_Norm = 14uy
    | UShort4 = 15uy
    | Short2_Norm = 16uy
    | Short2 = 17uy
    | Short4_Norm = 18uy
    | Short4 = 19uy
    | UInt1 = 20uy
    | UInt2 = 21uy
    | UInt3 = 22uy
    | UInt4 = 23uy
    | Int1 = 24uy
    | Int2 = 25uy
    | Int3 = 26uy
    | Int4 = 27uy
    | Half1 = 28uy
    | Half2 = 29uy
    | Half4 = 30uy

type VertexDescriptor<'v
                when 'v : struct 
                and 'v : (new : unit -> 'v) 
                and 'v :> ValueType> = {
    getPosition : 'v -> Vector3
    vertexElements : VertexElement list
    }

type BlendState =
    | Additive = 1
    | Alpha = 2
    | Override = 3

module internal Buffer =
    let inline private log2 x =
        let mutable log = 0
        let mutable y = x
        while y > 1 do
            y <- y >>> 1
            log <- log + 1;
        log

    let inline private nextLog2 x =
        let log = log2 x
        if x - (1 <<< log) > 0 then 1 + log else log

    let inline internal getRequiredCount count =
        1 <<< nextLog2 count

    let inline internal resizeArray count (arr : byref<_[]>) =
        let required = getRequiredCount count
        let newArr = Array.zeroCreate required
        arr.CopyTo(newArr, 0)
        arr <- newArr

    let inline internal append (arr : byref<_[]>) (count : byref<int>) x =
        count <- count + 1
        if count > arr.Length then
            resizeArray count &arr
        arr.[count - 1] <- x

    let inline internal appendAll (arr : byref<_[]>) (count : byref<int>) (src : _[]) srcCount =
        let destOffset = count
        count <- count + srcCount
        if count > arr.Length then
            resizeArray count &arr
        Array.Copy(src, 0, arr, destOffset, srcCount)
        
module internal Picking =
    let triContainsPoint (p : Vector2) (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) =
        let A = 0.5f * (-p1.Y * p2.X + p0.Y * (-p1.X + p2.X) + p0.X * (p1.Y - p2.Y) + p1.X * p2.Y)
        let sign = if A < 0.0f then -1.0f else 1.0f;
        let s = (p0.Y * p2.X - p0.X * p2.Y + (p2.Y - p0.Y) * p.X + (p0.X - p2.X) * p.Y) * sign
        let t = (p0.X * p1.Y - p0.Y * p1.X + (p0.Y - p1.Y) * p.X + (p1.X - p0.X) * p.Y) * sign
        //s > 0.0f && t > 0.0f && (s + t) < 2.0f * A * sign
        s >= 0.0f && t >= 0.0f && (s + t) <= 2.0f * A * sign

    let rectContainsPoint (min : Vector2) (max : Vector2) (p : Vector2) =
        p.X >= min.X && p.X <= max.X &&
        p.Y >= min.Y && p.Y <= max.Y

type IDeviceBuffer =
    abstract Write<'v
            when 'v : struct 
            and 'v : (new : unit -> 'v) 
            and 'v :> ValueType> : 'v[] * int -> unit
            
type IStagingBuffer =
    abstract WriteTo : IDeviceBuffer -> int

type StagingBuffer<'v
                when 'v : struct 
                and 'v : (new : unit -> 'v) 
                and 'v :> ValueType>(capacity) =
    let mutable array = Array.zeroCreate<'v> capacity
    let mutable count = 0
    member c.Item 
        with get i = array.[i]
        and set i x = array.[i] <- x
    member c.Array = array
    member c.Count = count
    member c.Capacity = array.Length
    member c.Clear() = count <- 0
    member c.Add x = 
        count <- count + 1
        if count > array.Length then
            Buffer.resizeArray count &array
        array.[count - 1] <- x
    member c.WriteTo (target : IDeviceBuffer) =
        target.Write(array, count)
        count
    interface IStagingBuffer with
        member c.WriteTo target =
            c.WriteTo target

type Mesh<'v
        when 'v : struct 
        and 'v : (new : unit -> 'v) 
        and 'v :> ValueType>(getPosition) =
    let vertices = StagingBuffer<'v>(0)
    let indices = StagingBuffer<uint32>(0)
    let sizes = StagingBuffer<int32>(0)
    member c.Vertices = vertices
    member c.Indices = indices
    member c.Sizes = sizes
    member c.Clear() =
        vertices.Clear()
        indices.Clear()
    member c.Pick(p) =
        let mutable s = 0
        let mutable i = 0
        let mutable result = -1
        while result < 0 && s < sizes.Count do
            let size = sizes.[s]
            let mutable j = 0
            while result < 0 && j < size do
                let i0 = indices.[i + 0]
                let i1 = indices.[i + 1]
                let i2 = indices.[i + 2]
                let p0 = getPosition vertices.[int i0]
                let p1 = getPosition vertices.[int i1]
                let p2 = getPosition vertices.[int i2]
                if Picking.triContainsPoint p p0 p1 p2 then result <- s
                i <- i + 3
                j <- j + 1
            s <- s + 1
        result
    member c.PickRect(param, min : Vector2, max : Vector2, action) =
        let mutable s = 0
        let mutable i = 0
        while s < sizes.Count do
            let size = sizes.[s]
            let mutable j = 0
            let mutable result = false
            while not result && j < size do
                let i0 = indices.[i + 0]
                let i1 = indices.[i + 1]
                let i2 = indices.[i + 2]
                let p0 = getPosition vertices.[int i0]
                let p1 = getPosition vertices.[int i1]
                let p2 = getPosition vertices.[int i2]
                result <- 
                    Picking.rectContainsPoint min max p0 ||
                    Picking.rectContainsPoint min max p1 ||
                    Picking.rectContainsPoint min max p2
                i <- i + 3
                j <- j + 1
            if result then
                action param s
            s <- s + 1
            
type Mesh<'v
    when 'v : struct 
    and 'v : (new : unit -> 'v) 
    and 'v :> ValueType> with
    member c.BeginTriangle() =
        let indices = c.Indices
        let vi = uint32 c.Vertices.Count
        c.Sizes.Add 1
        indices.Add (vi + 0u)
        indices.Add (vi + 1u)
        indices.Add (vi + 2u)

    member c.BeginQuad() =
        let indices = c.Indices
        let vi = uint32 c.Vertices.Count
        c.Sizes.Add 2
        indices.Add (vi + 0u)
        indices.Add (vi + 1u)
        indices.Add (vi + 2u)
        indices.Add (vi + 0u)
        indices.Add (vi + 2u)
        indices.Add (vi + 3u)

type LayerDescriptor<'v
            when 'v : struct 
            and 'v : (new : unit -> 'v) 
            and 'v :> ValueType> = {
    layerId : int
    viewportId : int
    vertexDescriptor : VertexDescriptor<'v>
    blendState : BlendState
    vertexShader : string
    fragmentShader : string
    texture : string
    pickMask : uint32
    }

type LayerDescriptor = {
    vertexSize : int
    vertexElements : VertexElement list
    blendState : BlendState
    layerId : int
    vertexShader : string
    fragmentShader : string
    texture : string
    pickMask : uint32
    }

[<Struct>]
type LayerPickResult = {
    layerId : int
    objectIndex : int
    }
    
type ILayer =
    abstract Descriptor : LayerDescriptor
    abstract Vertices : IStagingBuffer
    abstract Indices : IStagingBuffer
    abstract Clear : unit -> unit
    abstract Pick : Vector2 * uint32 -> LayerPickResult
    abstract PickRect<'a> : 'a * Vector2 * Vector2 * uint32 * ('a -> int -> unit) -> unit
    //abstract WriteTo : IDeviceBuffer * IDeviceBuffer -> int

[<Struct>]
type LayerTransforms = {
    projection : Matrix4x4
    view : Matrix4x4
    world : Matrix4x4
    }

type RenderBatch = {
    transform : Matrix4x4
    objectCount : int
    vertexCount : int
    indexCount : int
    }

type Layer<'v
        when 'v : struct 
        and 'v : (new : unit -> 'v) 
        and 'v :> ValueType>(desc : LayerDescriptor<'v>) =
    let mesh = 
        let getPos v = 
            let p = desc.vertexDescriptor.getPosition v
            Vector2(p.X, p.Y)
        Mesh<'v>(getPos)
    member c.Mesh = mesh
    interface ILayer with
        member c.Descriptor = {
            vertexElements = desc.vertexDescriptor.vertexElements
            vertexSize = sizeof<'v>
            layerId = desc.layerId
            vertexShader = desc.vertexShader
            fragmentShader = desc.fragmentShader
            blendState = desc.blendState
            texture = desc.texture
            pickMask = desc.pickMask
            }
        member c.Vertices = mesh.Vertices :> IStagingBuffer
        member c.Indices = mesh.Indices :> IStagingBuffer
        member c.Clear() =
            mesh.Clear()
        member c.PickRect(param, min, max, mask, action) =
            if desc.pickMask &&& mask <> 0u then
                mesh.PickRect(param, min, max, action)
        member c.Pick(p, mask) =
            let mutable result : LayerPickResult = { layerId = -1; objectIndex = -1 }
            if desc.pickMask &&& mask <> 0u then
                let index = mesh.Pick(p)
                if index >= 0 then
                    result <- { layerId = desc.layerId; objectIndex = index }
            result
        //member c.WriteTo(vertices, indices) =
        //    mesh.Vertices.WriteTo vertices |> ignore
        //    mesh.Indices.WriteTo indices

type Viewport() =
    let layers = List<ILayer>()
    member val Projection = Matrix4x4.Identity with get, set
    member val View = Matrix4x4.Identity with get, set
    member val ClearColor = Vector4.Zero with get, set
    member c.Layers = layers
    member c.AddLayer layer =
        layers.Add layer

type Canvas() =
    let viewports = Dictionary<int, _>()
    let layers = Dictionary<int, ILayer>()
    member c.Viewports = viewports.Values
    member c.Layers = layers.Values
    /// Get or add a viewport
    member c.GetViewport(viewportId) =
        match viewports.TryGetValue viewportId with
        | true, x -> x
        | false, _ ->
            let viewport = new Viewport()
            viewports.Add(viewportId, viewport)
            viewport
    /// Get or add a layer
    member c.GetLayer<'v
                when 'v : struct 
                and 'v : (new : unit -> 'v) 
                and 'v :> ValueType            
                >(desc : LayerDescriptor<'v>) =
        match layers.TryGetValue desc.layerId with
        | true, x -> x :?> Layer<'v>
        | false, _ ->
            // create layer
            let layer = new Layer<'v>(desc)
            layers.Add(desc.layerId, layer)
            // add to viewport
            let viewport = c.GetViewport(desc.viewportId)
            viewport.AddLayer layer
            layer
    member c.Clear() =
        for layer in layers.Values do
            layer.Clear()
