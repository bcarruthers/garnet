namespace Garnet.Graphics

open System
open System.Buffers
open Veldrid
open Garnet.Collections

type ResizableDeviceBuffer(device : GraphicsDevice, elementSize, usage) =
    let mutable buffer = device.ResourceFactory.CreateBuffer(BufferDescription(uint32 (elementSize * 8), usage))
    member c.Buffer = buffer
    member c.Write<'v
            when 'v : struct 
            and 'v : (new : unit -> 'v) 
            and 'v :> ValueType>(src : ReadOnlyMemory<'v>) =
        // ensure device buffer is large enough
        let size = src.Length * elementSize
        if buffer.SizeInBytes < uint32 size then
            // destroy old buffer
            buffer.Dispose()
            // round up to pow2 number of elements (not bytes)
            let requiredSize = Buffer.getRequiredCount src.Length * elementSize
            let desc = BufferDescription(uint32 requiredSize, usage)    
            buffer <- device.ResourceFactory.CreateBuffer(desc)
        // write data
        use handle = src.Pin()
        device.UpdateBuffer(buffer, 0u, IntPtr handle.Pointer, uint32 size)
    member c.Dispose() =
        buffer.Dispose()
    interface IDisposable with 
        member c.Dispose() = c.Dispose()

type DeviceMesh(device, vertexSize) =
    let vb = new ResizableDeviceBuffer(device, vertexSize, BufferUsage.Dynamic ||| BufferUsage.VertexBuffer)
    let ib = new ResizableDeviceBuffer(device, sizeof<uint32>, BufferUsage.Dynamic ||| BufferUsage.IndexBuffer)
    member c.WriteVertices(src) = vb.Write(src)
    member c.WriteIndexes(src) = ib.Write(src)
    member c.Draw(cmds : CommandList, indexCount) =
        if indexCount > 0 then
            cmds.SetVertexBuffer(0u, vb.Buffer)
            cmds.SetIndexBuffer(ib.Buffer, IndexFormat.UInt32)
            cmds.DrawIndexed(
                indexCount = uint32 indexCount,
                instanceCount = 1u,
                indexStart = 0u,
                vertexOffset = 0,
                instanceStart = 0u)
    member c.Dispose() =
        vb.Dispose()
        ib.Dispose()

type QuadIndexBuffer(device : GraphicsDevice) =
    let indexesPer = 6
    let verticesPer = 4
    let elementSize = sizeof<uint32>
    let usage = BufferUsage.Dynamic ||| BufferUsage.IndexBuffer
    let mutable buffer = device.ResourceFactory.CreateBuffer(BufferDescription(uint32 (elementSize * 8), usage))
    member private c.Update(requestedCount) =
        let bytesPer = elementSize * indexesPer
        let bufferedCount = int buffer.SizeInBytes / bytesPer
        if bufferedCount < requestedCount then
            // Round up to pow2 number of elements (not bytes)
            let requiredCount = Buffer.getRequiredCount requestedCount
            let requiredIndexes = requiredCount * indexesPer
            let requiredBytes = requiredCount * bytesPer
            // Generate indexes for primitive
            let arr = ArrayPool<int>.Shared.Rent(requiredIndexes)
            for i = 0 to requiredCount - 1 do
                let vi = i * verticesPer
                let ii = i * indexesPer
                arr.[ii + 0] <- vi + 0
                arr.[ii + 1] <- vi + 1
                arr.[ii + 2] <- vi + 2
                arr.[ii + 3] <- vi + 0
                arr.[ii + 4] <- vi + 2
                arr.[ii + 5] <- vi + 3
            // Destroy old device buffer and create new one
            buffer.Dispose()
            let desc = BufferDescription(uint32 requiredBytes, usage)    
            buffer <- device.ResourceFactory.CreateBuffer(desc)
            // Write data to device buffer
            let src = ReadOnlyMemory(arr)
            use handle = src.Pin()            
            device.UpdateBuffer(buffer, 0u, IntPtr handle.Pointer, uint32 requiredBytes)
            ArrayPool<int>.Shared.Return(arr)
    member c.Draw(cmds : CommandList, primitiveCount) =
        if primitiveCount > 0 then
            c.Update(primitiveCount)
            cmds.SetIndexBuffer(buffer, IndexFormat.UInt32)
            cmds.DrawIndexed(
                indexCount = uint32 (primitiveCount * indexesPer),
                instanceCount = 1u,
                indexStart = 0u,
                vertexOffset = 0,
                instanceStart = 0u)
    member c.Dispose() = buffer.Dispose()
    interface IDisposable with 
        member c.Dispose() = c.Dispose()

type IVertexBuffer =
    inherit IDisposable
    abstract VertexCount : int
    abstract SetVertexBuffer : CommandList -> unit
    abstract Flush : unit -> unit

type VertexBuffer<'v
                when 'v : struct 
                and 'v : (new : unit -> 'v) 
                and 'v :> ValueType>(device) =
    let vertices = ArrayBufferWriter<'v>()
    let vb = new ResizableDeviceBuffer(device, sizeof<'v>, BufferUsage.Dynamic ||| BufferUsage.VertexBuffer)
    let mutable vertexCount = 0
    member c.VertexCount = vertexCount
    member c.WrittenSpan = vertices.WrittenSpan
    member c.GetMemory(count) =
        vertices.GetMemory(count)
    member c.GetSpan(count) =
        vertices.GetSpan(count)
    member c.Advance(count) =
        vertices.Advance(count)
    member c.Flush() =
        vertexCount <- vertices.WrittenCount
        vb.Write(vertices.WrittenMemory)
        vertices.Clear()
    interface IBufferWriter<'v> with
        member c.GetSpan(count) = c.GetSpan(count)
        member c.GetMemory(count) = c.GetMemory(count)
        member c.Advance(count) = c.Advance(count)        
    member c.SetVertexBuffer(commands : CommandList) =
        commands.SetVertexBuffer(0u, vb.Buffer)
    member c.Dispose() =
        vb.Dispose()
    interface IVertexBuffer with
        member c.VertexCount = vertexCount
        member c.SetVertexBuffer(commands) = c.SetVertexBuffer(commands)
        member c.Flush() = c.Flush()
    interface IDisposable with
        member c.Dispose() = c.Dispose()        
