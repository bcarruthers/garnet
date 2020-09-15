namespace Garnet.Samples.Veldrid

open System
open Veldrid

type ResizableDeviceBuffer(device : GraphicsDevice, elementSize, usage) =
    let mutable buffer = device.ResourceFactory.CreateBuffer(BufferDescription(uint32 (elementSize * 8), usage))
    //let mutable invalid = true
    member c.Buffer = buffer
    //member c.Invalidate() =
    //    invalid <- true
    interface IDeviceBuffer with
        member c.Write<'v
                when 'v : struct 
                and 'v : (new : unit -> 'v) 
                and 'v :> ValueType>(src : 'v[], count) =
            //if invalid then
                // ensure device buffer is large enough
                let size = count * elementSize
                if buffer.SizeInBytes < uint32 size then
                    // destroy old buffer
                    buffer.Dispose()
                    // round up to pow2 number of elements (not bytes)
                    let requiredSize = Buffer.getRequiredCount count * elementSize
                    let desc = new BufferDescription(uint32 requiredSize, usage)    
                    //printfn "Buffer %A %A %A %A" typeof<'v>.Name count elementSize requiredSize
                    buffer <- device.ResourceFactory.CreateBuffer(desc)
                // write data (better if this could be partial)
                device.UpdateBuffer(buffer, 0u, src)
                //invalid <- false
    member c.Dispose() =
        buffer.Dispose()
    interface IDisposable with 
        member c.Dispose() = c.Dispose()

type DeviceMesh(device, vertexSize, vertices : IStagingBuffer, indices : IStagingBuffer) =
    let vb = new ResizableDeviceBuffer(device, vertexSize, BufferUsage.Dynamic ||| BufferUsage.VertexBuffer)
    let ib = new ResizableDeviceBuffer(device, sizeof<uint32>, BufferUsage.Dynamic ||| BufferUsage.IndexBuffer)
    member c.Invalidate() =
        ()
        //vb.Invalidate()
        //ib.Invalidate()
    member c.Draw(cmds : CommandList) =
        // write mesh changes
        let vertexCount = vertices.WriteTo(vb)
        let indexCount = indices.WriteTo(ib)
        //printfn "Draw %A %A" vertexCount indexCount
        // draw mesh
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
