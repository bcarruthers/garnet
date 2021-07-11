namespace Garnet.Samples.Engine

open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Text
open Newtonsoft.Json
open Veldrid
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

module internal TextureLoading =
    let getMipSize original mipLevel =
        original >>> mipLevel |> max 1
    
    let getFormatSize format =
        match format with
        | PixelFormat.R8_G8_B8_A8_UNorm -> 4
        | PixelFormat.BC3_UNorm -> 1
        | _ -> failwithf "Unsupported format %A" format

[<AutoOpen>]
module private Packing =
    let isContainedIn (a : Range2i) (b : Range2i) =
        a.X.Min >= b.X.Min && 
        a.Y.Min >= b.Y.Min &&
        a.X.Max <= b.X.Max &&
        a.Y.Max <= b.Y.Max
    
    let isOverlapping (a : Range2i) (b : Range2i) =
        a.X.Min >= b.X.Max ||
        a.X.Max <= b.X.Min ||
        a.Y.Min >= b.Y.Max ||
        a.Y.Max <= b.Y.Min

// http://wiki.unity3d.com/index.php?title=MaxRectsBinPack
type internal MaxRectsBinPack(size : Vector2i) =
    let w = max 0 size.X
    let h = max 0 size.Y
    let n = Range2i(Vector2i.Zero, Vector2i(w, h))
    let usedRects = List<string * Range2i>()
    let freeRects = List<Range2i>([ n ]) 
    member c.Entries = 
        usedRects :> seq<_>
    member c.Insert(key, size : Vector2i) =
        if size.X <= 0 || size.Y <= 0 then None
        else
            match c.TryFindPositionForNewNodeBestAreaFit(size) with
            | Some rect -> c.PlaceRect(key, rect) |> Some
            | None -> None
    member private c.PlaceRect(key, newNode) =
        let mutable n = freeRects.Count
        let mutable i = 0
        while i < n do
            if c.SplitFreeNode(freeRects.[i], newNode) then
                freeRects.RemoveAt(i)
                i <- i - 1
                n <- n - 1
            i <- i + 1
        c.PruneFreeList()
        usedRects.Add(key, newNode)
        newNode
    member private c.TryFindPositionForNewNodeBestAreaFit(size : Vector2i) = //(int width, int height, ref int bestAreaFit, ref int bestShortSideFit) 
        let mutable bestNode = None 
        let mutable bestAreaFit = Int64.MaxValue 
        let mutable bestShortSideFit = Int32.MaxValue 
        for rect in freeRects do
            let areaFit = int64 rect.X.Size * int64 rect.Y.Size - int64 size.X * int64 size.Y 
            // Try to place the rectangle in upright (non-flipped) orientation.
            if (rect.X.Size >= size.X && rect.Y.Size >= size.Y) then
                let leftoverHoriz = abs (rect.X.Size - size.X)
                let leftoverVert = abs (rect.Y.Size - size.Y)
                let shortSideFit = min leftoverHoriz leftoverVert 
                if areaFit < bestAreaFit || (areaFit = bestAreaFit && shortSideFit < bestShortSideFit) then
                    bestNode <- Some (Range2i.Sized(rect.Min, size))
                    bestShortSideFit <- shortSideFit
                    bestAreaFit <- areaFit                                      
        bestNode 
    member private c.SplitFreeNode(free : Range2i, used : Range2i) =
        // Test with SAT if the rectangles even intersect.
        if isOverlapping used free then false
        else 
            if (used.X.Min < free.X.Max && used.X.Max > free.X.Min) then
                // New node at the top side of the used node.
                if (used.Y.Min > free.Y.Min && used.Y.Min < free.Y.Max) then
                    freeRects.Add(Range2i(free.X, (Rangei.Sized(free.Y.Min, used.Y.Min - free.Y.Min))))
                // New node at the bottom side of the used node.
                if (used.Y.Max < free.Y.Max) then
                    freeRects.Add(Range2i(free.X, (Rangei.Sized(used.Y.Max, free.Y.Max - used.Y.Max))))
            if (used.Y.Min < free.Y.Max && used.Y.Max > free.Y.Min) then
                // New node at the left side of the used node.
                if (used.X.Min > free.X.Min && used.X.Min < free.X.Max) then
                    freeRects.Add(Range2i(Rangei.Sized(free.X.Min, used.X.Min - free.X.Min), free.Y))
                // New node at the right side of the used node.
                if (used.X.Max < free.X.Max) then
                    freeRects.Add(Range2i(Rangei.Sized(used.X.Max, free.X.Max - used.X.Max), free.Y))
            true 
    member private c.PruneFreeList() =
        let mutable isDone = false
        let mutable i = 0
        while not isDone && i < freeRects.Count do
            let mutable j = i + 1
            while not isDone && j < freeRects.Count do
                if isContainedIn freeRects.[i] freeRects.[j] then
                    freeRects.RemoveAt(i)
                    i <- i - 1
                    isDone <- true                
                elif isContainedIn freeRects.[j] freeRects.[i] then
                    freeRects.RemoveAt(j)
                    j <- j - 1
                j <- j + 1
            i <- i + 1

type TextureAtlas(texture : Texture, entries : (string * Range2i) seq) =
    let dict = 
        let size = Vector2(float32 texture.Width, float32 texture.Height)
        let dict = Dictionary<string, Range2>()
        for (key, rect) in entries do
            let p0 = rect.Min.ToVector2() / size
            let p1 = rect.Max.ToVector2() / size
            dict.Add(key, Range2(p0, p1))
        dict
    member c.Texture = texture
    member c.GetBounds(key) =
        dict.[key]
    member c.Dispose() =
        texture.Dispose()
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()

[<AutoOpen>]
module TextureExtensions =
    type Texture with
        member texture.Load(device : GraphicsDevice, desc : TextureDescription, data : ReadOnlyMemory<byte>) =
            let factory = device.ResourceFactory
            // create staging texture
            use staging = 
                factory.CreateTexture(
                    TextureDescription(
                        desc.Width, desc.Height, desc.Depth, desc.MipLevels, 
                        desc.ArrayLayers, desc.Format, TextureUsage.Staging, 
                        desc.Type))
            // copy from buffer to staging
            use handle = data.Pin()
            let formatSize = TextureLoading.getFormatSize desc.Format
            let mutable offset = 0
            for level = 0 to int desc.MipLevels - 1 do
                let mipWidth = TextureLoading.getMipSize (int desc.Width) level
                let mipHeight = TextureLoading.getMipSize (int desc.Height) level
                let mipDepth = TextureLoading.getMipSize (int desc.Depth) level
                let subresourceSize = mipWidth * mipHeight * mipDepth * formatSize
                for layer = 0 to int desc.ArrayLayers - 1 do
                    device.UpdateTexture(
                        staging, IntPtr handle.Pointer + nativeint offset, uint32 subresourceSize,
                        0u, 0u, 0u, uint32 mipWidth, uint32 mipHeight, uint32 mipDepth,
                        uint32 level, 
                        uint32 layer)
                    offset <- offset + subresourceSize
            // copy from staging to final
            use cl = factory.CreateCommandList()
            cl.Begin()
            cl.CopyTexture(staging, texture)
            cl.End()
            device.SubmitCommands(cl)
            texture

    type GraphicsDevice with
        member device.CreateTexture(desc : TextureDescription, data) =
            let texture = device.ResourceFactory.CreateTexture(desc)
            texture.Load(device, desc, data)

        member device.CreateTextureRgba(width, height, data) =
            let desc = 
                TextureDescription(
                    Width = uint32 width, 
                    Height = uint32 height, 
                    Depth = 1u, 
                    MipLevels = 1u, 
                    ArrayLayers = 1u, 
                    Format = PixelFormat.R8_G8_B8_A8_UNorm,
                    Usage = TextureUsage.Sampled,
                    Type = TextureType.Texture2D)
            device.CreateTexture(desc, data)

        member device.CreateTexture(image : Image<Rgba32>) =
            let w = image.Width
            let h = image.Height
            let bytes = Array.zeroCreate<byte>(w * h * 4)
            for y = 0 to h - 1 do
                let row = image.GetPixelRowSpan(y)
                let src = MemoryMarshal.Cast<Rgba32, byte>(row)
                let dest = bytes.AsSpan().Slice(w * 4 * y, w * 4)
                src.CopyTo(dest)
            device.CreateTextureRgba(image.Width, image.Height, ReadOnlyMemory(bytes))

        member device.CreateTextureAtlas(atlasWidth, atlasHeight, images : (string * Image<Rgba32>) seq) =
            let bytes = Array.zeroCreate<byte>(atlasWidth * atlasHeight * 4)
            let packer = MaxRectsBinPack(Vector2i(atlasWidth, atlasHeight))
            for (key, image) in images do
                let size = Vector2i(image.Width, image.Height)
                match packer.Insert(key, size) with
                | None -> failwithf "Could not pack texture %s" key
                | Some rect ->
                    let w = image.Width
                    let h = image.Height
                    for y = 0 to h - 1 do
                        let row = image.GetPixelRowSpan(y)
                        let src = MemoryMarshal.Cast<Rgba32, byte>(row)
                        let dx = rect.Min.X
                        let dy = rect.Min.Y + y
                        let dest = bytes.AsSpan().Slice((dy * atlasWidth + dx) * 4, w * 4)
                        src.CopyTo(dest)
            let desc = 
                TextureDescription(
                    Width = uint32 atlasWidth, 
                    Height = uint32 atlasHeight, 
                    Depth = 1u, 
                    MipLevels = 1u, 
                    ArrayLayers = 1u, 
                    Format = PixelFormat.R8_G8_B8_A8_UNorm,
                    Usage = TextureUsage.Sampled,
                    Type = TextureType.Texture2D)
            let texture = device.CreateTexture(desc, ReadOnlyMemory(bytes))
            new TextureAtlas(texture, packer.Entries)


