namespace Garnet.Samples.Roguelike

open System.Buffers
open System.Runtime.CompilerServices
open Veldrid
open Garnet.Samples.Engine
open Garnet.Samples.Roguelike.Types

[<Extension>]
type ViewExtensions =
    [<Extension>]
    static member DrawWorld(w : IBufferWriter<PositionTextureDualColorVertex>, world : World) = 
        let span = w.GetTileSpan(world.tiles.Count)
        let min = World.getMinLocation world
        let mutable i = 0
        for kvp in world.tiles do
            let p = Vector.subtract kvp.Key min
            let tile = kvp.Value
            let ch = Tile.getChar tile
            let fg = RgbaFloat.White
            let bg = RgbaFloat.Black
            span.Slice(i * 4).DrawTile(p.x, p.y, ch, fg, bg)
            i <- i + 1
        w.Advance(span.Length)
