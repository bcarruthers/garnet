namespace Garnet.Samples.Roguelike

open System.Buffers
open System.Runtime.CompilerServices
open Veldrid
open Garnet.Graphics

[<Struct>]
type DisplayTile = {
    ch : char
    fg : RgbaFloat
    bg : RgbaFloat
    }

module DisplayTile =
    let fromTile tile =
        match tile.entity with
        | Some entity ->
            match entity.entityType with
            | Rogue -> {
                ch = '@'
                fg = RgbaFloat(0.3f, 1.0f, 1.0f, 1.0f)
                bg = RgbaFloat(0.3f, 1.0f, 1.0f, 0.3f)
                }
            | Minion -> {
                ch = 'm'
                fg = RgbaFloat(1.0f, 0.3f, 0.3f, 1.0f)
                bg = RgbaFloat(1.0f, 0.3f, 0.3f, 0.3f)
                }
        | None ->
            match tile.terrain with
            | Floor -> {
                ch = '.'
                fg = RgbaFloat(0.3f, 0.4f, 0.5f, 0.5f)
                bg = RgbaFloat(0.3f, 0.4f, 0.5f, 0.1f)
                }
            | Wall -> {
                ch = '#'
                fg = RgbaFloat(0.3f, 0.4f, 0.5f, 1.0f)
                bg = RgbaFloat(0.3f, 0.4f, 0.5f, 0.3f)
                }
            
[<Extension>]
type ViewExtensions =
    [<Extension>]
    static member DrawWorld(w : IBufferWriter<PositionTextureDualColorVertex>, world : World) = 
        let span = w.GetTileSpan(world.tiles.Count)
        let min = World.getMinLocation world
        let mutable i = 0
        for kvp in world.tiles do
            let p = Vector.subtract kvp.Key min
            let tile = DisplayTile.fromTile kvp.Value
            span.Slice(i * 4).DrawTile(p.x, p.y, tile.ch, tile.fg, tile.bg)
            i <- i + 1
        w.Advance(span.Length)
