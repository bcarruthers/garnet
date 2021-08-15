namespace Garnet.Graphics

open System
open System.Buffers
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open Veldrid
open Garnet.Numerics
open Garnet.Resources

/// JSON-serializable
[<Struct>]
type FontCharDescriptor = {
    Code : char
    Width : int
    OffsetX : int
    OffsetY : int
    RectX : int
    RectY : int
    RectWidth : int
    RectHeight : int
    }

/// JSON-serializable
type FontDescriptor = {
    Size : int
    Family : string
    Style : string
    Height : int
    Chars : FontCharDescriptor[]
    }

type Align =
    | Left = 0
    | Center = 1
    | Right = 2

type Valign =
    | Top = 0
    | Center = 1
    | Bottom = 2

type TextWrapping =
    | NoWrap = 0
    | WordWrap = 1

[<Struct>]
type TextBlock = {
    Text : string
    Color : RgbaFloat
    Bounds : Range2i
    Scale : float32
    Align : Align
    Valign : Valign
    Wrapping : TextWrapping
    Spacing : Vector2i
    PixelToViewport : Vector2
    }

[<Struct>]
type FontCharInfo = {
    Width : int
    Offset : Vector2i
    Size : Vector2i
    Rect : Range2
    }

module private FontRun =
    let getCharWidth ch (charWidths : int[]) =
        let index = int ch
        if index < charWidths.Length then charWidths.[index] else 0

    let getTrimmedRange (str : string) (run : Rangei) =
        let mutable start = run.Min
        while start < run.Max && Char.IsWhiteSpace str.[start] do
            start <- start + 1
        let mutable stop = run.Max
        while stop > run.Min && Char.IsWhiteSpace str.[stop - 1] do
            stop <- stop - 1
        Rangei(start, stop)

    let getWordStart (str : string) start =
        let mutable i = start
        let mutable word = false
        while not word && i < str.Length do            
            word <- not (Char.IsWhiteSpace str.[i])            
            i <- i + 1
        i

    let getWordEnd (str : string) start =
        let mutable i = start
        let mutable whitespace = false
        while not whitespace && i < str.Length do            
            whitespace <- Char.IsWhiteSpace str.[i]            
            i <- i + 1
        i

    let getRunWidth (str : string) (charWidths : int[]) (run : Rangei) =
        let mutable width = 0
        for i = run.Min to run.Max - 1 do
            width <- width + getCharWidth str.[i] charWidths
        width
        
    let tryGetNextRun (str : string) charWidths start maxAllowedWidth =
        let mutable runWidth = 0
        let mutable runStart = start
        let mutable i = start
        let mutable result = ValueNone
        while result.IsNone && i < str.Length do
            let ch = str.[i]
            if ch = '\n' then
                // Newline, return run
                result <- ValueSome (Rangei(runStart, i))
                i <- i + 1
            elif Char.IsWhiteSpace(ch) then
                // Whitespace, accumulate width but don't check for limit
                let width = getCharWidth ch charWidths
                runWidth <- runWidth + width
                i <- i + 1
            else
                // Word, scan until whitespace
                let start = i
                let stop = getWordEnd str i
                let wordWidth = getRunWidth str charWidths (Rangei(start, stop))
                let newRunWidth = runWidth + wordWidth
                if newRunWidth > maxAllowedWidth then
                    // If the word doesn't fit on line, return line without word
                    result <- ValueSome (Rangei(runStart, start))
                    runWidth <- wordWidth
                    // Scan to the start of next word to eat any whitespace that
                    // would otherwise appear at the start of next line
                    i <- getWordStart str stop
                else
                    runWidth <- newRunWidth
                    i <- stop
        let remaining = str.Length - runStart
        if remaining > 0 then
            result <- ValueSome (Rangei(runStart, str.Length))
        result

    let measure (str : string) (charWidths : int[]) charHeight maxAllowedWidth =
        let mutable maxWidth = 0
        let mutable count = 0
        let mutable runOpt = tryGetNextRun str charWidths 0 maxAllowedWidth
        while runOpt.IsSome do
            let run = runOpt.Value
            let width = getRunWidth str charWidths run
            maxWidth <- max maxWidth width
            count <- count + 1
            runOpt <- tryGetNextRun str charWidths maxAllowedWidth run.Max
        Vector2i(maxWidth, count * charHeight)    

    let getBounds (size : Vector2i) (bounds : Range2) align valign (pixelToViewport : Vector2) =
        let p0 = Vector2i.FromVector2(bounds.Min / pixelToViewport)
        let p1 = Vector2i.FromVector2(bounds.Max / pixelToViewport)
        let boxSize = p1 - p0
        let x0 =
            match align with
            | Align.Left -> p0.X
            | Align.Right -> p1.X - size.X
            | Align.Center -> p0.X + (boxSize.X - size.X) / 2
            | x -> failwith $"Invalid align {x}"
        let y0 =
            match valign with
            | Valign.Top -> p0.Y
            | Valign.Bottom -> p1.Y - size.Y
            | Valign.Center -> p0.Y + (boxSize.Y - size.X) / 2
            | x -> failwith $"Invalid valign {x}"
        Range2i.Sized(Vector2i(x0, y0), size)

    let getMaxAllowedWidth wrapping (size : float32) (pixelToViewport : float32) =
        match wrapping with
        | TextWrapping.NoWrap -> Int32.MaxValue
        | TextWrapping.WordWrap -> int (size / pixelToViewport)
        | x -> failwith $"Invalid wrapping {x}"

module private FontCharInfoModule =
    let getTexBounds (charRect : Range2i) (mapSize : Vector2i) texBounds =
        let scale = Vector2.One / (mapSize.ToVector2())
        let t0 = charRect.Min.ToVector2() * scale
        let t1 = charRect.Max.ToVector2() * scale       
        let p0 = Range2.Lerp(texBounds, t0) 
        let p1 = Range2.Lerp(texBounds, t1)
        Range2(p0, p1)
        
    let fromCharDescriptor (mapSize : Vector2i) (p : FontCharDescriptor) (texBounds : Range2) = 
        let r = Range2i.Sized(Vector2i(p.RectX, p.RectY), Vector2i(p.RectWidth, p.RectHeight))
        {
            Width = p.Width
            Offset = Vector2i(p.OffsetX, p.OffsetY)
            Size = Vector2i(p.RectWidth, p.RectHeight)
            Rect = getTexBounds r mapSize texBounds
        }

type Font(height, charLookup : FontCharInfo[]) =
    let widths = charLookup |> Array.map (fun c -> c.Width)
    member c.Height = height
    member c.GetCharInfo(ch : char) =
        let code = int ch
        if code < charLookup.Length then charLookup.[code] else charLookup.[0]
    member c.TryGetNextRun(str, start, maxAllowedWidth) =
        FontRun.tryGetNextRun str widths start maxAllowedWidth
    member c.Measure(text) =
        FontRun.measure text widths height Int32.MaxValue
    member c.Measure(block) =
        let bounds = block.Bounds.ToRange2()
        let maxAllowedWidth = FontRun.getMaxAllowedWidth block.Wrapping bounds.Size.X block.PixelToViewport.X
        let size = FontRun.measure block.Text widths height maxAllowedWidth
        FontRun.getBounds size bounds block.Align block.Valign block.PixelToViewport
    static member CreateMonospaced(charSheetSize : Vector2i, texBounds, xSpacing) =
        // Assume this is a 16x16 char tile sheet
        let charSize = charSheetSize / 16
        let lookup = [|
            for y = 0 to 15 do
                for x = 0 to 15 do
                    let p = Vector2i(x, y) * charSize
                    let charRect = Range2i.Sized(p, charSize)
                    {
                        Width = charSize.X + xSpacing
                        Offset = Vector2i.Zero 
                        Size = charSize
                        Rect = FontCharInfoModule.getTexBounds charRect charSheetSize texBounds
                    }    
            |]
        Font(charSize.Y, lookup)
    static member FromDescriptor(desc, mapSize, texBounds) =
        let table = Array.zeroCreate 256
        for ch in desc.Chars do
            let code = int ch.Code
            if code < table.Length then
                let coords = FontCharInfoModule.fromCharDescriptor mapSize ch texBounds
                table.[code] <- coords
        Font(desc.Height, table)
        
[<AutoOpen>]
module FontLoadingExtensions =
    type IStreamSource with
        /// Loads a JSON font paired with a PNG with matching name
        member c.LoadJsonFont(fontName : string, texCache : TextureCache) =
            let desc = c.LoadJson<FontDescriptor>(fontName)
            let textureName = Path.GetFileNameWithoutExtension(fontName) + ".png"
            let size = texCache.[textureName].Size
            Font.FromDescriptor(desc, size, Range2.ZeroToOne)
        
type FontCache() =
    let fonts = Dictionary<string, Font>()
    member c.Item with get name = fonts.[name]
    member c.Add(name, font) =
        fonts.Add(name, font)

type FontCache with
    member c.Load(name, texCache : TextureCache, fs : IStreamSource) =
        let font = fs.LoadJsonFont(name, texCache)
        c.Add(name, font)

    /// Loads a monospace font stored in a texture atlas
    member c.Load(name, atlas : TextureAtlas, xSpacing) =
        let tex = atlas.[name]
        let font = Font.CreateMonospaced(tex.Bounds.Size, tex.NormalizedBounds, xSpacing)
        c.Add(name, font)
        
[<Extension>]
type FontVertexSpanExtensions =
    /// Draws monospace text
    [<Extension>]
    static member DrawText(w : IBufferWriter<PositionTextureDualColorVertex>,
            text : string,
            pos : Vector2i,
            atlasSize : Vector2i,
            fontTexBounds : Range2i,
            charMargin : Vector2i,
            color : RgbaFloat) =
        let charSetSize = fontTexBounds.Size
        let charSize = charSetSize / Vector2i(16, 16)
        let displayCharSize = charSize + charMargin
        let atlasScale = Vector2.One / atlasSize.ToVector2()
        let span = w.GetSpriteSpan(text.Length)
        for i = 0 to text.Length - 1 do
            let ch = text.[i]
            let tileId = int ch
            let tx = tileId &&& 0xf
            let ty = tileId >>> 4
            let t0 = Vector2i(tx, ty) * charSize + fontTexBounds.Min
            let t1 = t0 + charSize
            let tb = Range2(t0.ToVector2() * atlasScale, t1.ToVector2() * atlasScale)
            let b = Range2i.Sized(pos + Vector2i(i * displayCharSize.X, 0), charSize)
            let span = span.Slice(i * 4)
            span.DrawRect(b.ToRange2(), tb, color, RgbaFloat.Clear)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawText(w : IBufferWriter<PositionTextureDualColorVertex>, font : Font, block : TextBlock) =
        let textBounds = font.Measure(block)
        let span = w.GetSpriteSpan(block.Text.Length)
        let mutable runOpt = font.TryGetNextRun(block.Text, 0, block.Bounds.Size.X)
        let mutable row = 0
        let mutable vi = 0
        while runOpt.IsSome do
            let run = runOpt.Value
            let y = textBounds.Y.Min + row * font.Height
            let mutable x = textBounds.X.Min
            for i = run.Min to run.Max - 1 do
                let ch = block.Text.[i]
                let desc = font.GetCharInfo(ch)
                if desc.Size.X > 0 then
                    let b =
                        let tp0 = Vector2i(x + desc.Offset.X, y + desc.Offset.Y)
                        let tp1 = tp0 + desc.Size
                        let p0 = block.PixelToViewport * (tp0.ToVector2())
                        let p1 = block.PixelToViewport * (tp1.ToVector2())
                        Range2(p0, p1)
                    let span = span.Slice(vi)
                    span.DrawRect(b, desc.Rect, block.Color, RgbaFloat.Clear)
                    vi <- vi + 4
                x <- x + desc.Width
            runOpt <- font.TryGetNextRun(block.Text, run.Max, block.Bounds.Size.X)
            row <- row + 1
        w.Advance(vi)

    [<Extension>]
    static member DrawText(w : IBufferWriter<PositionTextureDualColorVertex>, font,
            text,
            pos : Vector2i,
            color : RgbaFloat,
            [<Optional; DefaultParameterValue(0)>] width : int,
            [<Optional; DefaultParameterValue(0)>] height : int,
            [<Optional; DefaultParameterValue(Align.Left)>] align : Align,
            [<Optional; DefaultParameterValue(Valign.Top)>] valign : Valign,
            [<Optional; DefaultParameterValue(TextWrapping.NoWrap)>] wrapping : TextWrapping,
            [<Optional; DefaultParameterValue(1.0f)>] scale : float32,
            [<Optional; DefaultParameterValue(0)>] xSpacing : int,
            [<Optional; DefaultParameterValue(0)>] ySpacing : int
            ) =
        w.DrawText(font, {
            Text = text
            Color = color
            Bounds = Range2i(pos, Vector2i(width, height))
            Scale = scale
            Align = align
            Valign = valign
            Wrapping = wrapping
            Spacing = Vector2i(xSpacing, ySpacing)
            PixelToViewport = Vector2.One
        })
    