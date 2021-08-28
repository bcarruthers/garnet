namespace Garnet.Graphics

open System
open System.Buffers
open System.Runtime.CompilerServices
open System.Numerics
open System.Runtime.InteropServices
open Veldrid
open Garnet.Numerics
open Garnet.Composition

/// JSON-serializable
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
    Scale : int
    Align : Align
    Valign : Valign
    Wrapping : TextWrapping
    Spacing : Vector2i
    } with
    static member Default = {
        Text = ""
        Color = RgbaFloat.White
        Bounds = Range2i.Zero
        Scale = 1
        Align = Align.Left
        Valign = Valign.Top
        Wrapping = TextWrapping.NoWrap
        Spacing = Vector2i.Zero
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
        
    let isWhitespace ch =
        Char.IsWhiteSpace ch ||
        ch = '\n'

    let getWordStart (str : string) start =
        let mutable i = start
        while i < str.Length && isWhitespace str.[i] do
            i <- i + 1
        i

    let getWordEnd (str : string) start =
        let mutable i = start
        while i < str.Length && not (isWhitespace str.[i]) do
            i <- i + 1
        i

    let getRunWidth (str : string) (charWidths : int[]) (run : Rangei) =
        let mutable width = 0
        for i = run.Min to run.Max - 1 do
            width <- width + getCharWidth str.[i] charWidths
        width
        
    let tryGetNextRun (str : string) charWidths start maxAllowedWidth =
        let mutable runWidth = 0
        let mutable i = start
        let mutable result = ValueNone
        while result.IsNone && i < str.Length do
            let ch = str.[i]
            if isWhitespace ch then
                // Whitespace, accumulate width but don't check for limit
                let width = getCharWidth ch charWidths
                runWidth <- runWidth + width
                i <- i + 1
            else
                // Word, scan until whitespace
                let stop = getWordEnd str i
                if stop < str.Length && str.[stop] = '\n' then
                    // Newline, return a run including newline
                    result <- ValueSome (Rangei(start, stop + 1))
                else
                    let wordWidth = getRunWidth str charWidths (Rangei(start, stop))
                    let newRunWidth = runWidth + wordWidth
                    if newRunWidth > maxAllowedWidth then
                        // Scan to the start of next word to eat any whitespace that
                        // would otherwise appear at the start of next line
                        let nextWord = getWordStart str stop
                        // If the word doesn't fit on line, return line without word
                        result <- ValueSome (Rangei(start, nextWord))
                    else
                        runWidth <- newRunWidth
                        i <- stop
        if result.IsNone then
            let remaining = str.Length - start
            if remaining > 0 then
                result <- ValueSome (Rangei(start, str.Length))
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
            runOpt <- tryGetNextRun str charWidths run.Max maxAllowedWidth
        Vector2i(maxWidth, count * charHeight)    

    let getBounds (size : Vector2i) (bounds : Range2i) align valign =
        let p0 = bounds.Min
        let p1 = bounds.Max
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
            | Valign.Center -> p0.Y + (boxSize.Y - size.Y) / 2
            | x -> failwith $"Invalid valign {x}"
        Range2i.Sized(Vector2i(x0, y0), size)

    let getMaxAllowedWidth wrapping size =
        match wrapping with
        | TextWrapping.NoWrap -> Int32.MaxValue
        | TextWrapping.WordWrap -> size
        | x -> failwith $"Invalid wrapping {x}"

module private FontCharInfo =
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
        let bounds = block.Bounds
        let maxAllowedWidth = FontRun.getMaxAllowedWidth block.Wrapping (bounds.Size.X * block.Scale)
        let size = FontRun.measure block.Text widths height maxAllowedWidth * block.Scale
        FontRun.getBounds size bounds block.Align block.Valign
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
                        Rect = FontCharInfo.getTexBounds charRect charSheetSize texBounds
                    }    
            |]
        Font(charSize.Y, lookup)
    static member FromDescriptor(desc, mapSize, texBounds) =
        let table = Array.zeroCreate 256
        for ch in desc.Chars do
            let code = int ch.Code
            if code < table.Length then
                let coords = FontCharInfo.fromCharDescriptor mapSize ch texBounds
                table.[code] <- coords
        Font(desc.Height, table)
        
[<AutoOpen>]
module FontLoadingExtensions =
    type IReadOnlyFolder with
        member c.LoadJsonFontDescriptor(fontName : string) =
            c.LoadJson<FontDescriptor>(fontName)

    type IResourceCache with
        /// Loads a monospace font stored in a texture atlas
        member c.LoadMonospacedFont(atlasName, fontName, xSpacing) =
            match c.TryGetResource<Font>(fontName) with
            | true, font -> font
            | false, _ ->
                let atlas = c.LoadResource<TextureAtlas>(atlasName)
                let tex = atlas.[fontName]
                let font = Font.CreateMonospaced(tex.Bounds.Size, tex.NormalizedBounds, xSpacing)
                c.AddResource(fontName, font)
                font

        /// Loads a JSON font paired with a PNG with matching name
        member c.LoadJsonFont(fontName,
                fontTextureName,
                [<Optional; DefaultParameterValue("")>] atlasName : string) =
            match c.TryGetResource<Font>(fontName) with
            | true, font -> font
            | false, _ ->
                let desc = c.LoadResource<FontDescriptor>(fontName)
                let font =
                    if String.IsNullOrEmpty(atlasName) then
                        let tex = c.LoadResource<Texture>(fontTextureName)
                        Font.FromDescriptor(desc, Vector2i(int tex.Width, int tex.Height), Range2.ZeroToOne)
                    else
                        let atlas = c.LoadResource<TextureAtlas>(atlasName)
                        let tex = atlas.[fontTextureName]
                        let size = tex.Bounds.Size //- Vector2i.One * tex.Padding * 2
                        let texBounds = tex.NormalizedBounds
                        Font.FromDescriptor(desc, Vector2i(abs size.X, abs size.Y), texBounds)
                c.AddResource(fontName, font)
                font
        
type FontLoader() =
    interface IResourceLoader with
        member c.Load(folder, cache, key) =
            let font = folder.LoadJsonFontDescriptor(key)
            cache.AddResource<FontDescriptor>(key, font)

[<AutoOpen>]
module FontLoaderExtensions =
    type ResourceCache with
        member c.AddFontLoaders() =
            c.AddLoader(".font.json", FontLoader())
        
[<Extension>]
type FontVertexSpanExtensions =
    /// Draws monospace text
    [<Extension>]
    static member DrawText(w : IBufferWriter<PositionTextureColorVertex>,
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
        let span = w.GetQuadSpan(text.Length)
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
            span.DrawQuad(b.ToRange2(), tb, color)
        w.Advance(span.Length)

    [<Extension>]
    static member DrawText(w : IBufferWriter<PositionTextureColorVertex>, font : Font, block : TextBlock) =
        let textBounds = font.Measure(block)
        let span = w.GetQuadSpan(block.Text.Length)
        let mutable runOpt = font.TryGetNextRun(block.Text, 0, block.Bounds.Size.X)
        let mutable row = 0
        let mutable vi = 0
        while runOpt.IsSome do
            let run = runOpt.Value
            let y = textBounds.Y.Min + row * font.Height * block.Scale
            let mutable x = textBounds.X.Min
            for i = run.Min to run.Max - 1 do
                let ch = block.Text.[i]
                let desc = font.GetCharInfo(ch)
                if desc.Size.X > 0 then
                    let b =
                        let offset = desc.Offset * block.Scale
                        let p0 = Vector2i(x + offset.X, y + offset.Y)
                        let p1 = p0 + desc.Size * block.Scale
                        Range2i(p0, p1)
                    let span = span.Slice(vi)
                    span.DrawQuad(b.ToRange2(), desc.Rect, block.Color)
                    vi <- vi + 4
                x <- x + desc.Width * block.Scale
            runOpt <- font.TryGetNextRun(block.Text, run.Max, block.Bounds.Size.X)
            row <- row + 1
        w.Advance(vi)

    [<Extension>]
    static member DrawText(w : IBufferWriter<PositionTextureColorVertex>, font,
            text,
            pos : Vector2i,
            color : RgbaFloat,
            [<Optional; DefaultParameterValue(0)>] width : int,
            [<Optional; DefaultParameterValue(0)>] height : int,
            [<Optional; DefaultParameterValue(Align.Left)>] align : Align,
            [<Optional; DefaultParameterValue(Valign.Top)>] valign : Valign,
            [<Optional; DefaultParameterValue(TextWrapping.NoWrap)>] wrapping : TextWrapping,
            [<Optional; DefaultParameterValue(1)>] scale : int,
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
        })
    