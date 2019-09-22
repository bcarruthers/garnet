namespace Garnet.Samples.Veldrid

open System
open System.Collections.Generic
open Newtonsoft.Json

type TextureEntry = {
    name : string
    x : int
    y : int
    width : int
    height : int
    padding : int
    }

type TextureAtlas = {
    width : int
    height : int
    undefinedName : string
    textures : TextureEntry list
    }

[<Struct>]
type TextureBounds = {
    tx0 : float32
    ty0 : float32
    tx1 : float32
    ty1 : float32
    }

module TextureBounds =
    let zero = {
        tx0 = 0.0f
        ty0 = 0.0f
        tx1 = 0.0f
        ty1 = 0.0f
        }

    let zeroToOne = {
        tx0 = 0.0f
        ty0 = 0.0f
        tx1 = 1.0f
        ty1 = 1.0f
        }

module internal Texturing =
    [<Struct>]
    type Interval = {
        min : int
        max : int
    }

    [<Struct>]
    type Vec2 = {
        x : int
        y : int
    } with
        static member (+) (a: Vec2, b: Vec2) = { x = a.x + b.x; y = a.y + b.y }
        static member (-) (a: Vec2, b: Vec2) = { x = a.x - b.x; y = a.y - b.y }
        static member (*) (a: Vec2, c) = { x = a.x * c; y = a.y * c }

    [<Struct>]
    type Rect = {
        ix : Interval
        iy : Interval
    }

    [<Struct>]
    type Intervalf = {
        min : float32
        max : float32
    }

    [<Struct>]
    type Vec2f = {
        x : float32
        y : float32
    }

    [<Struct>]
    type Rectf = {
        ix : Intervalf
        iy : Intervalf
    }

    type Interval with
        member c.len = c.max - c.min

    module Interval =
        let length c = c.max - c.min
        let init min max = { Interval.min = min; max = max }
        let zero = init 0 0
        let sized min size = init min (min + size)

    module Intervalf =
        let length c = c.max - c.min
        let init min max = { Intervalf.min = min; max = max }
        let zero = init 0.0f 0.0f

    module Vec2 =
        let init x y = { Vec2.x = x; y = y }
        let zero = init 0 0
        let one = init 1 1
        let add (a: Vec2) (b: Vec2) = { Vec2.x = a.x + b.x; y = a.y + b.y }

    module Vec2f =
        let init x y = { Vec2f.x = x; y = y }
        let fromInt (v: Vec2) = init (float32 v.x) (float32 v.y)

    type Rect with
        member c.min = Vec2.init c.ix.min c.iy.min
        member c.max = Vec2.init c.ix.max c.iy.max

    module Rect =
        let init ix iy = { Rect.ix = ix; iy = iy }
        let zero = init Interval.zero Interval.zero

        let bounds (min : Vec2) (max : Vec2) = 
            init (Interval.init min.x max.x) (Interval.init min.y max.y)
            
        let sized (min : Vec2) (size : Vec2) = bounds min (Vec2.add min size)

    module Rectf =
        let init ix iy = { Rectf.ix = ix; iy = iy }
        let zero = init Intervalf.zero Intervalf.zero

        let bounds (min : Vec2f) (max : Vec2f) = 
            init (Intervalf.init min.x max.x) (Intervalf.init min.y max.y)

open Texturing

// http://wiki.unity3d.com/index.php?title=MaxRectsBinPack
module private Packing =
    let private isContainedIn (a : Rect) (b : Rect) =
        a.ix.min >= b.ix.min && 
        a.iy.min >= b.iy.min &&
        a.ix.max <= b.ix.max &&
        a.iy.max <= b.iy.max
    
    let private isOverlapping (a : Rect) (b : Rect) =
        a.ix.min >= b.ix.max ||
        a.ix.max <= b.ix.min ||
        a.iy.min >= b.iy.max ||
        a.iy.max <= b.iy.min

    type MaxRectsBinPack(size : Vec2) =
        let w = max 0 size.x
        let h = max 0 size.y
        let n = Rect.bounds Vec2.zero (Vec2.init w h)
        let usedRects = List<Rect>()
        let freeRects = List<Rect>([ n ])    
 
        member c.Insert(size : Vec2) =
            if size.x <= 0 || size.y <= 0 then None
            else
                c.FindPositionForNewNodeBestAreaFit(size)
                |> Option.map c.PlaceRect

        member private c.PlaceRect(newNode) =
            let mutable n = freeRects.Count
            let mutable i = 0
            while i < n do
                if c.SplitFreeNode(freeRects.[i], newNode) then
                    freeRects.RemoveAt(i)
                    i <- i - 1
                    n <- n - 1
                i <- i + 1
            c.PruneFreeList()
            usedRects.Add(newNode)
            newNode
            
        member private c.FindPositionForNewNodeBestAreaFit(size : Vec2) = //(int width, int height, ref int bestAreaFit, ref int bestShortSideFit) 
            let mutable bestNode = None 
            let mutable bestAreaFit = Int64.MaxValue 
            let mutable bestShortSideFit = Int32.MaxValue 
            for rect in freeRects do
                let areaFit = int64 rect.ix.len * int64 rect.iy.len - int64 size.x * int64 size.y 
                // Try to place the rectangle in upright (non-flipped) orientation.
                if (rect.ix.len >= size.x && rect.iy.len >= size.y) then
                    let leftoverHoriz = abs (rect.ix.len - size.x)
                    let leftoverVert = abs (rect.iy.len - size.y)
                    let shortSideFit = min leftoverHoriz leftoverVert 
                    if areaFit < bestAreaFit || (areaFit = bestAreaFit && shortSideFit < bestShortSideFit) then
                        bestNode <- Some (Rect.sized rect.min size)
                        bestShortSideFit <- shortSideFit
                        bestAreaFit <- areaFit                                      
            bestNode
 
        member private c.SplitFreeNode(free : Rect, used : Rect) =
            // Test with SAT if the rectangles even intersect.
            if isOverlapping used free then false
            else 
                if (used.ix.min < free.ix.max && used.ix.max > free.ix.min) then
                    // New node at the top side of the used node.
                    if (used.iy.min > free.iy.min && used.iy.min < free.iy.max) then
                        freeRects.Add(Rect.init free.ix (Interval.sized free.iy.min (used.iy.min - free.iy.min)))
                    // New node at the bottom side of the used node.
                    if (used.iy.max < free.iy.max) then
                        freeRects.Add(Rect.init free.ix (Interval.sized used.iy.max (free.iy.max - (used.iy.max))))
                if (used.iy.min < free.iy.max && used.iy.max > free.iy.min) then
                    // New node at the left side of the used node.
                    if (used.ix.min > free.ix.min && used.ix.min < free.ix.max) then
                        freeRects.Add(Rect.init (Interval.sized free.ix.min (used.ix.min - free.ix.min)) free.iy)
                    // New node at the right side of the used node.
                    if (used.ix.max < free.ix.max) then
                        freeRects.Add(Rect.init (Interval.sized used.ix.max (free.ix.max - (used.ix.max))) free.iy)
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

    /// Stateful
    let createPacker atlasSize =
        MaxRectsBinPack(atlasSize).Insert    

    let packItems atlasSize itemSizes =
        let pack = createPacker atlasSize
        itemSizes |> Seq.map (fun (item, size) -> item, pack size)

    let pack atlasSize sizes =
        let pack = createPacker atlasSize
        sizes |> Seq.map pack

module TextureAtlas =
    let empty = {
        width = 0
        height = 0
        undefinedName = ""
        textures = List.empty 
        }

    let readFile file = 
        JsonConvert.DeserializeObject<TextureAtlas>(System.IO.File.ReadAllText(file))

    let writeFile file atlas = 
        let str = JsonConvert.SerializeObject(atlas, Formatting.Indented)
        System.IO.File.WriteAllText(file, str)

    let private getTexBounds width height (tex : TextureEntry) =
        let size = Vec2f.init (float32 width) (float32 height)
        let padding = Vec2.init tex.padding tex.padding
        let tc0 = Vec2.init tex.x tex.y
        let tc1 = Vec2.add tc0 (Vec2.init tex.width tex.height)
        let p0 = Vec2.add tc0 padding |> Vec2f.fromInt
        let p1 = Vec2.add tc1 padding |> Vec2f.fromInt
        {
            tx0 = p0.x / size.x
            ty0 = p0.y / size.y
            tx1 = p1.x / size.x
            ty1 = p1.y / size.y
        }
    
    // Converts int coords to float
    let toLookup atlas =
        let lut = Dictionary<_,_>()
        for tex in atlas.textures do
            let nameLower = tex.name.ToLowerInvariant()
            let bounds = getTexBounds atlas.width atlas.height tex
            lut.[nameLower] <- bounds
            lut.[tex.name] <- bounds
        let undefinedBounds = 
            match lut.TryGetValue atlas.undefinedName with
            | true, x -> x
            | false, _ -> TextureBounds.zero
        fun (key : string) -> 
            match lut.TryGetValue key with
            | true, x -> x
            | false, _ -> undefinedBounds            
