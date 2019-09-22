namespace Garnet.Samples.Veldrid

open System.Numerics
open Veldrid

module ToVeldrid =
    let getRgbaFloat (v : Vector4) =
        RgbaFloat(v.X, v.Y, v.Z, v.W)

    let getVertexLayout (elems : seq<VertexElement>) =
        elems 
        |> Seq.mapi (fun i e -> 
            VertexElementDescription(sprintf "Element%d" i, 
                VertexElementSemantic.TextureCoordinate, 
                LanguagePrimitives.EnumOfValue (byte e)))
        |> Seq.toArray
        |> VertexLayoutDescription
