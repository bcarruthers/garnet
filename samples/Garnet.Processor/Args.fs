namespace Garnet.Processor

open Argu

[<CliPrefix(CliPrefix.Dash)>]
type PackArgs =
    | Input of string
    | Output of string
    | Compression of int
    | Ignore of string list
    | Recurse
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Input _ -> "Input directory to pack"
            | Output _ -> "Output packed file"
            | Compression _ -> "Compression level, 0 for none"
            | Recurse -> "Include subdirectories recursively"
            | Ignore _ -> "Ignored file pattern, e.g. *.dat"

and ProcessorArgs =
    | [<CliPrefix(CliPrefix.None)>] Pack of ParseResults<PackArgs>
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Pack _ -> "Pack files into a single archive file."
