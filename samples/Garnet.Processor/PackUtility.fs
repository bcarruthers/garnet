namespace Garnet.Processor

open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text.RegularExpressions
open Argu

module PackUtility =
    let getHash file =
        if File.Exists(file) then
            use fs = File.OpenRead(file)
            use sha = SHA1.Create()
            sha.ComputeHash(fs)
            |> Array.map (fun b -> b.ToString("x2"))
            |> String.concat ""
        else ""
        
    let getGlobRegex (globs : string list) =
        if globs.Length = 0 then Regex("x^")
        else
            let pattern =
                globs
                |> List.map (fun str -> "^" + Regex.Escape(str).Replace(@"\*", ".*").Replace(@"\?", ".") + "$")
                |> String.concat "|"
            Regex(pattern, RegexOptions.IgnoreCase ||| RegexOptions.Singleline)

    let run (args : ParseResults<PackArgs>) =
        let inputPath = Path.GetFullPath(args.GetResult <@ PackArgs.Input @>)
        let outputPath = Path.GetFullPath(args.GetResult <@ PackArgs.Output @>)
        let recurse = args.Contains <@ Recurse @>
        let ignoreRegex = getGlobRegex (args.GetResult(<@ Ignore @>, defaultValue = List.empty))
        let level =
            let compression = args.GetResult (<@ Compression @>, defaultValue = 0)
            if compression = 0 then CompressionLevel.NoCompression else CompressionLevel.Optimal
        printfn $"Input path: {inputPath}"
        // Optionally insert version into output path
        let outputPath =
            let key = "{version}"
            if outputPath.Contains(key) then
                let exeFile = Directory.EnumerateFiles(inputPath, "*.exe") |> Seq.head
                let info = FileVersionInfo.GetVersionInfo(exeFile)
                outputPath.Replace(key, info.ProductVersion)
            else outputPath
        printfn $"Output path: {outputPath}"
        // Create folder
        let outputDir = Path.GetDirectoryName(outputPath)
        Directory.CreateDirectory(outputDir) |> ignore
        // Write to temp path
        let tempPath = outputPath + ".temp"
        let _ =
            use zip = ZipFile.Open(tempPath, ZipArchiveMode.Create)
            let options = EnumerationOptions()
            options.RecurseSubdirectories <- recurse
            for file in Directory.EnumerateFiles(inputPath, "*.*", options) do
                let fullPath = Path.GetFullPath(file)
                let name = Path.GetRelativePath(inputPath, fullPath)
                if ignoreRegex.IsMatch(name) then
                    printfn $"Ignoring {name}"
                else
                    printfn $"Adding {name}"
                    zip.CreateEntryFromFile(fullPath, name, level) |> ignore
        // Calc hash of old and new files
        let oldHash = getHash outputPath
        let newHash = getHash tempPath
        printfn $"Old hash: {oldHash}"
        printfn $"New hash: {newHash}"
        // Overwrite only if hash differs
        if newHash = oldHash then
            printfn $"Hashes match, skipping write"
            File.Delete(tempPath)
        else
            File.Move(tempPath, outputPath, overwrite = true)
            let info = FileInfo(outputPath)
            printfn $"{info.Length} bytes written to {outputPath}"
    