open Argu
open Garnet.Processor

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<ProcessorArgs>()
    try
        let args = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        match args.GetSubCommand() with
        | Pack args -> PackUtility.run args
    with e -> printfn $"%s{e.Message}"
    0