module Garnet.Tests.Program

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly { defaultConfig with verbosity = Logging.LogLevel.Verbose } argv
