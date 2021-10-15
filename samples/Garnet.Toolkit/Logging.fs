namespace Garnet.Composition

open System
open System.IO
open Microsoft.Extensions.Logging
open Cysharp.Text
open ZLogger
    
type LogSettings = {
    WriteToFile : bool
    WriteToConsole : bool
    RollFile : bool
    LogPath : string
    LogName : string
    TimestampFormat : string
    PrefixFormat : string
    MinLogLevel : LogLevel
    FileRollSizeKb : int
    }

module private Logging =
    let configure settings (options : ZLoggerOptions) =
        let prefixFormat = ZString.PrepareUtf8<string, LogLevel, string>(settings.PrefixFormat)
        options.PrefixFormatter <- fun writer info -> 
            let mutable w = writer
            let timestamp = info.Timestamp.DateTime.ToLocalTime().ToString(settings.TimestampFormat)
            prefixFormat.FormatTo(&w, timestamp, info.LogLevel, info.CategoryName)

type LogSettings with
    static member Default = {
        WriteToFile = true
        WriteToConsole = false
        RollFile = false
        LogPath = "."
        LogName = "run"
        TimestampFormat = "yyyy-MM-dd HH:mm:ss.fff"
        PrefixFormat = "{0} {1} [{2}] "
        MinLogLevel = LogLevel.Information
        FileRollSizeKb = 1024
        }
    
    member settings.CreateFactory() =
        LoggerFactory.Create(fun builder ->
            let builder = builder.SetMinimumLevel(settings.MinLogLevel)
            // Add console
            let builder =
                if settings.WriteToConsole
                then builder.AddZLoggerConsole(Logging.configure settings)
                else builder
            // Add rolling file
            let _ =
                if settings.WriteToFile then
                    if settings.RollFile then
                        let logPrefix = Path.Combine(settings.LogPath, settings.LogName + "-")
                        builder.AddZLoggerRollingFile(
                            (fun dt index ->
                                let timestamp = dt.ToLocalTime().ToString("yyyy-MM-dd")
                                let num = index.ToString().PadLeft(3, '0')
                                $"{logPrefix}{timestamp}_{num}.log"),
                            (fun dt -> DateTimeOffset(dt.ToLocalTime().Date)),
                            settings.FileRollSizeKb,
                            Action<ZLoggerOptions>(Logging.configure settings))
                    else
                        let file = Path.Combine(settings.LogPath, settings.LogName + ".log")
                        builder.AddZLoggerFile(file, Action<_>(Logging.configure settings))
                else builder
            ())

[<AutoOpen>]
module LoggingExtensions =
    type Container with
        static member RunLoop(settings : LogSettings, register) =
            use factory = settings.CreateFactory()
            let logger = factory.CreateLogger("Default")
            try
                let c = Container()
                c.Set<ILogger>(logger)
                use s = register c
                c.RunLoop()
            with ex ->
                logger.LogError(ex, "")

        static member RunLoop(register) =
            Container.RunLoop(LogSettings.Default, register)
