﻿namespace Garnet.Samples.Engine

open Newtonsoft.Json

[<AutoOpen>]
module LoaderExtensions =
    type IStreamSource with                
        member c.LoadJson<'a> key =
            let json = c.LoadText(key)
            JsonConvert.DeserializeObject<'a>(json)
            