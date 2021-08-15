namespace Garnet.Resources

open Newtonsoft.Json

[<AutoOpen>]
module LoaderExtensions =
    type IStreamSource with                
        member c.LoadJson<'a> key =
            let json = c.LoadText(key)
            JsonConvert.DeserializeObject<'a>(json)
            