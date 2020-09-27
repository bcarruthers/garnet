namespace Garnet.Formatting

open System
open System.Collections
open System.Collections.Generic
open System.Text
open Garnet.Comparisons

/// Provides debug formatting for specific types
type IFormatter =
    abstract member Format<'a> : 'a -> string
    abstract member CanFormat<'a> : unit -> bool
        
/// Provides debug formatting for specific types
type Formatter() =
    let skippedTypes = HashSet<Type>()
    let dict = Dictionary<Type, obj>()
    member c.Skip(t : Type) =
        skippedTypes.Add(t) |> ignore
    member c.Skip<'a>() =
        c.Skip(typeof<'a>)
    member c.Register(t : Type, format) =
        dict.[t] <- format
    member c.Register<'a>(action : 'a -> string) =
        dict.[typeof<'a>] <- action
    member c.Format<'a> e =
        match dict.TryGetValue(typeof<'a>) with
        | true, x -> 
            let handle = x :?> ('a -> string)
            handle e
        | false, _ -> sprintf "%A" e
    interface IFormatter with
        member c.Format<'a> e =
            c.Format<'a> e
        member c.CanFormat<'a>() =
            not (skippedTypes.Contains(typeof<'a>))

type IStringBlockWriter =
    abstract Begin : string * string -> bool
    abstract Write : string -> unit
    abstract End : unit -> unit

[<AutoOpen>]
module StringBlockWriter =
    type IStringBlockWriter with
        member c.Begin(name) =
            c.Begin(name, name)

        member c.BeginList(name, count) =
            let str = sprintf "%s (%d)" name count
            if count = 0 
                then c.Write(str); false
                else c.Begin(str, name)

type StringBlockWriter() =
    let sb = StringBuilder()
    let mutable indent = 0
    member private c.AppendLine(str) =
        for i = 0 to indent - 1 do
            sb.Append("  ") |> ignore
        sb.AppendLine(str) |> ignore
    interface IStringBlockWriter with
        member c.Begin(name, id) =
            c.AppendLine(name)
            indent <- indent + 1
            true
        member c.Write(text) =
            c.AppendLine(text)
        member c.End() =
            indent <- indent - 1
    member c.Clear() =
        sb.Clear()
    override c.ToString() =
        sb.ToString()
    static member Format(toString) =
        let w = StringBlockWriter()
        toString w
        w.ToString()

[<AutoOpen>]
module internal Internal =
    let private getNonGenericTypeName (name : string) =
        let last = name.LastIndexOf('`')
        if last < 0 then name else name.Substring(0, last)

    let rec typeToString (t : Type) =
        let name = getNonGenericTypeName t.Name
        let args = t.GetGenericArguments()
        if args.Length = 0 then name
        else sprintf "%s<%s>" name (String.Join(",", args
            |> Seq.map typeToString |> Seq.toArray))
    
    let isEmptyType (t : Type) =
        not (t.IsPrimitive || t.IsEnum || t.GetProperties().Length > 0)

    let formatRecord indent value =        
        let str = sprintf "%A" value
        str.Replace("\n", "\n" + indent)
        
    let listInfoToString (list : IList) =
        let args = list.GetType().GetGenericArguments()
        sprintf "%s (%d)" (args.[0] |> typeToString) list.Count
    
    let addIndent (str : string) =
        str.Replace("\n", "\n  ")

    let formatList name count (str : string) =
        sprintf "%s (%d)%s" name count
            (if count > 0 then ":\n  " + addIndent str else "")

    let listToString prefix title items =
        let str =
            String.Join("", 
                items 
                |> Seq.map (fun x -> "\n" + prefix + x.ToString()) 
                |> Seq.toArray)
        sprintf "%s%s" title str 

    let reverseString (str : string) = 
        let ch = str.ToCharArray()
        Array.Reverse(ch)
        String(ch)

    let maskToString maxBits (mask : uint64) =
        Convert.ToString(int64 mask, 2)
            .PadLeft(maxBits, '0')
            .Replace('0', '.')
            .Replace('1', 'x')
            |> reverseString
                    
    [<Struct>]
    type CachedTypeInfo = {
        isEmpty : bool
        typeName : string
        }

    let isEnumerable<'a> =
        typeof<'a>.GetInterfaces() 
        |> Seq.exists (fun i -> 
            let isEnumerable = 
                obj.ReferenceEquals(i, typeof<System.Collections.IEnumerable>) || 
                obj.ReferenceEquals(i, typeof<System.Collections.IEnumerator>)
            let isGenericEnumerable = 
                i.IsGenericType && 
                (obj.ReferenceEquals(i.GetGenericTypeDefinition(), typeof<IEnumerable<_>>) || 
                    obj.ReferenceEquals(i.GetGenericTypeDefinition(), typeof<IEnumerator<_>>))
            isEnumerable || isGenericEnumerable)

    type CachedTypeInfo<'a>() =        
        static member val Info = {
            // special cases
            // - strings already have logging
            // - don't want enumerators to execute
            typeName = "Handle " + typeToString typeof<'a>
            isEmpty = isEmptyType typeof<'a>
            }

    let formatMessagesTo (sb : StringBuilder) (formatMsg : _ -> string) (batch : ReadOnlySpan<_>) maxCount =
        let printCount = min batch.Length maxCount
        for i = 0 to printCount - 1 do
            let msg = batch.[i]
            sb.AppendLine() |> ignore
            sb.Append("  ") |> ignore
            sb.Append(formatMsg msg) |> ignore
            //sb.Append(sprintf "%A" msg) |> ignore
        // count of messages not printed
        let remaining = batch.Length - printCount
        if remaining > 0 then
            sb.AppendLine() |> ignore
            sb.Append(sprintf "  +%d" remaining) |> ignore
