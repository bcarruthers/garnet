namespace Garnet.Formatting

open System
open System.Collections
open System.Collections.Generic
open System.Text
open Garnet
open Garnet.Comparisons
open Garnet.Metrics

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
        canPrint : bool
        canSendTimings : bool
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

    let isLoggingType<'a> =
        obj.ReferenceEquals(typeof<'a>, typeof<string>) ||
        obj.ReferenceEquals(typeof<'a>, typeof<Timing>)

    type CachedTypeInfo<'a>() =        
        static member val Info = {
            // special cases
            // - strings already have logging
            // - don't want enumerators to execute
            canPrint = not (isLoggingType<'a> || isEnumerable<'a>) 
            canSendTimings = not (obj.ReferenceEquals(typeof<'a>, typeof<Timing>))
            typeName = "Handle " + typeToString typeof<'a>
            isEmpty = isEmptyType typeof<'a>
            }

    let formatMessagesTo (sb : StringBuilder) (formatMsg : _ -> string) (batch : Buffer<_>) maxCount =
        let printCount = min batch.Count maxCount
        for i = 0 to printCount - 1 do
            let msg = batch.[i]
            sb.AppendLine() |> ignore
            sb.Append("  ") |> ignore
            sb.Append(formatMsg msg) |> ignore
            //sb.Append(sprintf "%A" msg) |> ignore
        // count of messages not printed
        let remaining = batch.Count - printCount
        if remaining > 0 then
            sb.AppendLine() |> ignore
            sb.Append(sprintf "  +%d" remaining) |> ignore
