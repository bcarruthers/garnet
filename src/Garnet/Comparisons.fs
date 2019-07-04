namespace Garnet

// The purpose of this is to avoid equality operator allocations
// https://github.com/dotnet/fsharp/issues/526
#nowarn "86"
module Comparisons =
    let inline eq<'a when 'a :> System.IEquatable<'a>> (x:'a) (y:'a) = x.Equals y    
    let inline (=) x y = eq x y
    let inline (<>) x y = not (eq x y)
     
    let inline (=@) x y = Microsoft.FSharp.Core.Operators.(=) x y
    let inline (<>@) x y = Microsoft.FSharp.Core.Operators.(<>) x y

    let inline lt<'a when 'a :> System.IComparable<'a>> (x:'a) (y:'a) = x.CompareTo(y) < 0
    let inline gt<'a when 'a :> System.IComparable<'a>> (x:'a) (y:'a) = x.CompareTo(y) > 0
    let inline lte<'a when 'a :> System.IComparable<'a>> (x:'a) (y:'a) = x.CompareTo(y) <= 0
    let inline gte<'a when 'a :> System.IComparable<'a>> (x:'a) (y:'a) = x.CompareTo(y) >= 0
    let inline (<) x y = lt x y
    let inline (>) x y = gt x y
    let inline (<=) x y = lte x y
    let inline (>=) x y = gte x y

    let inline isNull x = obj.ReferenceEquals(x, null)
    let inline isNotNull x = not (isNull x)
