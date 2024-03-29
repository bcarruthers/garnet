﻿namespace Garnet.Composition

open Garnet.Composition.Comparisons

module Join =
    module Array =
        let iter1 action param mask (s1 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then action param s1.[i]
                m <- m >>> 1
                i <- i + 1
    
        let iter2 action param mask struct(s1 : _[], s2 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then action param struct(s1.[i], s2.[i])
                m <- m >>> 1
                i <- i + 1
    
        let iter3 action param mask struct(s1 : _[], s2 : _[], s3 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then action param struct(s1.[i], s2.[i], s3.[i])
                m <- m >>> 1
                i <- i + 1

        let iter4 action param mask struct(s1 : _[], s2 : _[], s3 : _[], s4 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then action param struct(s1.[i], s2.[i], s3.[i], s4.[i])
                m <- m >>> 1
                i <- i + 1

        let iter5 action param mask struct(s1 : _[], s2 : _[], s3 : _[], s4 : _[], s5 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then action param struct(s1.[i], s2.[i], s3.[i], s4.[i], s5.[i])
                m <- m >>> 1
                i <- i + 1

        let iter6 action param mask struct(s1 : _[], s2 : _[], s3 : _[], s4 : _[], s5 : _[], sf : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then action param struct(s1.[i], s2.[i], s3.[i], s4.[i], s5.[i], sf.[i])
                m <- m >>> 1
                i <- i + 1

        let iter7 action param mask struct(s1 : _[], s2 : _[], s3 : _[], s4 : _[], s5 : _[], s6 : _[], s7 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then action param struct(s1.[i], s2.[i], s3.[i], s4.[i], s5.[i], s6.[i], s7.[i])
                m <- m >>> 1
                i <- i + 1

        let update1 map param mask (s1 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then s1.[i] <- map param s1.[i]
                m <- m >>> 1
                i <- i + 1
    
        let update2 map param mask struct(s1 : _[], s2 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then s1.[i] <- map param struct(s1.[i], s2.[i])
                m <- m >>> 1
                i <- i + 1
    
        let update3 map param mask struct(s1 : _[], s2 : _[], s3 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then s1.[i] <- map param struct(s1.[i], s2.[i], s3.[i])
                m <- m >>> 1
                i <- i + 1

        let update4 map param mask struct(s1 : _[], s2 : _[], s3 : _[], s4 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then s1.[i] <- map param struct(s1.[i], s2.[i], s3.[i], s4.[i])
                m <- m >>> 1
                i <- i + 1

        let update5 map param mask struct(s1 : _[], s2 : _[], s3 : _[], s4 : _[], s5 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then s1.[i] <- map param struct(s1.[i], s2.[i], s3.[i], s4.[i], s5.[i])
                m <- m >>> 1
                i <- i + 1

        let update6 map param mask struct(s1 : _[], s2 : _[], s3 : _[], s4 : _[], s5 : _[], s6 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then s1.[i] <- map param struct(s1.[i], s2.[i], s3.[i], s4.[i], s5.[i], s6.[i])
                m <- m >>> 1
                i <- i + 1

        let update7 map param mask struct(s1 : _[], s2 : _[], s3 : _[], s4 : _[], s5 : _[], s6 : _[], s7 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then s1.[i] <- map param struct(s1.[i], s2.[i], s3.[i], s4.[i], s5.[i], s6.[i], s7.[i])
                m <- m >>> 1
                i <- i + 1

        let add1 map param mask struct(sr : _[], s1 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then sr.[i] <- map param s1.[i]
                m <- m >>> 1
                i <- i + 1
    
        let add2 map param mask struct(sr : _[], s1 : _[], s2 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then sr.[i] <- map param struct(s1.[i], s2.[i])
                m <- m >>> 1
                i <- i + 1

        let add3 map param mask struct(sr : _[], s1 : _[], s2 : _[], s3 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then sr.[i] <- map param struct(s1.[i], s2.[i], s3.[i])
                m <- m >>> 1
                i <- i + 1
            
        let add4 map param mask struct(sr : _[], s1 : _[], s2 : _[], s3 : _[], s4 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then sr.[i] <- map param struct(s1.[i], s2.[i], s3.[i], s4.[i])
                m <- m >>> 1
                i <- i + 1
            
        let add5 map param mask struct(sr : _[], s1 : _[], s2 : _[], s3 : _[], s4 : _[], s5 : _[]) =
            let mutable m = mask
            let mutable i = 0
            while m <> 0UL do
                if m &&& 1UL <> 0UL then sr.[i] <- map param struct(s1.[i], s2.[i], s3.[i], s4.[i], s5.[i])
                m <- m >>> 1
                i <- i + 1

        let fold1 action initState param mask (s1 : _[]) =
            let mutable m = mask
            let mutable i = 0
            let mutable state = initState
            while m <> 0UL do
                if m &&& 1UL <> 0UL then state <- action param state s1.[i]
                m <- m >>> 1
                i <- i + 1
            state

        let fold2 action initState param mask struct(s1 : _[], s2 : _[]) =
            let mutable m = mask
            let mutable i = 0
            let mutable state = initState
            while m <> 0UL do
                if m &&& 1UL <> 0UL then state <- action param state struct(s1.[i], s2.[i])
                m <- m >>> 1
                i <- i + 1
            state
    
        let fold3 action initState param mask struct(s1 : _[], s2 : _[], s3 : _[]) =
            let mutable m = mask
            let mutable i = 0
            let mutable state = initState
            while m <> 0UL do
                if m &&& 1UL <> 0UL then state <- action param state struct(s1.[i], s2.[i], s3.[i])
                m <- m >>> 1
                i <- i + 1
            state        
    
        let fold4 action initState param mask struct(s1 : _[], s2 : _[], s3 : _[], s4 : _[]) =
            let mutable m = mask
            let mutable i = 0
            let mutable state = initState
            while m <> 0UL do
                if m &&& 1UL <> 0UL then state <- action param state struct(s1.[i], s2.[i], s3.[i], s4.[i])
                m <- m >>> 1
                i <- i + 1
            state        
    
        let fold5 action initState param mask struct(s1 : _[], s2 : _[], s3 : _[], s4 : _[], s5 : _[]) =
            let mutable m = mask
            let mutable i = 0
            let mutable state = initState
            while m <> 0UL do
                if m &&& 1UL <> 0UL then state <- action param state struct(s1.[i], s2.[i], s3.[i], s4.[i], s5.[i])
                m <- m >>> 1
                i <- i + 1
            state        

    module Joins =
        let count (a : Segments<_,_>) = a.Count
        let get (a : Segments<_,_>) i = a.[i]
        let find (a : Segments<_,_>) sid =
            match a.TryFind(sid) with
            | true, i -> i
            | false, _ -> -1
        
        let iter1 action a param =
            let c1 = count a
            for i1 = 0 to c1 - 1 do
                let s1 = get a i1
                action param s1.Mask s1.Data
    
        let iter2 action struct(a, b) param =
            let c1 = count a
            let c2 = count b
            let mutable i1 = 0
            let mutable i2 = 0
            while i1 < c1 && i2 < c2 do
                let s1 = get a i1
                let s2 = get b i2
                let n1 = s1.Id
                let n2 = s2.Id
                if n1 < n2 then i1 <- i1 + 1
                elif n2 < n1 then i2 <- i2 + 1
                else
                    let mask = s1.Mask &&& s2.Mask
                    if mask <> 0UL then action param mask struct(s1.Data, s2.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
    
        let iter3 action struct(a, b, c) param =
            let c1 = count a
            let c2 = count b
            let c3 = count c
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable i3 = 0
            while i1 < c1 && i2 < c2 && i3 < c3 do
                let s1 = get a i1
                let s2 = get b i2
                let s3 = get c i3
                let n1 = s1.Id
                let n2 = s2.Id
                let n3 = s3.Id
                if n1 < n2 || n1 < n3 then i1 <- i1 + 1
                elif n2 < n1 || n2 < n3 then i2 <- i2 + 1
                elif n3 < n1 || n3 < n2 then i3 <- i3 + 1
                else
                    let mask = s1.Mask &&& s2.Mask &&& s3.Mask
                    if mask <> 0UL then action param mask struct(s1.Data, s2.Data, s3.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
                    i3 <- i3 + 1

        let iter4 action struct(a, b, c, d) param =
            let c1 = count a
            let c2 = count b
            let c3 = count c
            let c4 = count d
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable i3 = 0
            let mutable i4 = 0
            while i1 < c1 && i2 < c2 && i3 < c3 && i4 < c4 do
                let s1 = get a i1
                let s2 = get b i2
                let s3 = get c i3
                let s4 = get d i4
                let n1 = s1.Id
                let n2 = s2.Id
                let n3 = s3.Id
                let n4 = s4.Id
                if n1 < n2 || n1 < n3 || n1 < n4 then i1 <- i1 + 1
                elif n2 < n1 || n2 < n3 || n2 < n4 then i2 <- i2 + 1
                elif n3 < n1 || n3 < n2 || n3 < n4 then i3 <- i3 + 1
                elif n4 < n1 || n4 < n2 || n4 < n3 then i4 <- i4 + 1
                else
                    let mask = s1.Mask &&& s2.Mask &&& s3.Mask &&& s4.Mask
                    if mask <> 0UL then action param mask struct(s1.Data, s2.Data, s3.Data, s4.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
                    i3 <- i3 + 1
                    i4 <- i4 + 1

        let iter5 action struct(a, b, c, d, e) param =
            let c1 = count a
            let c2 = count b
            let c3 = count c
            let c4 = count d
            let c5 = count e
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable i3 = 0
            let mutable i4 = 0
            let mutable i5 = 0
            while i1 < c1 && i2 < c2 && i3 < c3 && i4 < c4 && i5 < c5 do
                let s1 = get a i1
                let s2 = get b i2
                let s3 = get c i3
                let s4 = get d i4
                let s5 = get e i5
                let n1 = s1.Id
                let n2 = s2.Id
                let n3 = s3.Id
                let n4 = s4.Id
                let n5 = s5.Id
                if n1 < n2 || n1 < n3 || n1 < n4 || n1 < n5 then i1 <- i1 + 1
                elif n2 < n1 || n2 < n3 || n2 < n4 || n2 < n5 then i2 <- i2 + 1
                elif n3 < n1 || n3 < n2 || n3 < n4 || n3 < n5 then i3 <- i3 + 1
                elif n4 < n1 || n4 < n2 || n4 < n3 || n4 < n5 then i4 <- i4 + 1
                elif n5 < n1 || n5 < n2 || n5 < n3 || n5 < n4 then i5 <- i5 + 1
                else
                    let mask = s1.Mask &&& s2.Mask &&& s3.Mask &&& s4.Mask &&& s5.Mask
                    if mask <> 0UL then action param mask struct(s1.Data, s2.Data, s3.Data, s4.Data, s5.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
                    i3 <- i3 + 1
                    i4 <- i4 + 1
                    i5 <- i5 + 1
  
        let iter6 action struct(a, b, c, d, e, f) param =
            let c1 = count a
            let c2 = count b
            let c3 = count c
            let c4 = count d
            let c5 = count e
            let c6 = count f
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable i3 = 0
            let mutable i4 = 0
            let mutable i5 = 0
            let mutable i6 = 0
            while i1 < c1 && i2 < c2 && i3 < c3 && i4 < c4 && i5 < c5 && i6 < c6 do
                let s1 = get a i1
                let s2 = get b i2
                let s3 = get c i3
                let s4 = get d i4
                let s5 = get e i5
                let s6 = get f i6
                let n1 = s1.Id
                let n2 = s2.Id
                let n3 = s3.Id
                let n4 = s4.Id
                let n5 = s5.Id
                let n6 = s6.Id
                if   n1 < n2 || n1 < n3 || n1 < n4 || n1 < n5 || n1 < n6 then i1 <- i1 + 1
                elif n2 < n1 || n2 < n3 || n2 < n4 || n2 < n5 || n2 < n6 then i2 <- i2 + 1
                elif n3 < n1 || n3 < n2 || n3 < n4 || n3 < n5 || n3 < n6 then i3 <- i3 + 1
                elif n4 < n1 || n4 < n2 || n4 < n3 || n4 < n5 || n4 < n6 then i4 <- i4 + 1
                elif n5 < n1 || n5 < n2 || n5 < n3 || n5 < n4 || n5 < n6 then i5 <- i5 + 1
                elif n6 < n1 || n6 < n2 || n6 < n3 || n6 < n4 || n6 < n5 then i6 <- i6 + 1
                else
                    let mask = s1.Mask &&& s2.Mask &&& s3.Mask &&& s4.Mask &&& s5.Mask &&& s6.Mask
                    if mask <> 0UL then action param mask struct(s1.Data, s2.Data, s3.Data, s4.Data, s5.Data, s6.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
                    i3 <- i3 + 1
                    i4 <- i4 + 1
                    i5 <- i5 + 1
                    i6 <- i6 + 1

        let iter7 action struct(a, b, c, d, e, f, g) param =
            let c1 = count a
            let c2 = count b
            let c3 = count c
            let c4 = count d
            let c5 = count e
            let c6 = count f
            let c7 = count g
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable i3 = 0
            let mutable i4 = 0
            let mutable i5 = 0
            let mutable i6 = 0
            let mutable i7 = 0
            while i1 < c1 && i2 < c2 && i3 < c3 && i4 < c4 && i5 < c5 && i6 < c6 && i7 < c7 do
                let s1 = get a i1
                let s2 = get b i2
                let s3 = get c i3
                let s4 = get d i4
                let s5 = get e i5
                let s6 = get f i6
                let s7 = get g i7
                let n1 = s1.Id
                let n2 = s2.Id
                let n3 = s3.Id
                let n4 = s4.Id
                let n5 = s5.Id
                let n6 = s6.Id
                let n7 = s7.Id
                if   n1 < n2 || n1 < n3 || n1 < n4 || n1 < n5 || n1 < n6 || n1 < n7 then i1 <- i1 + 1
                elif n2 < n1 || n2 < n3 || n2 < n4 || n2 < n5 || n2 < n6 || n2 < n7 then i2 <- i2 + 1
                elif n3 < n1 || n3 < n2 || n3 < n4 || n3 < n5 || n3 < n6 || n3 < n7 then i3 <- i3 + 1
                elif n4 < n1 || n4 < n2 || n4 < n3 || n4 < n5 || n4 < n6 || n4 < n7 then i4 <- i4 + 1
                elif n5 < n1 || n5 < n2 || n5 < n3 || n5 < n4 || n5 < n6 || n5 < n7 then i5 <- i5 + 1
                elif n6 < n1 || n6 < n2 || n6 < n3 || n6 < n4 || n6 < n5 || n6 < n7 then i6 <- i6 + 1
                elif n7 < n1 || n7 < n2 || n7 < n3 || n7 < n4 || n7 < n5 || n7 < n6 then i7 <- i7 + 1
                else
                    let mask = s1.Mask &&& s2.Mask &&& s3.Mask &&& s4.Mask &&& s5.Mask &&& s6.Mask &&& s7.Mask
                    if mask <> 0UL then action param mask struct(s1.Data, s2.Data, s3.Data, s4.Data, s5.Data, s6.Data, s7.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
                    i3 <- i3 + 1
                    i4 <- i4 + 1
                    i5 <- i5 + 1
                    i6 <- i6 + 1
                    i7 <- i7 + 1
        
        let iterKey1 action a param =
            let ca = count a
            for ia = 0 to ca - 1 do
                let sa = a.[ia]
                action param sa.Mask sa.Id sa.Data
    
        let iterKey2 action struct(a, b) param =
            let ca = count a
            let cb = count b
            let mutable ia = 0
            let mutable ib = 0
            while ia < ca && ib < cb do
                let sa = a.[ia]
                let sb = b.[ib]
                let na = sa.Id
                let nb = sb.Id
                if na < nb then ia <- ia + 1
                elif nb < na then ib <- ib + 1
                else
                    let mask = sa.Mask &&& sb.Mask
                    if mask <> 0UL then action param mask na struct(sa.Data, sb.Data)
                    ia <- ia + 1
                    ib <- ib + 1
    
        let iterKey3 action struct(a, b, c) param =
            let ca = count a
            let cb = count b
            let cc = count c
            let mutable ia = 0
            let mutable ib = 0
            let mutable ic = 0
            while ia < ca && ib < cb && ic < cc do
                let sa = a.[ia]
                let sb = b.[ib]
                let sc = c.[ic]
                let na = sa.Id
                let nb = sb.Id
                let nc = sc.Id
                if na < nb || na < nc then ia <- ia + 1
                elif nb < na || nb < nc then ib <- ib + 1
                elif nc < na || nc < nb then ic <- ic + 1
                else
                    let mask = sa.Mask &&& sb.Mask &&& sc.Mask
                    if mask <> 0UL then action param mask na struct(sa.Data, sb.Data, sc.Data)
                    ia <- ia + 1
                    ib <- ib + 1
                    ic <- ic + 1

        let iterKey4 action struct(a, b, c, d) param =
            let ca = count a
            let cb = count b
            let cc = count c
            let cd = count d
            let mutable ia = 0
            let mutable ib = 0
            let mutable ic = 0
            let mutable id = 0
            while ia < ca && ib < cb && ic < cc && id < cd do
                let sa = a.[ia]
                let sb = b.[ib]
                let sc = c.[ic]
                let sd = d.[id]
                let na = sa.Id
                let nb = sb.Id
                let nc = sc.Id
                let nd = sd.Id
                if   na < nb || na < nc || na < nd then ia <- ia + 1
                elif nb < na || nb < nc || nb < nd then ib <- ib + 1
                elif nc < na || nc < nb || nc < nd then ic <- ic + 1
                elif nd < na || nd < nb || nd < nc then id <- id + 1
                else
                    let mask = sa.Mask &&& sb.Mask &&& sc.Mask &&& sd.Mask
                    if mask <> 0UL then action param mask na struct(sa.Data, sb.Data, sc.Data, sd.Data)
                    ia <- ia + 1
                    ib <- ib + 1
                    ic <- ic + 1
                    id <- id + 1

        /// Creates components when present in A and missing from R
        let add1 action struct(r, a) param =
            let c1 = count a
            for i1 = 0 to c1 - 1 do
                let s1 = get a i1
                let ir = find r s1.Id
                let mask = s1.Mask
                let sid = s1.Id
                if ir < 0 then
                    let data = r.Add(sid, mask)
                    action param mask struct(data, s1.Data)
                else
                    let sr = r.[ir]
                    let addMask = mask &&& ~~~sr.Mask
                    if addMask <> 0UL then
                        let data = r.Add(sid, addMask)
                        action param addMask struct(data, s1.Data)

        /// Creates components when present in A and B and missing from R
        let add2 action struct(r, a, b) param =
            let c1 = count a
            let c2 = count b
            let mutable i1 = 0
            let mutable i2 = 0
            while i1 < c1 && i2 < c2 do
                let s1 = get a i1
                let s2 = get b i2
                let n1 = s1.Id
                let n2 = s2.Id
                if n1 < n2 then i1 <- i1 + 1
                elif n2 < n1 then i2 <- i2 + 1
                else
                    let mask = s1.Mask &&& s2.Mask
                    if mask <> 0UL then 
                        let sid = s1.Id
                        let ir = find r sid
                        if ir < 0 then
                            let data = r.Add(sid, mask)
                            action param mask struct(data, s1.Data, s2.Data)
                        else
                            let sr = r.[ir]
                            let addMask = mask &&& ~~~sr.Mask
                            if addMask <> 0UL then
                                let data = r.Add(sid, addMask)
                                action param addMask struct(data, s1.Data, s2.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1            
    
        /// Creates components when present in A and B and C and missing from R
        let add3 action struct(r, a, b, c) param =
            let c1 = count a
            let c2 = count b
            let c3 = count c
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable i3 = 0
            while i1 < c1 && i2 < c2 && i3 < c3 do
                let s1 = get a i1
                let s2 = get b i2
                let s3 = get c i3
                let n1 = s1.Id
                let n2 = s2.Id
                let n3 = s3.Id
                if n1 < n2 || n1 < n3 then i1 <- i1 + 1
                elif n2 < n1 || n2 < n3 then i2 <- i2 + 1
                elif n3 < n1 || n3 < n2 then i3 <- i3 + 1
                else
                    let mask = s1.Mask &&& s2.Mask &&& s3.Mask
                    if mask <> 0UL then 
                        let sid = s1.Id
                        let ir = find r sid
                        if ir < 0 then
                            let data = r.Add(sid, mask)
                            action param mask struct(data, s1.Data, s2.Data, s3.Data)
                        else
                            let sr = r.[ir]
                            let addMask = mask &&& ~~~sr.Mask
                            if addMask <> 0UL then
                                let data = r.Add(sid, addMask)
                                action param addMask struct(data, s1.Data, s2.Data, s3.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
                    i3 <- i3 + 1

        /// Creates components when present in A and B and C and missing from R
        let add4 action struct(r, a, b, c, d) param =
            let c1 = count a
            let c2 = count b
            let c3 = count c
            let c4 = count d
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable i3 = 0
            let mutable i4 = 0
            while i1 < c1 && i2 < c2 && i3 < c3 && i4 < c4 do
                let s1 = get a i1
                let s2 = get b i2
                let s3 = get c i3
                let s4 = get d i4
                let n1 = s1.Id
                let n2 = s2.Id
                let n3 = s3.Id
                let n4 = s4.Id
                if n1 < n2 || n1 < n3 || n1 < n4 then i1 <- i1 + 1
                elif n2 < n1 || n2 < n3 || n2 < n4 then i2 <- i2 + 1
                elif n3 < n1 || n3 < n2 || n3 < n4 then i3 <- i3 + 1
                elif n4 < n1 || n4 < n2 || n4 < n3 then i4 <- i4 + 1
                else
                    let mask = s1.Mask &&& s2.Mask &&& s3.Mask &&& s4.Mask
                    if mask <> 0UL then 
                        let sid = s1.Id
                        let ir = find r sid
                        if ir < 0 then
                            let data = r.Add(sid, mask)
                            action param mask struct(data, s1.Data, s2.Data, s3.Data, s4.Data)
                        else
                            let sr = r.[ir]
                            let addMask = mask &&& ~~~sr.Mask
                            if addMask <> 0UL then
                                let data = r.Add(sid, addMask)
                                action param addMask struct(data, s1.Data, s2.Data, s3.Data, s4.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
                    i3 <- i3 + 1
                    i4 <- i4 + 1

        let add5 action struct(r, a, b, c, d, e) param =
            let c1 = count a
            let c2 = count b
            let c3 = count c
            let c4 = count d
            let c5 = count e
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable i3 = 0
            let mutable i4 = 0
            let mutable i5 = 0
            while i1 < c1 && i2 < c2 && i3 < c3 && i4 < c4 && i5 < c5 do
                let s1 = get a i1
                let s2 = get b i2
                let s3 = get c i3
                let s4 = get d i4
                let s5 = get e i5
                let n1 = s1.Id
                let n2 = s2.Id
                let n3 = s3.Id
                let n4 = s4.Id
                let n5 = s5.Id
                if n1 < n2 || n1 < n3 || n1 < n4 || n1 < n5 then i1 <- i1 + 1
                elif n2 < n1 || n2 < n3 || n2 < n4 || n2 < n5 then i2 <- i2 + 1
                elif n3 < n1 || n3 < n2 || n3 < n4 || n3 < n5 then i3 <- i3 + 1
                elif n4 < n1 || n4 < n2 || n4 < n3 || n4 < n5 then i4 <- i4 + 1
                elif n5 < n1 || n5 < n2 || n5 < n3 || n5 < n4 then i5 <- i5 + 1
                else
                    let mask = s1.Mask &&& s2.Mask &&& s3.Mask &&& s4.Mask &&& s5.Mask
                    if mask <> 0UL then 
                        let sid = s1.Id
                        let ir = find r sid
                        if ir < 0 then
                            let data = r.Add(sid, mask)
                            action param mask struct(data, s1.Data, s2.Data, s3.Data, s4.Data, s5.Data)
                        else
                            let sr = r.[ir]
                            let addMask = mask &&& ~~~sr.Mask
                            if addMask <> 0UL then
                                let data = r.Add(sid, addMask)
                                action param addMask struct(data, s1.Data, s2.Data, s3.Data, s4.Data, s5.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
                    i3 <- i3 + 1
                    i4 <- i4 + 1
                    i5 <- i5 + 1

        let fold1 action a initState param =
            let mutable state = initState
            let c1 = count a
            for i1 = 0 to c1 - 1 do
                let s1 = a.[i1]
                state <- action state param s1.Mask s1.Data
            state
            
        let fold2 action struct(a, b) initState param =
            let c1 = count a
            let c2 = count b
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable state = initState
            while i1 < c1 && i2 < c2 do
                let s1 = a.[i1]
                let s2 = b.[i2]
                let n1 = s1.Id
                let n2 = s2.Id
                if n1 < n2 then i1 <- i1 + 1
                elif n2 < n1 then i2 <- i2 + 1
                else
                    let mask = s1.Mask &&& s2.Mask
                    if mask <> 0UL then state <- action state param mask struct(s1.Data, s2.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
            state    

        let fold3 action struct(a, b, c) initState param =
            let c1 = count a
            let c2 = count b
            let c3 = count c
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable i3 = 0
            let mutable state = initState
            while i1 < c1 && i2 < c2 && i3 < c3 do
                let s1 = a.[i1]
                let s2 = b.[i2]
                let s3 = c.[i3]
                let n1 = s1.Id
                let n2 = s2.Id
                let n3 = s3.Id
                if n1 < n2 || n1 < n3 then i1 <- i1 + 1
                elif n2 < n1 || n2 < n3 then i2 <- i2 + 1
                elif n3 < n1 || n3 < n2 then i3 <- i3 + 1
                else
                    let mask = s1.Mask &&& s2.Mask &&& s3.Mask
                    if mask <> 0UL then state <- action state param mask struct(s1.Data, s2.Data, s3.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
                    i3 <- i3 + 1
            state

        let fold4 action struct(a, b, c, d) initState param =
            let c1 = count a
            let c2 = count b
            let c3 = count c
            let c4 = count d
            let mutable i1 = 0
            let mutable i2 = 0
            let mutable i3 = 0
            let mutable id = 0
            let mutable state = initState
            while i1 < c1 && i2 < c2 && i3 < c3 && id < c4 do
                let s1 = get a i1
                let s2 = get b i2
                let s3 = get c i3
                let s4 = get d id
                let n1 = s1.Id
                let n2 = s2.Id
                let n3 = s3.Id
                let n4 = s4.Id
                if n1 < n2 || n1 < n3 || n1 < n4 then i1 <- i1 + 1
                elif n2 < n1 || n2 < n3 || n2 < n4 then i2 <- i2 + 1
                elif n3 < n1 || n3 < n2 || n3 < n4 then i3 <- i3 + 1
                elif n4 < n1 || n4 < n2 || n4 < n3 then id <- id + 1
                else
                    let mask = s1.Mask &&& s2.Mask &&& s3.Mask &&& s4.Mask
                    if mask <> 0UL then state <- action state param mask struct(s1.Data, s2.Data, s3.Data, s4.Data)
                    i1 <- i1 + 1
                    i2 <- i2 + 1
                    i3 <- i3 + 1
                    id <- id + 1
            state

    module Segments =
        let get (c : ISegmentStore<_>) = c.GetSegments()
        let get2 c = struct(get c, get c)
        let get3 c = struct(get c, get c, get c)
        let get4 c = struct(get c, get c, get c, get c)
        let get5 c = struct(get c, get c, get c, get c, get c)
        let get6 c = struct(get c, get c, get c, get c, get c, get c)
        let get7 c = struct(get c, get c, get c, get c, get c, get c, get c)
        
        let iter1 a c = get c |> Joins.iter1 a    
        let iter2 a c = get2 c |> Joins.iter2 a
        let iter3 a c = get3 c |> Joins.iter3 a    
        let iter4 a c = get4 c |> Joins.iter4 a
        let iter5 a c = get5 c |> Joins.iter5 a
        let iter6 a c = get6 c |> Joins.iter6 a
        let iter7 a c = get7 c |> Joins.iter7 a

        let iterKey1 a c = get c |> Joins.iterKey1 a    
        let iterKey2 a c = get2 c |> Joins.iterKey2 a
        let iterKey3 a c = get3 c |> Joins.iterKey3 a    
        let iterKey4 a c = get4 c |> Joins.iterKey4 a    

        let add1 a c = get2 c |> Joins.add1 a
        let add2 a c = get3 c |> Joins.add2 a
        let add3 a c = get4 c |> Joins.add3 a
        let add4 a c = get5 c |> Joins.add4 a
        let add5 a c = get6 c |> Joins.add5 a

        let fold1 a c = get c |> Joins.fold1 a
        let fold2 a c = get2 c |> Joins.fold2 a
        let fold3 a c = get3 c |> Joins.fold3 a
        let fold4 a c = get4 c |> Joins.fold4 a

    let iter1 a = a |> Array.iter1 |> Segments.iter1
    let iter2 a = a |> Array.iter2 |> Segments.iter2
    let iter3 a = a |> Array.iter3 |> Segments.iter3
    let iter4 a = a |> Array.iter4 |> Segments.iter4
    let iter5 a = a |> Array.iter5 |> Segments.iter5
    let iter6 a = a |> Array.iter6 |> Segments.iter6
    let iter7 a = a |> Array.iter7 |> Segments.iter7

    let add1 a = a |> Array.add1 |> Segments.add1
    let add2 a = a |> Array.add2 |> Segments.add2        
    let add3 a = a |> Array.add3 |> Segments.add3
    let add4 a = a |> Array.add4 |> Segments.add4
    let add5 a = a |> Array.add5 |> Segments.add5

    let update1 a = a |> Array.update1 |> Segments.iter1
    let update2 a = a |> Array.update2 |> Segments.iter2
    let update3 a = a |> Array.update3 |> Segments.iter3
    let update4 a = a |> Array.update4 |> Segments.iter4
    let update5 a = a |> Array.update5 |> Segments.iter5
    let update6 a = a |> Array.update6 |> Segments.iter6
    let update7 a = a |> Array.update7 |> Segments.iter7

    let fold1 a = a |> Array.fold1 |> Segments.fold1
    let fold2 a = a |> Array.fold2 |> Segments.fold2
    let fold3 a = a |> Array.fold3 |> Segments.fold3
    let fold4 a = a |> Array.fold4 |> Segments.fold4

    let list procs = fun c -> procs |> List.iter (fun proc -> proc c)

    /// Takes a single event processor and returns a processor which operates on a batch of events
    let batch proc =
        fun c -> 
            let run = proc c
            fun (events : List<_>) ->
                for e in events do
                    run e

    let where pred action =
        fun param values ->
            if pred param values then
                action param values

    let over c proc =
        proc c

    let run param iter = iter param
    