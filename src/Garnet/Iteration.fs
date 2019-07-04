module Garnet.Ecs.Iter

open Garnet.Comparisons

module Array =
    let iter1 action param mask (sa : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then action param sa.[i]
            m <- m >>> 1
            i <- i + 1
    
    let iter2 action param mask struct(sa : _[], sb : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then action param struct(sa.[i], sb.[i])
            m <- m >>> 1
            i <- i + 1
    
    let iter3 action param mask struct(sa : _[], sb : _[], sc : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then action param struct(sa.[i], sb.[i], sc.[i])
            m <- m >>> 1
            i <- i + 1

    let iter4 action param mask struct(sa : _[], sb : _[], sc : _[], sd : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then action param struct(sa.[i], sb.[i], sc.[i], sd.[i])
            m <- m >>> 1
            i <- i + 1

    let iter5 action param mask struct(sa : _[], sb : _[], sc : _[], sd : _[], se : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then action param struct(sa.[i], sb.[i], sc.[i], sd.[i], se.[i])
            m <- m >>> 1
            i <- i + 1

    let update1 map param mask (sa : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then sa.[i] <- map param sa.[i]
            m <- m >>> 1
            i <- i + 1
    
    let update2 map param mask struct(sa : _[], sb : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then sa.[i] <- map param struct(sa.[i], sb.[i])
            m <- m >>> 1
            i <- i + 1
    
    let update3 map param mask struct(sa : _[], sb : _[], sc : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then sa.[i] <- map param struct(sa.[i], sb.[i], sc.[i])
            m <- m >>> 1
            i <- i + 1

    let update4 map param mask struct(sa : _[], sb : _[], sc : _[], sd : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then sa.[i] <- map param struct(sa.[i], sb.[i], sc.[i], sd.[i])
            m <- m >>> 1
            i <- i + 1

    let update5 map param mask struct(sa : _[], sb : _[], sc : _[], sd : _[], se : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then sa.[i] <- map param struct(sa.[i], sb.[i], sc.[i], sd.[i], se.[i])
            m <- m >>> 1
            i <- i + 1

    let create1 map param mask struct(sr : _[], sa : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then sr.[i] <- map param sa.[i]
            m <- m >>> 1
            i <- i + 1
    
    let create2 map param mask struct(sr : _[], sa : _[], sb : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then sr.[i] <- map param struct(sa.[i], sb.[i])
            m <- m >>> 1
            i <- i + 1

    let create3 map param mask struct(sr : _[], sa : _[], sb : _[], sc : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then sr.[i] <- map param struct(sa.[i], sb.[i], sc.[i])
            m <- m >>> 1
            i <- i + 1
            
    let create4 map param mask struct(sr : _[], sa : _[], sb : _[], sc : _[], sd : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then sr.[i] <- map param struct(sa.[i], sb.[i], sc.[i], sd.[i])
            m <- m >>> 1
            i <- i + 1
            
    let create5 map param mask struct(sr : _[], sa : _[], sb : _[], sc : _[], sd : _[], se : _[]) =
        let mutable m = mask
        let mutable i = 0
        while m <> 0UL do
            if m &&& 1UL <> 0UL then sr.[i] <- map param struct(sa.[i], sb.[i], sc.[i], sd.[i], se.[i])
            m <- m >>> 1
            i <- i + 1

    let fold1 action initState param mask (sa : _[]) =
        let mutable m = mask
        let mutable i = 0
        let mutable state = initState
        while m <> 0UL do
            if m &&& 1UL <> 0UL then state <- action param state sa.[i]
            m <- m >>> 1
            i <- i + 1
        state

    let fold2 action initState param mask struct(sa : _[], sb : _[]) =
        let mutable m = mask
        let mutable i = 0
        let mutable state = initState
        while m <> 0UL do
            if m &&& 1UL <> 0UL then state <- action param state struct(sa.[i], sb.[i])
            m <- m >>> 1
            i <- i + 1
        state
    
    let fold3 action initState param mask struct(sa : _[], sb : _[], sc : _[]) =
        let mutable m = mask
        let mutable i = 0
        let mutable state = initState
        while m <> 0UL do
            if m &&& 1UL <> 0UL then state <- action param state struct(sa.[i], sb.[i], sc.[i])
            m <- m >>> 1
            i <- i + 1
        state        
    
    let fold4 action initState param mask struct(sa : _[], sb : _[], sc : _[], sd : _[]) =
        let mutable m = mask
        let mutable i = 0
        let mutable state = initState
        while m <> 0UL do
            if m &&& 1UL <> 0UL then state <- action param state struct(sa.[i], sb.[i], sc.[i], sd.[i])
            m <- m >>> 1
            i <- i + 1
        state        
    
    let fold5 action initState param mask struct(sa : _[], sb : _[], sc : _[], sd : _[], se : _[]) =
        let mutable m = mask
        let mutable i = 0
        let mutable state = initState
        while m <> 0UL do
            if m &&& 1UL <> 0UL then state <- action param state struct(sa.[i], sb.[i], sc.[i], sd.[i], se.[i])
            m <- m >>> 1
            i <- i + 1
        state        

module Joins =
    let count (a : Segments<_,_>) = a.Count
    let get (a : Segments<_,_>) i = a.[i]
    let find (a : Segments<_,_>) sid = a.Find sid
        
    let iter1 action a param =
        let ca = count a
        for ia = 0 to ca - 1 do
            let sa = get a ia
            action param sa.mask sa.data
    
    let iter2 action struct(a, b) param =
        let ca = count a
        let cb = count b
        let mutable ia = 0
        let mutable ib = 0
        while ia < ca && ib < cb do
            let sa = get a ia
            let sb = get b ib
            let na = sa.id
            let nb = sb.id
            if na < nb then ia <- ia + 1
            elif nb < na then ib <- ib + 1
            else
                let mask = sa.mask &&& sb.mask
                if mask <> 0UL then action param mask struct(sa.data, sb.data)
                ia <- ia + 1
                ib <- ib + 1
    
    let iter3 action struct(a, b, c) param =
        let ca = count a
        let cb = count b
        let cc = count c
        let mutable ia = 0
        let mutable ib = 0
        let mutable ic = 0
        while ia < ca && ib < cb && ic < cc do
            let sa = get a ia
            let sb = get b ib
            let sc = get c ic
            let na = sa.id
            let nb = sb.id
            let nc = sc.id
            if na < nb || na < nc then ia <- ia + 1
            elif nb < na || nb < nc then ib <- ib + 1
            elif nc < na || nc < nb then ic <- ic + 1
            else
                let mask = sa.mask &&& sb.mask &&& sc.mask
                if mask <> 0UL then action param mask struct(sa.data, sb.data, sc.data)
                ia <- ia + 1
                ib <- ib + 1
                ic <- ic + 1

    let iter4 action struct(a, b, c, d) param =
        let ca = count a
        let cb = count b
        let cc = count c
        let cd = count d
        let mutable ia = 0
        let mutable ib = 0
        let mutable ic = 0
        let mutable id = 0
        while ia < ca && ib < cb && ic < cc && id < cd do
            let sa = get a ia
            let sb = get b ib
            let sc = get c ic
            let sd = get d id
            let na = sa.id
            let nb = sb.id
            let nc = sc.id
            let nd = sd.id
            if na < nb || na < nc || na < nd then ia <- ia + 1
            elif nb < na || nb < nc || nb < nd then ib <- ib + 1
            elif nc < na || nc < nb || nc < nd then ic <- ic + 1
            elif nd < na || nd < nb || nd < nc then id <- id + 1
            else
                let mask = sa.mask &&& sb.mask &&& sc.mask &&& sd.mask
                if mask <> 0UL then action param mask struct(sa.data, sb.data, sc.data, sd.data)
                ia <- ia + 1
                ib <- ib + 1
                ic <- ic + 1
                id <- id + 1

    let iter5 action struct(a, b, c, d, e) param =
        let ca = count a
        let cb = count b
        let cc = count c
        let cd = count d
        let ce = count e
        let mutable ia = 0
        let mutable ib = 0
        let mutable ic = 0
        let mutable id = 0
        let mutable ie = 0
        while ia < ca && ib < cb && ic < cc && id < cd && id < ce do
            let sa = get a ia
            let sb = get b ib
            let sc = get c ic
            let sd = get d id
            let se = get e ie
            let na = sa.id
            let nb = sb.id
            let nc = sc.id
            let nd = sd.id
            let ne = se.id
            if na < nb || na < nc || na < nd || na < ne then ia <- ia + 1
            elif nb < na || nb < nc || nb < nd || nb < ne then ib <- ib + 1
            elif nc < na || nc < nb || nc < nd || nc < ne then ic <- ic + 1
            elif nd < na || nd < nb || nd < nc || nd < ne then id <- id + 1
            elif ne < na || ne < nb || ne < nc || ne < nd then ie <- ie + 1
            else
                let mask = sa.mask &&& sb.mask &&& sc.mask &&& sd.mask &&& se.mask
                if mask <> 0UL then action param mask struct(sa.data, sb.data, sc.data, sd.data, se.data)
                ia <- ia + 1
                ib <- ib + 1
                ic <- ic + 1
                id <- id + 1
                ie <- ie + 1
  
    /// Creates components when present in A and missing from R
    let create1 action struct(r, a) param =
        let ca = count a
        for ia = 0 to ca - 1 do
            let sa = get a ia
            let ir = find r sa.id
            let mask = sa.mask
            let sid = sa.id
            if ir < 0 then
                let data = r.AddMask(sid, mask)
                action param mask struct(data, sa.data)
            else
                let sr = r.[ir]
                let createMask = mask &&& ~~~sr.mask
                if createMask <> 0UL then
                    let data = r.AddMask(sid, createMask)
                    action param mask struct(data, sa.data)

    /// Creates components when present in A and B and missing from R
    let create2 action struct(r, a, b) param =
        let ca = count a
        let cb = count b
        let mutable ia = 0
        let mutable ib = 0
        while ia < ca && ib < cb do
            let sa = get a ia
            let sb = get b ib
            let na = sa.id
            let nb = sb.id
            if na < nb then ia <- ia + 1
            elif nb < na then ib <- ib + 1
            else
                let mask = sa.mask &&& sb.mask
                if mask <> 0UL then 
                    let sid = sa.id
                    let ir = find r sid
                    if ir < 0 then
                        let data = r.AddMask(sid, mask)
                        action param mask struct(data, sa.data, sb.data)
                    else
                        let sr = r.[ir]
                        let createMask = mask &&& ~~~sr.mask
                        if createMask <> 0UL then
                            let data = r.AddMask(sid, createMask)
                            action param mask struct(data, sa.data, sb.data)
                ia <- ia + 1
                ib <- ib + 1            
    
    /// Creates components when present in A and B and C and missing from R
    let create3 action struct(r, a, b, c) param =
        let ca = count a
        let cb = count b
        let cc = count c
        let mutable ia = 0
        let mutable ib = 0
        let mutable ic = 0
        while ia < ca && ib < cb && ic < cc do
            let sa = get a ia
            let sb = get b ib
            let sc = get c ic
            let na = sa.id
            let nb = sb.id
            let nc = sc.id
            if na < nb || na < nc then ia <- ia + 1
            elif nb < na || nb < nc then ib <- ib + 1
            elif nc < na || nc < nb then ic <- ic + 1
            else
                let mask = sa.mask &&& sb.mask &&& sc.mask
                if mask <> 0UL then 
                    let sid = sa.id
                    let ir = find r sid
                    if ir < 0 then
                        let data = r.AddMask(sid, mask)
                        action param mask struct(data, sa.data, sb.data, sc.data)
                    else
                        let sr = r.[ir]
                        let createMask = mask &&& ~~~sr.mask
                        if createMask <> 0UL then
                            let data = r.AddMask(sid, createMask)
                            action param mask struct(data, sa.data, sb.data, sc.data)
                ia <- ia + 1
                ib <- ib + 1
                ic <- ic + 1

    /// Creates components when present in A and B and C and missing from R
    let create4 action struct(r, a, b, c, d) param =
        let ca = count a
        let cb = count b
        let cc = count c
        let cd = count d
        let mutable ia = 0
        let mutable ib = 0
        let mutable ic = 0
        let mutable id = 0
        while ia < ca && ib < cb && ic < cc && id < cd do
            let sa = get a ia
            let sb = get b ib
            let sc = get c ic
            let sd = get d id
            let na = sa.id
            let nb = sb.id
            let nc = sc.id
            let nd = sd.id
            if na < nb || na < nc || na < nd then ia <- ia + 1
            elif nb < na || nb < nc || nb < nd then ib <- ib + 1
            elif nc < na || nc < nb || nc < nd then ic <- ic + 1
            elif nd < na || nd < nb || nd < nc then id <- id + 1
            else
                let mask = sa.mask &&& sb.mask &&& sc.mask
                if mask <> 0UL then 
                    let sid = sa.id
                    let ir = find r sid
                    if ir < 0 then
                        let data = r.AddMask(sid, mask)
                        action param mask struct(data, sa.data, sb.data, sc.data, sd.data)
                    else
                        let sr = r.[ir]
                        let createMask = mask &&& ~~~sr.mask
                        if createMask <> 0UL then
                            let data = r.AddMask(sid, createMask)
                            action param mask struct(data, sa.data, sb.data, sc.data, sd.data)
                ia <- ia + 1
                ib <- ib + 1
                ic <- ic + 1
                id <- id + 1

    let fold1 action a initState param =
        let mutable state = initState
        let ca = count a
        for ia = 0 to ca - 1 do
            let sa = a.[ia]
            state <- action state param sa.mask sa.data
        state
            
    let fold2 action struct(a, b) initState param =
        let ca = count a
        let cb = count b
        let mutable ia = 0
        let mutable ib = 0
        let mutable state = initState
        while ia < ca && ib < cb do
            let sa = a.[ia]
            let sb = b.[ib]
            let na = sa.id
            let nb = sb.id
            if na < nb then ia <- ia + 1
            elif nb < na then ib <- ib + 1
            else
                let mask = sa.mask &&& sb.mask
                if mask <> 0UL then state <- action state param mask struct(sa.data, sb.data)
                ia <- ia + 1
                ib <- ib + 1
        state    

    let fold3 action struct(a, b, c) initState param =
        let ca = count a
        let cb = count b
        let cc = count c
        let mutable ia = 0
        let mutable ib = 0
        let mutable ic = 0
        let mutable state = initState
        while ia < ca && ib < cb && ic < cc do
            let sa = a.[ia]
            let sb = b.[ib]
            let sc = c.[ic]
            let na = sa.id
            let nb = sb.id
            let nc = sc.id
            if na < nb || na < nc then ia <- ia + 1
            elif nb < na || nb < nc then ib <- ib + 1
            elif nc < na || nc < nb then ic <- ic + 1
            else
                let mask = sa.mask &&& sb.mask &&& sc.mask
                if mask <> 0UL then state <- action state param mask struct(sa.data, sb.data, sc.data)
                ia <- ia + 1
                ib <- ib + 1
                ic <- ic + 1
        state

    let fold4 action struct(a, b, c, d) initState param =
        let ca = count a
        let cb = count b
        let cc = count c
        let cd = count d
        let mutable ia = 0
        let mutable ib = 0
        let mutable ic = 0
        let mutable id = 0
        let mutable state = initState
        while ia < ca && ib < cb && ic < cc && id < cd do
            let sa = get a ia
            let sb = get b ib
            let sc = get c ic
            let sd = get d id
            let na = sa.id
            let nb = sb.id
            let nc = sc.id
            let nd = sd.id
            if na < nb || na < nc || na < nd then ia <- ia + 1
            elif nb < na || nb < nc || nb < nd then ib <- ib + 1
            elif nc < na || nc < nb || nc < nd then ic <- ic + 1
            elif nd < na || nd < nb || nd < nc then id <- id + 1
            else
                let mask = sa.mask &&& sb.mask &&& sc.mask &&& sd.mask
                if mask <> 0UL then state <- action state param mask struct(sa.data, sb.data, sc.data, sd.data)
                ia <- ia + 1
                ib <- ib + 1
                ic <- ic + 1
                id <- id + 1
        state

module Segments =
    let get (c : ISegmentStore<_>) = c.Get()
    let get2 c = struct(get c, get c)
    let get3 c = struct(get c, get c, get c)
    let get4 c = struct(get c, get c, get c, get c)
    let get5 c = struct(get c, get c, get c, get c, get c)
        
    let join1 a c = get c |> Joins.iter1 a    
    let join2 a c = get2 c |> Joins.iter2 a
    let join3 a c = get3 c |> Joins.iter3 a    
    let join4 a c = get4 c |> Joins.iter4 a
    let join5 a c = get5 c |> Joins.iter5 a

    let create1 a c = get2 c |> Joins.create1 a
    let create2 a c = get3 c |> Joins.create2 a
    let create3 a c = get4 c |> Joins.create3 a
    let create4 a c = get5 c |> Joins.create4 a

    let fold1 a c = get c |> Joins.fold1 a
    let fold2 a c = get2 c |> Joins.fold2 a
    let fold3 a c = get3 c |> Joins.fold3 a
    let fold4 a c = get4 c |> Joins.fold4 a

let join1 a = a |> Array.iter1 |> Segments.join1
let join2 a = a |> Array.iter2 |> Segments.join2
let join3 a = a |> Array.iter3 |> Segments.join3
let join4 a = a |> Array.iter4 |> Segments.join4
let join5 a = a |> Array.iter5 |> Segments.join5

let create1 a = a |> Array.create1 |> Segments.create1
let create2 a = a |> Array.create2 |> Segments.create2        
let create3 a = a |> Array.create3 |> Segments.create3
let create4 a = a |> Array.create4 |> Segments.create4

let update1 a = a |> Array.update1 |> Segments.join1
let update2 a = a |> Array.update2 |> Segments.join2
let update3 a = a |> Array.update3 |> Segments.join3
let update4 a = a |> Array.update4 |> Segments.join4
let update5 a = a |> Array.update5 |> Segments.join5

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

let over c proc =
    proc c

let run param iter = iter param