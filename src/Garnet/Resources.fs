namespace Garnet.Resources

open System
open System.Collections.Generic
open System.IO
open System.Threading

type IStreamSource =
    abstract GetFiles : unit -> string seq
    abstract OpenRead : string -> Stream
    abstract OpenWrite : string -> Stream
    abstract FlushChanged : Action<string> -> unit

module private ResourcePath =
    let getCanonical (path : string) =
        path.Replace('\\', '/').ToLowerInvariant()

    /// Result has frontslashes
    let getRelativePath (dir : string) (file : string) =
        try
            let pathUri = Uri(getCanonical file)
            let dir = getCanonical dir
            let dirWithSlash = if dir.EndsWith("/") then dir else dir + "/"
            let dirUri = Uri(dirWithSlash);
            Uri.UnescapeDataString(dirUri.MakeRelativeUri(pathUri).ToString())
        with ex ->
            raise(
                Exception(
                    sprintf "Could not get relative path in directory '%s' for file '%s'" dir file,
                    ex))

/// Thread-safe
type private ResourceInvalidationSet() =
    let changedSet = Dictionary<string, DateTime>()
    let flushed = List<_>()
    let sync = obj()
    member c.Invalidate(key, timestamp) =
        Monitor.Enter sync
        changedSet.[key] <- timestamp
        Monitor.Exit sync
    member c.FlushChanged(maxTimestamp, action : Action<string>) =
        Monitor.Enter sync
        for kvp in changedSet do
            if kvp.Value < maxTimestamp then
                flushed.Add kvp.Key
        for key in flushed do
            action.Invoke key
            changedSet.Remove key |> ignore
        flushed.Clear()
        Monitor.Exit sync

/// Thread-safe
type FileStreamSource(rootDir, delay) =
    let rootDir = Path.GetFullPath rootDir
    let changedSet = ResourceInvalidationSet()
    let handler =
        FileSystemEventHandler(fun s e ->              
            let path = ResourcePath.getRelativePath rootDir e.FullPath
            printfn "'%s' %A" path e.ChangeType
            changedSet.Invalidate(path, DateTime.UtcNow))
    let disposeWatcher = 
        if delay = Int32.MaxValue then ignore
        else
            if not (Directory.Exists rootDir) then
                failwithf "Could not find directory '%s'" rootDir
            let watcher = 
                new FileSystemWatcher(
                    Path = rootDir,
                    NotifyFilter = NotifyFilters.LastWrite,
                    Filter = "*.*",
                    IncludeSubdirectories = true,
                    EnableRaisingEvents = true)
            watcher.add_Changed handler
            watcher.add_Created handler
            watcher.Dispose
    let rec openFile path (delay : int) retryCount =
        try
            File.Open(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        with ex ->
            if retryCount = 0 then raise ex
            else 
                Thread.Sleep delay
                openFile path (delay * 2) (retryCount - 1)
    let getFullPath file =
        if Path.IsPathRooted(file) then file
        else Path.Combine(rootDir, file)
    new(rootDir) = new FileStreamSource(rootDir, 50)
    new() = new FileStreamSource("")
    member c.GetFiles() =
        if Directory.Exists rootDir 
        then 
            Directory.EnumerateFiles(rootDir, "*.*")
            |> Seq.map (fun file -> 
                let fullPath = Path.GetFullPath file
                ResourcePath.getRelativePath rootDir fullPath)
        else Seq.empty
    member c.OpenRead file =
        let path = getFullPath file
        openFile path 1 5 :> Stream
    member c.OpenWrite file =
        let path = getFullPath file
        let dir = Path.GetDirectoryName path
        Directory.CreateDirectory dir |> ignore
        File.OpenWrite path :> Stream
    member c.FlushChanged (action : Action<string>) =
        let maxTimestamp = DateTime.UtcNow - TimeSpan.FromMilliseconds(float delay)
        changedSet.FlushChanged(maxTimestamp, action)
    member c.Dispose() =
        disposeWatcher()
    interface IStreamSource with
        member c.GetFiles() = c.GetFiles()
        member c.OpenRead file = c.OpenRead file
        member c.OpenWrite file = c.OpenWrite file
        member c.FlushChanged action = c.FlushChanged action
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()

/// Note Dispose() is absent
type private NonDisposingStream(stream : Stream, onClose) =
    inherit Stream()
    override c.Position
        with get() = stream.Position
        and set value = stream.Position <- value
    override c.CanRead = stream.CanRead
    override c.CanWrite = stream.CanWrite
    override c.CanSeek = stream.CanSeek
    override c.Length = stream.Length
    override c.Write(input, offset, count) =
        stream.Write(input, offset, count)
    override c.Read(output, offset, count) =
        stream.Read(output, offset, count)
    override c.Flush() = stream.Flush()
    override c.Seek(offset, origin) =
        stream.Seek(offset, origin)
    override c.SetLength(length) =
        stream.SetLength(length)
    override c.Close() =
        onClose()

/// Stream lookup is thread-safe, but reading/writing is not
type MemoryStreamLookup<'k when 'k : equality>() =
    let updated = List<'k>()
    let logs = Dictionary<'k, MemoryStream>()
    let sync = obj()
    member private c.Open (id : 'k) =
        lock sync <| fun () ->
            match logs.TryGetValue id with
            | true, x -> x
            | false, _ ->            
                raise (FileNotFoundException(sprintf "Could not find %A" id, id.ToString()))
    member c.OpenWrite (id : 'k) =
        new NonDisposingStream(c.Open id, fun () -> updated.Add id) :> Stream
    member c.GetKeys() =
        lock sync <| fun () ->
            logs.Keys |> Seq.cache
    member c.OpenRead (id : 'k) =
        let ms = c.Open id
        let length = int ms.Length
        let buffer = ms.GetBuffer()
        new MemoryStream(buffer, 0, length, false) :> Stream 
    member c.FlushChanged (action : Action<'k>) =
        let count = updated.Count
        for i = 0 to count - 1 do
            let key = updated.[i]
            action.Invoke(key)
        updated.RemoveRange(0, count)
    override c.ToString() =
        String.Join("\n",
            logs |> Seq.map (fun kvp ->
                sprintf "%A: %d" kvp.Key kvp.Value.Length))

type MemoryStreamSource() =
    let lookup = MemoryStreamLookup<string>()
    interface IStreamSource with
        member c.GetFiles() =
            lookup.GetKeys()
        member c.OpenRead file =
            lookup.OpenRead file
        member c.OpenWrite file =
            lookup.OpenWrite file
        member c.FlushChanged (action : Action<string>) =
            lookup.FlushChanged action

type IResourceLoader<'a> =
    abstract Load : Stream -> 'a
    abstract Dispose : 'a -> unit

type ResourceLoader() =
    let loaders = Dictionary<struct(string * Type), obj>()
    member private c.GetKey<'a> format =
        struct(format, typeof<'a>)
    member c.Register<'a>(format, loader : IResourceLoader<'a>) =
        let key = c.GetKey<'a> format
        loaders.[key] <- loader
    member c.GetLoader<'a>(format : string) =
        let key = c.GetKey<'a> format
        match loaders.TryGetValue key with
        | true, load -> load :?> IResourceLoader<'a>
        | false, _ -> 
            failwithf "No %s loader for '%s', available:\n%s" 
                (typeof<'a>.Name) format
                (String.Join("\n", loaders.Keys |> Seq.map (fun struct(format, t) ->
                    sprintf "%s %s" format t.Name)))

type ResourceLoader<'a>(load) =
    interface IResourceLoader<'a> with
        member c.Load stream = load stream
        member c.Dispose resource = ()

type DisposableLoader<'a when 'a :> IDisposable>(load) =
    interface IResourceLoader<'a> with
        member c.Load stream = load stream
        member c.Dispose resource = resource.Dispose()

type IResource =
    inherit IDisposable
    abstract member Invalidate : unit -> unit
    abstract member Subscribe : (unit -> unit) -> IDisposable

type IResource<'a> =
    inherit IResource
    abstract Load : unit -> 'a

type private Subscription<'a>(subs : List<'a>, item : 'a) =
    interface IDisposable with
        member c.Dispose() =
            subs.Remove item |> ignore

type Resource<'a>(load, dispose) =
    let mutable loaded : 'a voption = ValueNone
    let subs = List<unit -> unit>()
    let tempSubs = List<unit -> unit>()
    /// Immediately calls action and subscribes to further changes.
    member c.Subscribe action =
        subs.Add action
        new Subscription<_>(subs, action) :> IDisposable      
    member c.Invalidate() =
        c.Dispose()
        // Copy list to avoid problems if modified during invalidation.
        for sub in subs do
            tempSubs.Add sub
        for sub in tempSubs do
            sub()
        tempSubs.Clear()
    member c.Dispose() =
        match loaded with
        | ValueSome x -> 
            dispose x
            loaded <- ValueNone
        | ValueNone -> ()            
    member c.Load() =
        match loaded with
        | ValueSome x -> x
        | ValueNone ->
            let x = load()
            loaded <- ValueSome x
            x
    interface IResource<'a> with
        member c.Load() =
            c.Load()
        member c.Subscribe action =
            c.Subscribe action
        member c.Invalidate() =
            c.Invalidate()
        member c.Dispose() =
            c.Dispose()
    override c.ToString() =
        sprintf "%s %A" typeof<'a>.Name loaded

type IResourceSet =
    abstract GetResource<'a> : string -> 'a

type ResourceSet() =
    let loader = ResourceLoader()
    let resources = Dictionary<string, IResource>()
    let sources = List<IStreamSource>()
    let invalidate = 
        Action<_>(fun key ->
            let key = ResourcePath.getCanonical key
            match resources.TryGetValue key with
            | true, resource -> resource.Invalidate()
            | false, _ -> ())//printfn "%s not found" key)
    member private c.Open key =
        let mutable result = ValueNone
        let mutable i = 0
        while result = ValueNone && i < sources.Count do
            result <- ValueSome (sources.[i].OpenRead key)
            i <- i + 1
        match result with
        | ValueNone -> failwithf "Could not open %s" key
        | ValueSome x -> x
    member c.AddSource source =
        sources.Add source
    member c.Register(format : string, typeLoader) =
        loader.Register(format.ToLowerInvariant(), typeLoader)
    member c.AddResource<'a>(key, resource) =
        let key = ResourcePath.getCanonical key
        // dispose existing
        match resources.TryGetValue key with
        | true, resource -> resource.Dispose()
        | false, _ -> ()
        // replace with new
        resources.[key] <- resource
    member c.GetResource<'a> key =
        let key = ResourcePath.getCanonical key
        match resources.TryGetValue key with
        | true, resource -> resource :?> Resource<'a>
        | false, _ ->
            let format = Path.GetExtension(key)
            let load = 
                fun _ -> 
                    let loader = loader.GetLoader<'a>(format)
                    use stream = c.Open(key)
                    loader.Load stream
            let dispose =
                fun resource ->
                    let loader = loader.GetLoader<'a>(format)
                    loader.Dispose resource
            let resource = new Resource<'a>(load, dispose)
            resources.Add(key, resource)
            resource
    member c.LoadResource<'a> key =
        c.GetResource<'a>(key).Load()
    member c.InvalidateChanged() =
        for source in sources do
            source.FlushChanged invalidate
    member c.Dispose() =
        for resource in resources.Values do
            resource.Dispose()
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()

[<AutoOpen>]
module private Resource =
    let load (r : Resource<_>) = r.Load()
    let subscribe (r : Resource<_>) action = r.Subscribe action

type Resource =
    static member Value<'a>(value) =
        new Resource<'a>((fun () -> value), ignore)

    static member Create<'a>(load) =
        new Resource<'a>(load, ignore)

    static member Create<'a>(load, dispose) =
        new Resource<'a>(load, dispose)

    static member DeriveFrom<'a, 'b>(r1, map : 'b -> 'a, dispose) =
        let subs = List<IDisposable>()
        let invalidate = ref ignore
        let r = 
            new Resource<'a>(
                (fun r -> 
                    subs.Add(subscribe r1 invalidate.Value)
                    map (load r1)),
                (fun (x : 'a) -> 
                    dispose x
                    for sub in subs do sub.Dispose()))
        invalidate.Value <- r.Invalidate
        r

    static member DeriveFrom<'a, 'b, 'c>(r1, r2, map : 'b -> 'c -> 'a, dispose) =
        let subs = List<IDisposable>()
        let invalidate = ref ignore
        let r = 
            new Resource<_>(
                (fun r -> 
                    subs.Add(subscribe r1 invalidate.Value)
                    subs.Add(subscribe r2 invalidate.Value)
                    map (load r1) (load r2)),
                (fun (x : 'a) -> 
                    dispose x
                    for sub in subs do sub.Dispose()))
        invalidate.Value <- r.Invalidate
        r

    static member DeriveFrom<'a, 'b, 'c, 'd>(r1, r2, r3, map : 'b -> 'c -> 'd -> 'a, dispose) =
        let subs = List<IDisposable>()
        let invalidate = ref ignore
        let r = 
            new Resource<_>(
                (fun r -> 
                    subs.Add(subscribe r1 invalidate.Value)
                    subs.Add(subscribe r2 invalidate.Value)
                    subs.Add(subscribe r3 invalidate.Value)
                    map (load r1) (load r2) (load r3)),
                (fun (x : 'a) -> 
                    dispose x
                    for sub in subs do sub.Dispose()))
        invalidate.Value <- r.Invalidate
        r

    static member DeriveDisposableFrom<'a, 'b when 'a :> IDisposable>(r1, map : 'b -> 'a) =
        Resource.DeriveFrom(r1, map, fun x -> x.Dispose())

    static member DeriveDisposableFrom<'a, 'b, 'c when 'a :> IDisposable>(r1, r2, map : 'b -> 'c -> 'a) =
        Resource.DeriveFrom(r1, r2, map, fun x -> x.Dispose())

    static member DeriveDisposableFrom<'a, 'b, 'c, 'd when 'a :> IDisposable>(r1, r2, r3, map : 'b -> 'c -> 'd -> 'a) =
        Resource.DeriveFrom(r1, r2, r3, map, fun x -> x.Dispose())

type Resource<'a> with
    member c.Derive(map, dispose) = Resource.DeriveFrom(c, map, dispose)
    member c.Derive(a, map, dispose) = Resource.DeriveFrom(c, a, map, dispose)
    member c.Derive(a, b, map, dispose) = Resource.DeriveFrom(c, a, b, map, dispose)
    member c.DeriveDisposable(map) = Resource.DeriveDisposableFrom(c, map)
    member c.DeriveDisposable(a, map) = Resource.DeriveDisposableFrom(c, a, map)
    member c.DeriveDisposable(a, b, map) = Resource.DeriveDisposableFrom(c, a, b, map)
