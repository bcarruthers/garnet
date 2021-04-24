namespace Garnet.Resources

open System
open System.Diagnostics
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text
open System.Threading

type IFolder =
    inherit IDisposable
    abstract GetFiles : unit -> string seq
    abstract Contains : string -> bool
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
type FileFolder(rootDir, delay) =
    let rootDir = 
        let path = if String.IsNullOrEmpty(rootDir) then Directory.GetCurrentDirectory() else rootDir
        Path.GetFullPath(path)
    let changedSet = ResourceInvalidationSet()
    let handler =
        FileSystemEventHandler(fun s e ->              
            let path = ResourcePath.getRelativePath rootDir e.FullPath
            printfn "'%s' %A" path e.ChangeType
            changedSet.Invalidate(path, DateTime.UtcNow))
    let disposeWatcher = 
        if delay = Int32.MaxValue then ignore
        else
            Directory.CreateDirectory(rootDir) |> ignore
            //if not (Directory.Exists rootDir) then
            //    failwithf "Could not find directory '%s'" rootDir
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
    let getFullPath (file : string) =
        if Path.IsPathRooted(file) then file
        else Path.Combine(rootDir, file)
    new(rootDir) = new FileFolder(rootDir, 50)
    new() = new FileFolder("")
    member c.GetFiles() =
        c.GetFiles(".", "*.*")
    member c.GetFiles(dir, searchPattern) =
        let subDir = Path.Combine(rootDir, dir)
        if Directory.Exists subDir 
        then 
            Directory.EnumerateFiles(subDir, searchPattern)
            |> Seq.map (fun file -> 
                let fullPath = Path.GetFullPath file
                ResourcePath.getRelativePath rootDir fullPath)
        else Seq.empty
    member c.Contains(file) = 
        let path = getFullPath file
        File.Exists(path)
    member c.OpenRead(file) =
        let path = getFullPath file
        openFile path 1 5 :> Stream
    member c.OpenWrite(file) =
        let path = getFullPath file
        let dir = Path.GetDirectoryName path
        Directory.CreateDirectory dir |> ignore
        File.OpenWrite(path) :> Stream
    member c.FlushChanged(action : Action<string>) =
        let maxTimestamp = DateTime.UtcNow - TimeSpan.FromMilliseconds(float delay)
        changedSet.FlushChanged(maxTimestamp, action)
    member c.Dispose() =
        disposeWatcher()
    interface IFolder with
        member c.GetFiles() = c.GetFiles()
        member c.Contains(file) = c.Contains(file)
        member c.OpenRead(file) = c.OpenRead(file)
        member c.OpenWrite(file) = c.OpenWrite(file)
        member c.FlushChanged(action) = c.FlushChanged(action)
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()

/// Note Dispose() is absent
type NonDisposingStream(stream : Stream, onClose) =
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
    let streams = Dictionary<'k, MemoryStream>()
    let sync = obj()
    member c.OpenWrite (id : 'k) =
        let ms = 
            lock sync <| fun () ->
                match streams.TryGetValue id with
                | true, x -> x
                | false, _ -> 
                    let ms = new MemoryStream()
                    streams.Add(id, ms)
                    ms
        new NonDisposingStream(ms, fun () -> updated.Add id) :> Stream
    member c.GetKeys() =
        lock sync <| fun () ->
            streams.Keys |> Seq.cache
    member c.Contains(key) =
        lock sync <| fun () ->
            streams.ContainsKey(key)
    member c.TryOpenRead(id) =
        lock sync <| fun () ->
            match streams.TryGetValue id with
            | true, x -> ValueSome x
            | false, _ -> ValueNone
        |> ValueOption.map (fun ms ->
            let length = int ms.Length
            let buffer = ms.GetBuffer()
            new MemoryStream(buffer, 0, length, false) :> Stream)
    member c.OpenRead(id : 'k) =
        match c.TryOpenRead(id) with
        | ValueSome x -> x
        | ValueNone ->
            raise (FileNotFoundException(sprintf "Could not find %A" id, id.ToString()))
    member c.FlushChanged (action : Action<'k>) =
        let count = updated.Count
        for i = 0 to count - 1 do
            let key = updated.[i]
            action.Invoke(key)
        updated.RemoveRange(0, count)
    override c.ToString() =
        String.Join("\n",
            streams |> Seq.map (fun kvp ->
                sprintf "%A: %d" kvp.Key kvp.Value.Length))

type MemoryFolder() =
    let lookup = MemoryStreamLookup<string>()
    interface IFolder with
        member c.GetFiles() =
            lookup.GetKeys()
        member c.Contains(file) =
            lookup.Contains(file)
        member c.OpenRead(file) =
            lookup.OpenRead(file)
        member c.OpenWrite(file) =
            lookup.OpenWrite(file)
        member c.FlushChanged(action : Action<string>) =
            lookup.FlushChanged(action)
        member c.Dispose() =
            ()
    override c.ToString() =
        lookup.ToString()

type GZipStreamSource(streams : IFolder) =
    interface IFolder with
        member c.GetFiles() =
            streams.GetFiles()
        member c.Contains(file) =
            streams.Contains(file)
        member c.OpenRead(file) =
            new GZipStream(streams.OpenRead file, CompressionMode.Decompress, false)
            :> Stream
        member c.OpenWrite(file) =
            new GZipStream(streams.OpenWrite file, CompressionLevel.Optimal, false)
            :> Stream
        member c.FlushChanged(action : Action<string>) =
            streams.FlushChanged(action)
        member c.Dispose() =
            streams.Dispose()
    override c.ToString() =
        streams.ToString()

type IStreamSource =
    abstract TryOpen : string -> ValueOption<Stream>

type private Disposable(onDispose) =
    interface IDisposable with
        member c.Dispose() =
            onDispose()

type FolderCollection() =
    let sources = List<IFolder>()
    member c.AddSource(source) =
        sources.Add(source)
        new Disposable(fun () -> sources.Remove(source) |> ignore) :> IDisposable
    member c.InvalidateChanged(invalidate) =
        for source in sources do
            source.FlushChanged(invalidate)
    interface IStreamSource with
        member c.TryOpen(key) =
            let mutable result = ValueNone
            let mutable i = 0
            while result = ValueNone && i < sources.Count do
                if sources.[i].Contains(key) then
                    result <- ValueSome (sources.[i].OpenRead(key))
                i <- i + 1
            result

[<AutoOpen>]
module FileFolder =
    type IStreamSource with
        member c.Open(key) =
            match c.TryOpen(key) with
            | ValueNone -> failwithf "Could not open %s" key
            | ValueSome x -> x

type IResourceLoader<'a> =
    abstract Load : string * IStreamSource -> 'a
    abstract Dispose : 'a -> unit

type ResourceLoader() =
    let loaders = Dictionary<struct(string * Type), obj>()
    member private c.GetKey<'a> format =
        struct(format, typeof<'a>)
    member private c.Unregister<'a>(format) =
        let key = c.GetKey<'a> format
        loaders.Remove(key) |> ignore
    member c.Register<'a>(format, loader : IResourceLoader<'a>) =
        let key = c.GetKey<'a> format
        loaders.[key] <- loader
        new Disposable(fun () -> c.Unregister<'a>(format)) :> IDisposable
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
        member c.Load(key, source) = 
            use stream = source.Open(key)
            load stream
        member c.Dispose resource = ()

type DisposableLoader<'a when 'a :> IDisposable>(load) =
    interface IResourceLoader<'a> with
        member c.Load(key, source) = 
            use stream = source.Open(key)
            load stream
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

type Resource<'a>(load, dispose, name) =
    let mutable loadDuration = 0L
    let mutable loaded : 'a voption = ValueNone
    let subs = List<unit -> unit>()
    let tempSubs = List<unit -> unit>()
    new(load, dispose) =
        new Resource<'a>(load, dispose, "")
    /// Immediately calls action and subscribes to further changes.
    member c.Subscribe(action) =
        subs.Add(action)
        new Subscription<_>(subs, action) :> IDisposable      
    member c.Invalidate() =
        c.Dispose()
        // Copy list to avoid problems if modified during invalidation.
        for sub in subs do
            tempSubs.Add(sub)
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
            let start = Stopwatch.GetTimestamp()
            let x = load()
            let stop = Stopwatch.GetTimestamp()
            loadDuration <- stop - start
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
        let duration = loadDuration * 1000L / Stopwatch.Frequency
        let loadStr = if loaded.IsSome then sprintf "loaded in %A ms" duration else "unloaded"
        sprintf "%s (%s) %s" name typeof<'a>.Name loadStr

type IResourceSet =
    abstract GetResource<'a> : string -> 'a

type ResourceSet() =
    let loader = ResourceLoader()
    let resources = Dictionary<string, IResource>()
    let streamSource = FolderCollection()
    let invalidate = 
        Action<_>(fun key ->
            let key = ResourcePath.getCanonical key
            match resources.TryGetValue key with
            | true, resource -> resource.Invalidate()
            | false, _ -> ())//printfn "%s not found" key)
    member c.AddSource(source) =
        streamSource.AddSource(source)
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
    member c.GetResource<'a>(originalKey) =
        let key = ResourcePath.getCanonical originalKey
        match resources.TryGetValue(key) with
        | true, resource -> resource :?> Resource<'a>
        | false, _ ->
            let format = Path.GetExtension(key)
            let load = 
                fun _ -> 
                    let loader = loader.GetLoader<'a>(format)
                    loader.Load(key, streamSource)
            let dispose =
                fun resource ->
                    let loader = loader.GetLoader<'a>(format)
                    loader.Dispose(resource)
            let resource = new Resource<'a>(load, dispose, originalKey)
            resources.Add(key, resource)
            resource
    member c.LoadResource<'a> key =
        c.GetResource<'a>(key).Load()
    member c.InvalidateChanged() =
        streamSource.InvalidateChanged(invalidate)
    member c.Dispose() =
        for resource in resources.Values do
            resource.Dispose()
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()
    override c.ToString() =
        let sb = StringBuilder()
        sb.Append(sprintf "Resources (%d):" resources.Count) |> ignore
        for r in resources.Values do
            sb.AppendLine().Append("  " + r.ToString()) |> ignore
        sb.ToString()
            
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

    static member CreateDisposable<'a when 'a :> IDisposable>(create : unit -> 'a) =
        new Resource<'a>(create, fun x -> x.Dispose())

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
