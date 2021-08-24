namespace Garnet.Composition

open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open System.Threading

type IStreamSource =
    abstract TryOpen : string -> ValueOption<Stream>

type IReadOnlyFolder =
    inherit IStreamSource
    abstract GetFiles : string * string -> string seq
    abstract Contains : string -> bool
    abstract FlushChanged : Action<string> -> unit

type IFolder =
    inherit IReadOnlyFolder
    abstract OpenWrite : string -> Stream

module private ResourcePath =
    let getCanonical (path : string) =
        path.Replace('\\', '/').ToLowerInvariant()

    /// Result has front slashes
    let getRelativePath (dir : string) (file : string) =
        try
            let pathUri = Uri(getCanonical file)
            let dir = getCanonical dir
            let dirWithSlash = if dir.EndsWith("/") then dir else dir + "/"
            let dirUri = Uri(dirWithSlash);
            Uri.UnescapeDataString(dirUri.MakeRelativeUri(pathUri).ToString())
        with ex ->
            raise(Exception($"Could not get relative path in directory '%s{dir}' for file '%s{file}'", ex))

    let getExtensions (path : string) = seq {
        let file = Path.GetFileName(path)
        let mutable start = file.IndexOf('.')
        while start >= 0 do
            yield file.Substring(start)
            start <- file.IndexOf('.', start + 1)
        }

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
            //log <| sprintf "'%s' %A" path e.ChangeType
            changedSet.Invalidate(path, DateTime.UtcNow))
    let disposeWatcher = 
        if delay = Int32.MaxValue then ignore
        else
            Directory.CreateDirectory(rootDir) |> ignore
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
    new(rootDir) = new FileFolder(rootDir, Int32.MaxValue)
    new() = new FileFolder("")
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
    member c.TryOpen(file) =
        let path = getFullPath file
        if File.Exists(path) then ValueSome (openFile path 1 5 :> Stream)
        else ValueNone
    member c.OpenWrite(file) =
        let path = getFullPath file
        let dir = Path.GetDirectoryName path
        Directory.CreateDirectory dir |> ignore
        File.Open(path, FileMode.Create) :> Stream
    member c.FlushChanged(action : Action<string>) =
        let maxTimestamp = DateTime.UtcNow - TimeSpan.FromMilliseconds(float delay)
        changedSet.FlushChanged(maxTimestamp, action)
    member c.Dispose() =
        disposeWatcher()
    interface IFolder with
        member c.GetFiles(dir, searchPattern) = c.GetFiles(dir, searchPattern)
        member c.Contains(file) = c.Contains(file)
        member c.TryOpen(file) = c.TryOpen(file)
        member c.OpenWrite(file) = c.OpenWrite(file)
        member c.FlushChanged(action) = c.FlushChanged(action)
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()
    override c.ToString() =
        rootDir

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
            streams.Keys |> Seq.toArray :> seq<_>
    member c.Contains(key) =
        lock sync <| fun () ->
            streams.ContainsKey(key)
    member c.TryOpen(id) =
        lock sync <| fun () ->
            match streams.TryGetValue id with
            | true, x -> ValueSome x
            | false, _ -> ValueNone
        |> ValueOption.map (fun ms ->
            let length = int ms.Length
            let buffer = ms.GetBuffer()
            new MemoryStream(buffer, 0, length, false) :> Stream)
    member c.FlushChanged (action : Action<'k>) =
        let count = updated.Count
        for i = 0 to count - 1 do
            let key = updated.[i]
            action.Invoke(key)
        updated.RemoveRange(0, count)
    override c.ToString() =
        $"Streams (%d{streams.Count}):\n" + String.Join("\n",
            streams |> Seq.map (fun kvp ->
                $"%A{kvp.Key}: %d{kvp.Value.Length}"))

type MemoryFolder() =
    let lookup = MemoryStreamLookup<string>()
    interface IFolder with
        member c.GetFiles(_, _) =
            lookup.GetKeys()
        member c.Contains(file) =
            lookup.Contains(file)
        member c.TryOpen(file) =
            lookup.TryOpen(file)
        member c.OpenWrite(file) =
            lookup.OpenWrite(file)
        member c.FlushChanged _ = ()
    override c.ToString() =
        lookup.ToString()

[<AutoOpen>]
module FileFolder =
    type IStreamSource with
        member c.Open(key) =
            match c.TryOpen(key) with
            | ValueNone -> failwithf $"Could not open %s{key}"
            | ValueSome x -> x

    type IReadOnlyFolder with
        member c.GetFiles(dir) =
            c.GetFiles(dir, "*.*")

        member c.GetFiles() =
            c.GetFiles(".", "*.*")

type IResourceCache =
    abstract TryGetResource<'a> : string * [<Out>] value : byref<'a> -> bool
    abstract LoadResource<'a> : string -> 'a
    abstract AddResource<'a> : string * 'a -> unit

type IResourceLoader =
    abstract Load : IReadOnlyFolder * IResourceCache * string -> unit

type private ResourceTypeId() =
    static let mutable id  = 0
    static member GetNext() = Interlocked.Increment(&id)

type private ResourceTypeId<'a>() =
    static let mutable id = ResourceTypeId.GetNext()
    static member Id = id

type ResourceCache(folder : IReadOnlyFolder) =
    let mutable caches = Array.zeroCreate<obj>(8)
    let mutable folder = folder
    let disposables = List<IDisposable>()
    let loaders = Dictionary<string, IResourceLoader>()
    new() = new ResourceCache(MemoryFolder())
    member private c.GetResources<'a>() =
        let id = ResourceTypeId<'a>.Id
        if id >= caches.Length then
            Buffer.resizeArray (id + 1) &caches
        let cache = caches.[id]
        if Comparisons.isNotNull cache then cache :?> Dictionary<string, 'a>
        else            
            let cache = Dictionary<string, 'a>()
            caches.[id] <- cache :> obj
            cache
    member private c.TryLoad(key) =
        let extensions = ResourcePath.getExtensions key
        let mutable loaded = false
        let mutable e = extensions.GetEnumerator()
        while not loaded && e.MoveNext() do
            match loaders.TryGetValue(e.Current) with
            | true, loader ->
                loader.Load(folder, c, key)
                loaded <- true
            | false, _ -> ()
        loaded
    member c.SetFolder(newFolder) =
        folder <- newFolder
    member c.AddLoader(extension, typeLoader) =
        loaders.[extension] <- typeLoader
    member c.AddResource(key, resource) =
        let key = ResourcePath.getCanonical key
        c.GetResources().[key] <- resource
        match resource :> obj with
        | :? IDisposable as disposable -> disposables.Add(disposable)
        | _ -> ()
    member c.TryGetResource<'a>(key, [<Out>] value : byref<'a>) =
        let canonicalKey = ResourcePath.getCanonical key
        let cache = c.GetResources<'a>() 
        cache.TryGetValue(canonicalKey, &value)
    member c.LoadResource<'a> key =
        let canonicalKey = ResourcePath.getCanonical key
        let cache = c.GetResources<'a>() 
        match cache.TryGetValue(canonicalKey) with
        | true, resource -> resource
        | false, _ ->
            if not (c.TryLoad(canonicalKey)) then
                let str = String.Join("\n", loaders.Keys)
                failwith $"No loader for {key}, available:\n{str}" 
            match cache.TryGetValue(canonicalKey) with
            | true, resource -> resource
            | false, _ -> failwith $"{key} was not loaded"
    member c.LoadAll(path) =
        for file in folder.GetFiles(path) do
            c.TryLoad(file) |> ignore
    member c.Dispose() =
        for disposable in disposables do
           disposable.Dispose() 
    interface IResourceCache with
        member c.TryGetResource<'a>(key, [<Out>] value : byref<'a>) =
            c.TryGetResource<'a>(key, &value)
        member c.LoadResource<'a> key = c.LoadResource<'a>(key)
        member c.AddResource(key, resource) = c.AddResource(key, resource)
    interface IDisposable with
        member c.Dispose() = c.Dispose()
