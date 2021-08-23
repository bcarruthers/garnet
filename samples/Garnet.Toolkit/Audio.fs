namespace Garnet.Audio

open System
open System.Collections.Generic
open System.Numerics
open System.IO
open System.Runtime.InteropServices
open OpenTK.Audio.OpenAL
open OpenTK.Audio.OpenAL.Extensions
open Garnet.Resources
open Garnet.Collections

[<Struct>]
type SoundId = SoundId of int

[<Struct>]
type SoundDescriptor = {
    Channels : int
    BitsPerSample : int
    SampleRate : int
    SampleCount : int
    }

module SoundDescriptor =
    let getALFormat desc =
        if desc.Channels = 1 && desc.BitsPerSample = 8 then ALFormat.Mono8
        elif desc.Channels = 1 && desc.BitsPerSample = 16 then ALFormat.Mono16
        elif desc.Channels = 2 && desc.BitsPerSample = 8 then ALFormat.Stereo8
        elif desc.Channels = 2 && desc.BitsPerSample = 16 then ALFormat.Stereo16
        else failwith $"Unsupported audio format, {desc.Channels} channels, {desc.BitsPerSample} bits"

    let getDuration desc =
        desc.SampleCount * 1000 / desc.Channels / desc.SampleRate

[<AutoOpen>]
module internal OpenALInternal =
    module SoundId =
        let toInt id = match id with SoundId x -> x

    type AL =
        static member GetAvailableDeviceNames() = [|
            yield! ALC.GetStringList(GetEnumerationStringList.DeviceSpecifier)
            yield! Creative.EnumerateAll.EnumerateAll.GetStringList(Creative.EnumerateAll.GetEnumerateAllContextStringList.AllDevicesSpecifier)
            |]

        static member ThrowIfError(str) =
            let error = AL.GetError()
            if int error <> int ALError.NoError then
                failwith $"OpenAL error on {str}: {AL.GetErrorString(error)}"

    type internal SourcePool() =
        let maxSources = 32
        let sources = AL.GenSources(maxSources)
        let pool = 
            let stack = Stack<int>()
            for id in sources do 
                stack.Push(id)
            stack
        member c.TryGetSource() =
            if pool.Count > 0 then ValueSome (pool.Pop())
            else ValueNone
        member c.RecycleSource(id) =
            pool.Push(id)
        member c.Dispose() =
            AL.DeleteSources(sources)
        interface IDisposable with
            member c.Dispose() =
                c.Dispose()

    [<Struct>]
    type Sound = {
        descriptor : SoundDescriptor
        buffer : int
        }

type AudioDevice() =
    let devices = AL.GetAvailableDeviceNames()
    let device = ALC.OpenDevice(null)
    let context = 
        let c = ALC.CreateContext(device, Array.empty)
        let _ = ALC.MakeContextCurrent(c)
        c
    let sources = new SourcePool()
    let sounds = List<Sound>()
    let activeSources = PriorityQueue<int64, int>()
    let mutable time = 0L
    member c.CreateSound(desc, data : ReadOnlyMemory<byte>) =
        let buffers = AL.GenBuffers(1)
        use handle = data.Pin()
        let format = SoundDescriptor.getALFormat desc
        AL.BufferData(buffers.[0], format, IntPtr handle.Pointer, desc.SampleCount, desc.SampleRate)
        AL.ThrowIfError("loading audio data")
        let sound = {
            descriptor = desc
            buffer = buffers.[0]
            }
        let soundId = sounds.Count
        sounds.Add(sound)
        SoundId soundId
    member c.Update(currentTime) =
        time <- currentTime
        while activeSources.Count > 0 && currentTime >= activeSources.Top.Key do
            let sourceId = activeSources.Dequeue()
            sources.RecycleSource(sourceId)
    member c.PlaySound(soundId, looping) =
        match sources.TryGetSource() with
        | ValueNone -> ()
        | ValueSome sourceId ->
            let soundId = SoundId.toInt soundId
            let sound = sounds.[soundId]
            AL.Source(sourceId, ALSourcei.Buffer, sound.buffer)
            AL.Source(sourceId, ALSourceb.Looping, looping)
            //AL.Source(id, ALSource3f.Position, )
            AL.SourcePlay(sourceId)
            activeSources.Enqueue(time, sourceId)
    member c.SetPosition(pos : Vector3) =
        let mutable v = OpenTK.Mathematics.Vector3(pos.X, pos.Y, pos.Z)
        AL.Listener(ALListener3f.Position, &v)
    member c.SetGain(gain) =
        AL.Listener(ALListenerf.Gain, gain)
    member c.Dispose() =
        sources.Dispose()
        for sound in sounds do
            AL.DeleteBuffers([| sound.buffer |])            
        ALC.MakeContextCurrent(ALContext.Null) |> ignore
        ALC.DestroyContext(context)
        ALC.CloseDevice(device) |> ignore
    interface IDisposable with
        member c.Dispose() =
            c.Dispose()
    override c.ToString() =
        let version = AL.Get(ALGetString.Version)
        let vendor = AL.Get(ALGetString.Vendor)
        let renderer = AL.Get(ALGetString.Renderer)
        sprintf "Audio devices (%d):\n  %s\nSelected device:\n  %s\n  %s\n  %s" 
            devices.Length (String.Join("\n  ", devices)) 
            renderer version vendor

type AudioDevice with
    member c.CreateSoundFromSineWave() =
        let sampleRate = 44100
        let dt = 2.0 * Math.PI / float sampleRate
        let amp = 0.5
        let freq = 440.0
        let sampleCount = float sampleRate / freq
        let data = Array.zeroCreate<byte> (int sampleCount * 2)
        let dest = MemoryMarshal.Cast<byte, int16>(data.AsSpan())
        for i = 0 to dest.Length - 1 do
            dest.[i] <- int16 (amp * float Int16.MaxValue * sin (float i * dt * freq))
        let desc = {
            Channels = 1
            BitsPerSample = 16
            SampleRate = sampleRate
            SampleCount = data.Length * 8 / 16
            }
        c.CreateSound(desc, ReadOnlyMemory(data))

[<AutoOpen>]
module AudioLoaderExtensions =
    type IStreamSource with
        member c.LoadWave(device : AudioDevice, key) =
            use stream = c.Open(key)
            // https://stackoverflow.com/questions/8754111/how-to-read-the-data-in-a-wav-file-to-an-array
            use reader = new BinaryReader(stream)
            // chunk 0
            let _           = reader.ReadInt32() // chunkId
            let _           = reader.ReadInt32() // fileSize
            let _           = reader.ReadInt32() // riffType
            // chunk 1
            let _           = reader.ReadInt32() // fmtID
            let fmtSize     = reader.ReadInt32() // bytes for this chunk (expect 16 or 18)
            // 16 bytes coming
            let _           = int (reader.ReadInt16()) // fmtCode
            let channels    = int (reader.ReadInt16())
            let sampleRate  = reader.ReadInt32()
            let _           = reader.ReadInt32() // byteRate
            let _           = int (reader.ReadInt16()) // fmtBlockAlign
            let bitDepth    = int (reader.ReadInt16())
            if fmtSize = 18 then
                // Read any extra values
                let fmtExtraSize = int (reader.ReadInt16())
                stream.Seek(int64 fmtExtraSize, SeekOrigin.Current) |> ignore
            // chunk 2
            let _ = reader.ReadInt32() // dataId
            let length = reader.ReadInt32()
            // Read data
            // https://stackoverflow.com/questions/10996917/openal-albufferdata-returns-al-invalid-value-even-though-input-variables-look
            let multipleOf4Length = (length + 3) / 4 * 4
            let data = Array.zeroCreate multipleOf4Length
            stream.Read(data, 0, length) |> ignore
            // Create descriptor
            let bytesForSample = bitDepth / 8
            let sampleCount = multipleOf4Length / bytesForSample
            let desc = {
                Channels = channels
                BitsPerSample = bitDepth
                SampleRate = sampleRate
                SampleCount = sampleCount
                }
            device.CreateSound(desc, ReadOnlyMemory(data))

type AudioCache() =
    let sounds = Dictionary<string, SoundId>()
    member c.Item with get name = sounds.[name]
    member c.Add(name, soundId) =
        sounds.Add(name, soundId)

type AudioCache with
    member c.Load(device, fs : IReadOnlyFolder, path) =
        for file in fs.GetFiles(path) do
            let soundId = fs.LoadWave(device, file)
            c.Add(file, soundId)
    
