namespace Garnet.Samples.Engine

open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open OpenTK.Audio.OpenAL
open OpenTK.Audio.OpenAL.Extensions

[<Struct>]
type SoundDescriptor = {
    channels : int
    bitsPerSample : int
    sampleRate : int
    sampleCount : int
    }

module SoundDescriptor =
    let getALFormat desc =
        if desc.channels = 1 && desc.bitsPerSample = 8 then ALFormat.Mono8
        elif desc.channels = 1 && desc.bitsPerSample = 16 then ALFormat.Mono16
        elif desc.channels = 2 && desc.bitsPerSample = 8 then ALFormat.Stereo8
        elif desc.channels = 2 && desc.bitsPerSample = 16 then ALFormat.Stereo16
        else failwith $"Unsupported audio format, {desc.channels} channels, {desc.bitsPerSample} bits"

    let getDuration desc =
        desc.sampleCount * 1000 / desc.channels / desc.sampleRate

[<Struct>]
type UpdateSoundListener = {
    listenerPos : Vector3
    masterGain : float32
    }

[<Struct>]
type PlaySound = {
    soundName : string
    soundPos : Vector3
    soundGain : float32
    loopCount : int
    }

[<Struct>]
type SoundId = SoundId of int

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
        let result = ALC.MakeContextCurrent(c)
        c
    let sources = new SourcePool()
    let sounds = List<Sound>()
    let activeSources = PriorityQueue<int64, int>()
    let mutable time = 0L
    member c.CreateSound(desc, data : ReadOnlyMemory<byte>) =
        let buffers = AL.GenBuffers(1)
        use handle = data.Pin()
        let format = SoundDescriptor.getALFormat desc
        AL.BufferData(buffers.[0], format, IntPtr handle.Pointer, desc.sampleCount, desc.sampleRate)
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
            channels = 1
            bitsPerSample = 16
            sampleRate = sampleRate
            sampleCount = data.Length * 8 / 16
            }
        c.CreateSound(desc, ReadOnlyMemory(data))
    