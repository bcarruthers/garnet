namespace Garnet.Samples.Engine

open System.Collections.Generic
open System.Numerics
open Veldrid

type ParticleAnimation = {
    RandomSeed : uint64
    AnimationId : int
    Layer : SpriteLayerDescriptor
    Duration : float32
    Width : float32
    Height : float32
    SizeByEnergy : float32
    OpacityByEnergy : float32
    Textures : string[]
    MinTint : RgbaFloat
    MaxTint : RgbaFloat
    TintWeight : float32
    MinHue : float32
    MaxHue : float32
    MinSaturation : float32
    MaxSaturation : float32
    MinValue : float32
    MaxValue : float32
    Opacity : float32
    UseColor : bool
    StartColor : RgbaFloat
    EndColor : RgbaFloat
    MinSpeed : float32
    MaxSpeed : float32
    VelocityAngleRange : float32
    RotationAngleRange : float32
    MinCount : int
    MaxCount : int
    InitialDistance : float32
    InitialSize : float32
    InitialSizeRange : float32
    }

[<Struct>]
type ParticleEmission = {
    GroupId : int
    EmitDelay : float32
    EmitInterval : float32
    EmitCount : int
    Position : Vector2
    Velocity : Vector2
    Rotation : Vector2
    Color : RgbaFloat
    Energy : float32
    }

type ParticleGroup = {
    GroupId : int
    Animations : ParticleAnimation[]
    }

type ParticleAnimation with
    static member Defaults = {
        RandomSeed = 1UL
        AnimationId = 0
        Layer = Unchecked.defaultof<_>
        Duration = 1.0f
        Width = 1.0f
        Height = 1.0f
        SizeByEnergy = 0.0f
        OpacityByEnergy = -1.0f
        Textures = Array.empty
        MinHue = 0.0f
        MaxHue = 0.0f
        MinSaturation = 0.0f
        MaxSaturation = 0.0f
        MinValue = 1.0f
        MaxValue = 1.0f
        MinTint = RgbaFloat.White
        MaxTint = RgbaFloat.White
        UseColor = false
        StartColor = RgbaFloat.White
        EndColor = RgbaFloat.White
        MinSpeed = 0.0f
        MaxSpeed = 0.0f
        VelocityAngleRange = 360.0f
        RotationAngleRange = 360.0f
        MinCount = 1
        MaxCount = 1
        InitialDistance = 0.0f
        InitialSize = 1.0f
        InitialSizeRange = 0.0f
        TintWeight = 1.0f
        Opacity = 1.0f 
        }

type private ParticleBuffers() =
    [<DefaultValue>] val mutable timestamps : int64[]
    [<DefaultValue>] val mutable positions : Vector2[]
    [<DefaultValue>] val mutable velocities : Vector2[]
    [<DefaultValue>] val mutable sizes : float32[]
    [<DefaultValue>] val mutable rotations : Vector2[]
    [<DefaultValue>] val mutable energies : float32[]
    [<DefaultValue>] val mutable colors : RgbaFloat[]
    [<DefaultValue>] val mutable opacities : float32[]
    [<DefaultValue>] val mutable texIndices : int[]
    member c.Allocate(count) =
        Buffer.resizeArray count &c.timestamps 
        Buffer.resizeArray count &c.positions
        Buffer.resizeArray count &c.velocities
        Buffer.resizeArray count &c.sizes
        Buffer.resizeArray count &c.rotations
        Buffer.resizeArray count &c.energies
        Buffer.resizeArray count &c.colors
        Buffer.resizeArray count &c.opacities
        Buffer.resizeArray count &c.texIndices
    static member inline Copy(src : ParticleBuffers, srcIndex, dest : ParticleBuffers, destIndex) =
        dest.timestamps.[destIndex] <- src.timestamps.[srcIndex]
        dest.positions.[destIndex] <- src.positions.[srcIndex]
        dest.velocities.[destIndex] <- src.velocities.[srcIndex]
        dest.sizes.[destIndex] <- src.sizes.[srcIndex]
        dest.rotations.[destIndex] <- src.rotations.[srcIndex]
        dest.energies.[destIndex] <- src.energies.[srcIndex]
        dest.colors.[destIndex] <- src.colors.[srcIndex]
        dest.opacities.[destIndex] <- src.opacities.[srcIndex]
        dest.texIndices.[destIndex] <- src.texIndices.[srcIndex]

/// Cached for every kind of animation, not per emission
type ParticleSet(anim : ParticleAnimation) =
    let rand = PcgRandom(anim.RandomSeed, 1UL)
    let texBounds = Array.zeroCreate anim.Textures.Length
    let b = ParticleBuffers()
    let mutable time = 0L
    let mutable pc = 0
    let swapRemove (index : int byref) =
        pc <- pc - 1
        ParticleBuffers.Copy(b, pc, b, index)
        index <- index - 1        
    let pruneByTimestamp() =
        let mutable i = 0
        while i < pc do
            if time < b.timestamps.[i] then swapRemove &i
            i <- i + 1
    let updateEnergy energyUnitToTick =
        let energyRate = -energyUnitToTick
        let mutable i = 0
        while i < pc do
            let e = b.energies.[i] + energyRate
            if e > 0.0f
                then b.energies.[i] <- e
                else swapRemove &i
            i <- i + 1
    let addToScalars (arr : _[]) pc delta =
        if abs delta > 0.0f then
            for i = 0 to pc - 1 do
                arr.[i] <- arr.[i] + delta                
    let updatePositions deltaTime =
        for i = 0 to pc - 1 do
            b.positions.[i] <- b.positions.[i] + b.velocities.[i] * deltaTime
    let updateColors () =
        if anim.UseColor then
            let c0 = anim.StartColor
            let c1 = anim.EndColor
            for i = 0 to pc - 1 do
                b.colors.[i] <- RgbaFloat.lerp c0 c1 b.energies.[i]
    member c.ParticleCount = pc
    member c.Clear() =
        pc <- 0
        time <- 0L
    member s.Update(deltaTime : int64) =
        time <- time + deltaTime
        if pc > 0 then
            pruneByTimestamp()
            // Energy goes from 1.0 to 0.0 over duration
            let deltaSec = float32 deltaTime / 1000.0f
            let relativeDelta = deltaSec / anim.Duration 
            let opacityDelta = anim.OpacityByEnergy * relativeDelta
            let sizeDelta = anim.SizeByEnergy * relativeDelta
            updateEnergy relativeDelta
            addToScalars b.opacities pc opacityDelta
            addToScalars b.sizes pc sizeDelta
            updatePositions deltaSec
            updateColors ()
    member s.Emit(pe : ParticleEmission) =
        let emitCount = 
            let x0 = anim.MinCount * pe.EmitCount
            let x1 = anim.MaxCount * pe.EmitCount
            rand.NextInt32(x0, x1)
        let size = 
            let x0 = anim.InitialSize - anim.InitialSizeRange * 0.5f
            let x1 = anim.InitialSize + anim.InitialSizeRange * 0.5f
            rand.NextSingle(x0, x1)
        let minTint = anim.MinTint
        let maxTint = anim.MaxTint
        b.Allocate(pc + emitCount)
        for i = 0 to emitCount - 1 do
            let dir = rand.NextRotationDegrees(anim.VelocityAngleRange).Rotate(pe.Rotation)
            let index = pc + i
            b.timestamps.[index] <- time
            b.positions.[index] <- pe.Position + dir * anim.InitialDistance
            b.velocities.[index] <-
                let v = 
                    let speed = rand.NextSingle(anim.MinSpeed, anim.MaxSpeed)
                    dir * speed + pe.Velocity
                v
            b.sizes.[index] <- size
            b.rotations.[index] <- rand.NextRotationDegrees(anim.RotationAngleRange).Rotate(dir)
            b.energies.[index] <- pe.Energy
            b.colors.[index] <-
                let color =
                    let h = rand.NextSingle(anim.MinHue, anim.MaxHue)
                    let s = rand.NextSingle(anim.MinSaturation, anim.MaxSaturation)
                    let v = rand.NextSingle(anim.MinValue, anim.MaxValue)
                    let c = HsvaFloat(h, s, v, 1.0f).ToRgba()
                    let tint =
                        let c = RgbaFloat.lerp minTint maxTint (rand.NextSingle())
                        let tint = RgbaFloat.lerp RgbaFloat.White pe.Color anim.TintWeight
                        c.Multiply(tint)
                    c.Multiply(tint)
                color
            b.opacities.[index] <- anim.Opacity
            b.texIndices.[index] <- 0
        // Textures
        if anim.Textures.Length > 1 then
            for i = 0 to emitCount - 1 do
                b.texIndices.[pc + i] <- rand.NextInt32(anim.Textures.Length)
        pc <- pc + emitCount    
    member c.Draw(layers : SpriteRenderer, atlas : TextureAtlas) =
        // Update tex bounds lookup
        for i = 0 to texBounds.Length - 1 do
            texBounds.[i] <- atlas.[anim.Textures.[i]].NormalizedBounds
        // Draw sprites
        let w = layers.GetVertices<PositionTextureDualColorVertex>(anim.Layer)
        let bg = RgbaFloat.Clear
        let baseSize = Vector2(anim.Width, anim.Height) * 0.5f
        let span = w.GetSpriteSpan(pc)
        for i = 0 to pc - 1 do
            let p = b.positions.[i]
            let dir = b.rotations.[i]
            let scaling = b.sizes.[i]
            let fg =
                let opacity = b.opacities.[i]
                let color = b.colors.[i] 
                color.MultiplyAlpha(opacity)
            let side = -dir.GetPerpendicular()
            let s = baseSize * scaling
            let dy = dir * s.Y
            let dx = side * s.X
            let p0 = p - dx + dy
            let p1 = p - dx - dy
            let p2 = p + dx - dy
            let p3 = p + dx + dy
            let texIndex = b.texIndices.[i]
            let tb = texBounds.[texIndex]
            let t0 = tb.Min
            let t2 = tb.Max
            let span = span.Slice(i * 4)
            span.[0] <- {
                Position = Vector3(p0.X, p0.Y, 0.0f)
                TexCoord = Vector2(t0.X, t0.Y)
                Foreground = fg
                Background = bg
                }
            span.[1] <- {
                Position = Vector3(p1.X, p1.Y, 0.0f)
                TexCoord = Vector2(t2.X, t0.Y)
                Foreground = fg
                Background = bg
                }
            span.[2] <- {
                Position = Vector3(p2.X, p2.Y, 0.0f)
                TexCoord = Vector2(t2.X, t2.Y)
                Foreground = fg
                Background = bg
                }
            span.[3] <- {
                Position = Vector3(p3.X, p3.Y, 0.0f)
                TexCoord = Vector2(t0.X, t2.Y)
                Foreground = fg
                Background = bg
                }
        w.Advance(span.Length)        

type ParticleSystem() =
    let sets = List<ParticleSet>()
    let groups = List<ParticleSet[]>()
    let layerDescriptors = HashSet<SpriteLayerDescriptor>()
    member c.ParticleCount =
        let mutable count = 0
        for set in sets do
            count <- count + set.ParticleCount
        count
    member c.Clear() =
        for set in sets do
            set.Clear()
    member c.AddGroup(groupId, anims) =
        let group =
            anims
            |> Seq.map (fun anim ->
                let set = ParticleSet(anim)
                sets.Add(set)
                set)
            |> Seq.toArray
        while groups.Count <= groupId do
            groups.Add(Array.empty)
        groups.[groupId] <- group
        // Track the set of layers we're using
        for anim in anims do
            layerDescriptors.Add(anim.Layer) |> ignore
    member c.Emit(emission : ParticleEmission) =
        if emission.GroupId < groups.Count then
            let group = groups.[emission.GroupId]
            for set in group do
                set.Emit(emission)
    member c.Update(deltaTime) =
        for set in sets do
            set.Update(deltaTime)
    member c.Draw(canvas : SpriteRenderer, atlas) =
        for set in sets do
            set.Draw(canvas, atlas)
        // If not automatically flushing on draw, we need to manually do so here. This
        // assume layers are used exclusively for particles, otherwise they would overwrite
        // anything previously flushed to the same buffer
        for desc in layerDescriptors do
            match desc.FlushMode with
            | NoFlush -> canvas.GetVertices<PositionTextureDualColorVertex>(desc).Flush()
            | FlushOnDraw -> ()

type ParticleSystem with
    member c.AddGroups(groups) =
        for group  in groups do
            c.AddGroup(group.GroupId, group.Animations)
