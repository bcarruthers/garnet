# Samples

## Flocking

[[Code](https://github.com/bcarruthers/garnet/blob/master/samples/Garnet.Samples.Common/Flocking.fs)]

This sample demonstrates classic boids-style flocking or steering, with behaviors tuned so agents form clusters. Neighbor forces are calculated in brute-force fashion without use of spatial structures. The code is organized to minimize direct dependency on ECS and MonoGame.

## Strategy

[[Code](https://github.com/bcarruthers/garnet/blob/master/samples/Garnet.Samples.Common/Strategy.fs)]

Strategy games like Civilization have a world map consisting of grid cells. This sample demonstrates use of 2D location as a custom entity key for storing components in a world grid.

Of course, you could also forego ECS entirely and simply use an array of cells for your map, implementing your own logic for staged changes or sparse storage if needed. The benefit of using ECS is having those features available in a uniform way, especially for larger maps with sparse components.

### World grid storage

First, we define a 2D location type and use it to identify both cells and segments. The underlying component storage is based on 64-element segments (which can also be called pages or chunks), so we can define a mapping from cells to 8x8 cell segments. Note that unlike entity IDs, locations don't need to be created or recycled.

```fsharp
[<Struct>]
type Loc = { x : int; y : int }
    with 
        override c.ToString() = 
            sprintf "(%d, %d)" c.x c.y

module Loc =
    // Components are stored in size 64 segments, so
    // we need to define a mapping from component keys
    // to tuple of (segment key, index within segment)
    let toKeyPair p = 
        struct(
            { x = p.x >>> 3; y = p.y >>> 3 }, 
            ((p.y &&& 7) <<< 3) ||| (p.x &&& 7))

type WorldGrid() = 
    let store = ComponentStore<Loc, Loc>(Loc.toKeyPair)
```

You can iterate over world cells similarly to entities. However, to use location you would need to explicitly store it as a component or define join operations that implicitly calculate it on the fly.

```fsharp
fun param struct(p : Loc, cl : Climate, _ : Volcano) ->
    { cl with temperature = cl.temperature + 1 }
|> Join.update3
|> Join.over map)
```

### Units

You can store units in the typical way as entities:

```fsharp
let entity =
    c.Create()
        .With(Archer)
        .With({ x = 10; y = 20 })
        .With({ unitSize = 5 })                
```

However, for efficient world map queries, you may want to maintain a reference to units in the world map grid, along with any other components or markers useful for querying:

```fsharp
map.Get(loc).Add { unitEid = entity.id }
```

### Inspecting

Like entities, you can also print the content of grid cells:

```fsharp
printfn "%s" <| 
    c.GetInstance<WorldGrid>()
        .Get({ x = 10; y = 15 })
        .ToString()
```
Result:
``` 
Entity (10, 15): 12 bytes
Climate {tempurature = 79;
 humidity = 60;}
Terrain Grassland
```
Print all components:
```fsharp
printfn "%s" <| c.ToString()
```
Result:
```
Types
  Channels
    [None]
      PrintMap: 1H 0P 0E 1T 0SE
      ResetMap: 1H 0P 0E 1T 0SE
      StepSim: 1H 0P 0E 1T 0SE
    Garnet.Ecs
      Commit: 1H 0P 0E 0T 1SE
  Coroutines
    Stack: 0 pending, 0 active, frames: 
    Timed: 0 total, due: 0
  SegmentStore<Int32>
    Eid: 50/1C 
      C0 1 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx..............
    Loc: 50/1C 
      C0 1 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx..............
    UnitType: 50/1C 
      C0 1 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx..............
    UnitSize: 50/1C 
      C0 1 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx..............
  Pools
    0: 50C 0T 0P 0R
  SegmentStore<Loc>
    Climate: 400/9C 
      C0 (0, 0) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      C1 (0, 1) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    Ore: 43/8C 
      C0 (0, 0) ..x.....x.....................xx......x..x........x.....x...x...
      C1 (0, 1) ......x.....x................x....x....x.....x.....x.x...x......
    Terrain: 400/9C 
      C0 (0, 0) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      C1 (0, 1) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    City: 16/6C 
      C0 (0, 0) .........x......................x.....x.........................
      C1 (0, 1) .x................x.................x.x.........................
    Occupant: 48/9C 
      C0 (0, 0) ..x....................x...x..........x.....xx.........x........
      C1 (0, 1) x......x.........x....x.x..........x......x.........xx......x...
```


