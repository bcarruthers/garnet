namespace Garnet.Samples.Roguelike.Types

type Vector = {
    x : int
    y : int
    }

type Bounds = {
    min : Vector
    max : Vector
    }

type Direction =
    | East
    | West
    | North
    | South
    
type Terrain =
    | Floor
    | Wall

type EntityType =
    | Player
    | Minion

type Entity = {
    entityType : EntityType
    hits : int
    }

type Tile = {
    terrain : Terrain
    entity : Entity option
    }

type Action =
    | Move of Direction

type MovedEvent = {
    sourceLoc : Vector
    moveDir : Direction
    }

type AttackedEvent = {
    attackerLoc : Vector
    attackDir : Direction
    damage : int
    }

/// Events hold the minimal information needed to reconstruct world state
type Event =
    | Moved of MovedEvent
    | Attacked of AttackedEvent
    | Destroyed of Vector
    
type MovingEvent = {
    sourceLoc : Vector
    moveDir : Direction
    }

type AttackingAnimation = {
    attackerLoc : Vector
    attackerEntityType : EntityType
    attackDir : Direction
    damage : int
    targetEntityType : EntityType
    }
    
type DestroyingAnimation = {
    destroyedLoc : Vector
    destroyedEntityType : EntityType
    }

/// Animations can enriched with extra info to help present events that occurred
/// during a turn to the player
type Animation =
    | Moving of MovingEvent
    | Attacking of AttackingAnimation
    | Destroying of DestroyingAnimation

type World = {
    turn : int
    randomSeed : uint64
    tiles : Map<Vector, Tile>
    animations : Animation list
    }

[<Struct>]
type Command =
    | None = 0
    | MoveEast = 1
    | MoveNorth = 2
    | MoveWest = 3
    | MoveSouth = 4
    | FullScreen = 5
