namespace Garnet.Samples.Roguelike

[<AutoOpen>]
module WorldTypes =
    type Vector = {
        X : int
        Y : int
        }

    type Bounds = {
        Min : Vector
        Max : Vector
        }

    type Direction =
        | East
        | West
        | North
        | South

    type DistanceMap = {
        Distances : Map<Vector, int>
        }
        
    type Terrain =
        | Floor
        | Wall

    type EntityType =
        | Rogue
        | Minion

    type Entity = {
        EntityType : EntityType
        Hits : int
        }

    type Tile = {
        Terrain : Terrain
        Entity : Entity option
        }

    type Action =
        | Move of Direction

    type MovedEvent = {
        SourceLoc : Vector
        MoveDir : Direction
        }

    type AttackedEvent = {
        AttackerLoc : Vector
        AttackDir : Direction
        Damage : int
        }

    /// Events hold the minimal information needed to reconstruct world state
    type Event =
        | Moved of MovedEvent
        | Attacked of AttackedEvent
        | Destroyed of Vector
        
    type MovingEvent = {
        SourceLoc : Vector
        MoveDir : Direction
        }

    type AttackingAnimation = {
        AttackerLoc : Vector
        AttackerEntityType : EntityType
        AttackDir : Direction
        Damage : int
        TargetEntityType : EntityType
        }
        
    type DestroyingAnimation = {
        DestroyedLoc : Vector
        DestroyedEntityType : EntityType
        }

    /// Animations can enriched with extra info to help present events that occurred
    /// during a turn to the player
    type Animation =
        | Moving of MovingEvent
        | Attacking of AttackingAnimation
        | Destroying of DestroyingAnimation

    type World = {
        Turn : int
        RandomSeed : uint64
        Tiles : Map<Vector, Tile>
        Animations : Animation list
        }

    [<Struct>]
    type Command =
        | None = 0
        | MoveEast = 1
        | MoveNorth = 2
        | MoveWest = 3
        | MoveSouth = 4
        | Reset = 5
        | FullScreen = 6
