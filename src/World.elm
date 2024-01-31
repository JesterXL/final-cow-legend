module World exposing (TileType(..), defaultWorld)

import Vector29
import Vector31


type alias World =
    Vector29.Vector29 (Vector31.Vector31 TileType)


type TileType
    = Walkable
    | NotWalkable


defaultWorld : World
defaultWorld =
    Vector31.initializeFromInt (\_ -> Walkable)
        |> Vector29.repeat
