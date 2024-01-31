module World exposing (TileType(..), defaultWorld, getCell)

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


type alias RowIndex =
    Vector29.Index


getCell : Vector29.Index -> Vector31.Index -> World -> TileType
getCell row col world =
    let
        rowVector =
            Vector29.get row world

        tile =
            Vector31.get col rowVector
    in
    tile



-- setCell : Row -> Col -> TileType -> World -> World
-- setCell (Row row) (Col col) newValue world =
--     let
--         rowVector =
--             Vector29.get row world
--         updatedColVector =
--             Vector31.set col newValue rowVector
--         updatedRowVector =
--             Vector29.set row updatedColVector world
--     in
--     updatedRowVector
