module World exposing (Col(..), Row(..), TileType(..), defaultWorld, getCell, setCell)

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


type Row
    = Row0
    | Row1
    | Row2


type Col
    = Col0
    | Col1
    | Col2


rowToIndex : Row -> Vector29.Index
rowToIndex row =
    case row of
        Row0 ->
            Vector29.Index0

        Row1 ->
            Vector29.Index1

        Row2 ->
            Vector29.Index2


colToIndex : Col -> Vector31.Index
colToIndex col =
    case col of
        Col0 ->
            Vector31.Index0

        Col1 ->
            Vector31.Index1

        Col2 ->
            Vector31.Index2


getCell : Row -> Col -> World -> TileType
getCell row col world =
    let
        rowIndex =
            rowToIndex row

        colIndex =
            colToIndex col

        rowVector =
            Vector29.get rowIndex world

        tile =
            Vector31.get colIndex rowVector
    in
    tile


setCell : Row -> Col -> TileType -> World -> World
setCell row col newValue world =
    let
        rowIndex =
            rowToIndex row

        colIndex =
            colToIndex col

        rowVector =
            Vector29.get rowIndex world

        updatedColVector =
            Vector31.set colIndex newValue rowVector

        updatedRowVector =
            Vector29.set rowIndex updatedColVector world
    in
    updatedRowVector
