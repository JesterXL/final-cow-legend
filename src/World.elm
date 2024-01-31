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
    | Row3
    | Row4
    | Row5
    | Row6
    | Row7
    | Row8
    | Row9
    | Row10
    | Row11
    | Row12
    | Row13
    | Row14
    | Row15
    | Row16
    | Row17
    | Row18
    | Row19
    | Row20
    | Row21
    | Row22
    | Row23
    | Row24
    | Row25
    | Row26
    | Row27
    | Row28


type Col
    = Col0
    | Col1
    | Col2
    | Col3
    | Col4
    | Col5
    | Col6
    | Col7
    | Col8
    | Col9
    | Col10
    | Col11
    | Col12
    | Col13
    | Col14
    | Col15
    | Col16
    | Col17
    | Col18
    | Col19
    | Col20
    | Col21
    | Col22
    | Col23
    | Col24
    | Col25
    | Col26
    | Col27
    | Col28
    | Col29
    | Col30


rowToIndex : Row -> Vector29.Index
rowToIndex row =
    case row of
        Row0 ->
            Vector29.Index0

        Row1 ->
            Vector29.Index1

        Row2 ->
            Vector29.Index2

        Row3 ->
            Vector29.Index3

        Row4 ->
            Vector29.Index4

        Row5 ->
            Vector29.Index5

        Row6 ->
            Vector29.Index6

        Row7 ->
            Vector29.Index7

        Row8 ->
            Vector29.Index8

        Row9 ->
            Vector29.Index9

        Row10 ->
            Vector29.Index10

        Row11 ->
            Vector29.Index11

        Row12 ->
            Vector29.Index12

        Row13 ->
            Vector29.Index13

        Row14 ->
            Vector29.Index14

        Row15 ->
            Vector29.Index15

        Row16 ->
            Vector29.Index16

        Row17 ->
            Vector29.Index17

        Row18 ->
            Vector29.Index18

        Row19 ->
            Vector29.Index19

        Row20 ->
            Vector29.Index20

        Row21 ->
            Vector29.Index21

        Row22 ->
            Vector29.Index22

        Row23 ->
            Vector29.Index23

        Row24 ->
            Vector29.Index24

        Row25 ->
            Vector29.Index25

        Row26 ->
            Vector29.Index26

        Row27 ->
            Vector29.Index27

        Row28 ->
            Vector29.Index28


colToIndex : Col -> Vector31.Index
colToIndex col =
    case col of
        Col0 ->
            Vector31.Index0

        Col1 ->
            Vector31.Index1

        Col2 ->
            Vector31.Index2

        Col3 ->
            Vector31.Index3

        Col4 ->
            Vector31.Index4

        Col5 ->
            Vector31.Index5

        Col6 ->
            Vector31.Index6

        Col7 ->
            Vector31.Index7

        Col8 ->
            Vector31.Index8

        Col9 ->
            Vector31.Index9

        Col10 ->
            Vector31.Index10

        Col11 ->
            Vector31.Index11

        Col12 ->
            Vector31.Index12

        Col13 ->
            Vector31.Index13

        Col14 ->
            Vector31.Index14

        Col15 ->
            Vector31.Index15

        Col16 ->
            Vector31.Index16

        Col17 ->
            Vector31.Index17

        Col18 ->
            Vector31.Index18

        Col19 ->
            Vector31.Index19

        Col20 ->
            Vector31.Index20

        Col21 ->
            Vector31.Index21

        Col22 ->
            Vector31.Index22

        Col23 ->
            Vector31.Index23

        Col24 ->
            Vector31.Index24

        Col25 ->
            Vector31.Index25

        Col26 ->
            Vector31.Index26

        Col27 ->
            Vector31.Index27

        Col28 ->
            Vector31.Index28

        Col29 ->
            Vector31.Index29

        Col30 ->
            Vector31.Index30


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
