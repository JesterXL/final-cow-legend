module World exposing (Col(..), Row(..), TileType(..), World, colToIndex, colToInt, defaultWorld, getCell, indexToCol, indexToRow, intToCol, intToRow, rowToIndex, rowToInt, setCell)

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


rowToInt : Row -> Int
rowToInt row =
    case row of
        Row0 ->
            0

        Row1 ->
            1

        Row2 ->
            2

        Row3 ->
            3

        Row4 ->
            4

        Row5 ->
            5

        Row6 ->
            6

        Row7 ->
            7

        Row8 ->
            8

        Row9 ->
            9

        Row10 ->
            10

        Row11 ->
            11

        Row12 ->
            12

        Row13 ->
            13

        Row14 ->
            14

        Row15 ->
            15

        Row16 ->
            16

        Row17 ->
            17

        Row18 ->
            18

        Row19 ->
            19

        Row20 ->
            20

        Row21 ->
            21

        Row22 ->
            22

        Row23 ->
            23

        Row24 ->
            24

        Row25 ->
            25

        Row26 ->
            26

        Row27 ->
            27

        Row28 ->
            28


colToInt : Col -> Int
colToInt col =
    case col of
        Col0 ->
            0

        Col1 ->
            1

        Col2 ->
            2

        Col3 ->
            3

        Col4 ->
            4

        Col5 ->
            5

        Col6 ->
            6

        Col7 ->
            7

        Col8 ->
            8

        Col9 ->
            9

        Col10 ->
            10

        Col11 ->
            11

        Col12 ->
            12

        Col13 ->
            13

        Col14 ->
            14

        Col15 ->
            15

        Col16 ->
            16

        Col17 ->
            17

        Col18 ->
            18

        Col19 ->
            19

        Col20 ->
            20

        Col21 ->
            21

        Col22 ->
            22

        Col23 ->
            23

        Col24 ->
            24

        Col25 ->
            25

        Col26 ->
            26

        Col27 ->
            27

        Col28 ->
            28

        Col29 ->
            29

        Col30 ->
            30


intToRow : Int -> Maybe Row
intToRow value =
    case value of
        0 ->
            Just Row0

        1 ->
            Just Row1

        2 ->
            Just Row2

        3 ->
            Just Row3

        4 ->
            Just Row4

        5 ->
            Just Row5

        6 ->
            Just Row6

        7 ->
            Just Row7

        8 ->
            Just Row8

        9 ->
            Just Row9

        10 ->
            Just Row10

        11 ->
            Just Row11

        12 ->
            Just Row12

        13 ->
            Just Row13

        14 ->
            Just Row14

        15 ->
            Just Row15

        16 ->
            Just Row16

        17 ->
            Just Row17

        18 ->
            Just Row18

        19 ->
            Just Row19

        20 ->
            Just Row20

        21 ->
            Just Row21

        22 ->
            Just Row22

        23 ->
            Just Row23

        24 ->
            Just Row24

        25 ->
            Just Row25

        26 ->
            Just Row26

        27 ->
            Just Row27

        28 ->
            Just Row28

        _ ->
            Nothing


intToCol : Int -> Maybe Col
intToCol value =
    case value of
        0 ->
            Just Col0

        1 ->
            Just Col1

        2 ->
            Just Col2

        3 ->
            Just Col3

        4 ->
            Just Col4

        5 ->
            Just Col5

        6 ->
            Just Col6

        7 ->
            Just Col7

        8 ->
            Just Col8

        9 ->
            Just Col9

        10 ->
            Just Col10

        11 ->
            Just Col11

        12 ->
            Just Col12

        13 ->
            Just Col13

        14 ->
            Just Col14

        15 ->
            Just Col15

        16 ->
            Just Col16

        17 ->
            Just Col17

        18 ->
            Just Col18

        19 ->
            Just Col19

        20 ->
            Just Col20

        21 ->
            Just Col21

        22 ->
            Just Col22

        23 ->
            Just Col23

        24 ->
            Just Col24

        25 ->
            Just Col25

        26 ->
            Just Col26

        27 ->
            Just Col27

        28 ->
            Just Col28

        29 ->
            Just Col29

        30 ->
            Just Col30

        _ ->
            Nothing


indexToRow : Vector29.Index -> Row
indexToRow index =
    case index of
        Vector29.Index0 ->
            Row0

        Vector29.Index1 ->
            Row1

        Vector29.Index2 ->
            Row2

        Vector29.Index3 ->
            Row3

        Vector29.Index4 ->
            Row4

        Vector29.Index5 ->
            Row5

        Vector29.Index6 ->
            Row6

        Vector29.Index7 ->
            Row7

        Vector29.Index8 ->
            Row8

        Vector29.Index9 ->
            Row9

        Vector29.Index10 ->
            Row10

        Vector29.Index11 ->
            Row11

        Vector29.Index12 ->
            Row12

        Vector29.Index13 ->
            Row13

        Vector29.Index14 ->
            Row14

        Vector29.Index15 ->
            Row15

        Vector29.Index16 ->
            Row16

        Vector29.Index17 ->
            Row17

        Vector29.Index18 ->
            Row18

        Vector29.Index19 ->
            Row19

        Vector29.Index20 ->
            Row20

        Vector29.Index21 ->
            Row21

        Vector29.Index22 ->
            Row22

        Vector29.Index23 ->
            Row23

        Vector29.Index24 ->
            Row24

        Vector29.Index25 ->
            Row25

        Vector29.Index26 ->
            Row26

        Vector29.Index27 ->
            Row27

        Vector29.Index28 ->
            Row28


indexToCol : Vector31.Index -> Col
indexToCol index =
    case index of
        Vector31.Index0 ->
            Col0

        Vector31.Index1 ->
            Col1

        Vector31.Index2 ->
            Col2

        Vector31.Index3 ->
            Col3

        Vector31.Index4 ->
            Col4

        Vector31.Index5 ->
            Col5

        Vector31.Index6 ->
            Col6

        Vector31.Index7 ->
            Col7

        Vector31.Index8 ->
            Col8

        Vector31.Index9 ->
            Col9

        Vector31.Index10 ->
            Col10

        Vector31.Index11 ->
            Col11

        Vector31.Index12 ->
            Col12

        Vector31.Index13 ->
            Col13

        Vector31.Index14 ->
            Col14

        Vector31.Index15 ->
            Col15

        Vector31.Index16 ->
            Col16

        Vector31.Index17 ->
            Col17

        Vector31.Index18 ->
            Col18

        Vector31.Index19 ->
            Col19

        Vector31.Index20 ->
            Col20

        Vector31.Index21 ->
            Col21

        Vector31.Index22 ->
            Col22

        Vector31.Index23 ->
            Col23

        Vector31.Index24 ->
            Col24

        Vector31.Index25 ->
            Col25

        Vector31.Index26 ->
            Col26

        Vector31.Index27 ->
            Col27

        Vector31.Index28 ->
            Col28

        Vector31.Index29 ->
            Col29

        Vector31.Index30 ->
            Col30
