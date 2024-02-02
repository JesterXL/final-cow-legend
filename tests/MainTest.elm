module MainTest exposing (..)

import Expect
import Fuzz exposing (..)
import Main exposing (FacingDirection(..), getCharacterDesiredCol, getCharacterDesiredRow)
import Test exposing (..)
import World exposing (Col(..), Row(..))


suite : Test
suite =
    describe "Main"
        [ describe "getCharacterDesiredRow"
            [ test "should increment south" <|
                \_ ->
                    Expect.equal (getCharacterDesiredRow South Row0) (Just Row1)
            , test "should increment north" <|
                \_ ->
                    Expect.equal (getCharacterDesiredRow North Row1) (Just Row0)
            , test "should give you same row if you can't go farther north" <|
                \_ ->
                    Expect.equal (getCharacterDesiredRow North Row0) Nothing
            , test "should give you same row if you can't go farther south" <|
                \_ ->
                    Expect.equal (getCharacterDesiredRow South Row28) Nothing
            ]
        , describe "getCharacterDesiredCol"
            [ test "should increment east" <|
                \_ ->
                    Expect.equal (getCharacterDesiredCol East Col0) (Just Col1)
            , test "should increment west" <|
                \_ ->
                    Expect.equal (getCharacterDesiredCol West Col1) (Just Col0)
            , test "should give you same row if you can't go farther west" <|
                \_ ->
                    Expect.equal (getCharacterDesiredCol West Col0) Nothing
            , test "should give you same row if you can't go farther east" <|
                \_ ->
                    Expect.equal (getCharacterDesiredCol East Col30) Nothing
            ]
        ]
