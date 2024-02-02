module MainTest exposing (..)

import Expect
import Fuzz exposing (..)
import Main exposing (FacingDirection(..), getCharacterDesiredRow)
import Test exposing (..)
import World exposing (Row(..))


suite : Test
suite =
    describe "Main"
        [ describe "getCharacterDesiredRow"
            [ test "should increment south" <|
                \_ ->
                    Expect.equal (getCharacterDesiredRow South Row0) Row1
            , test "should increment north" <|
                \_ ->
                    Expect.equal (getCharacterDesiredRow North Row1) Row0
            , test "should give you same row if you can't go farther north" <|
                \_ ->
                    Expect.equal (getCharacterDesiredRow North Row0) Row0
            , test "should give you same row if you can't go farther south" <|
                \_ ->
                    Expect.equal (getCharacterDesiredRow South Row28) Row28
            ]
        ]
