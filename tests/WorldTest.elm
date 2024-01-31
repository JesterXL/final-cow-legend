module WorldTest exposing (..)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
import Vector29
import Vector31
import World exposing (TileType(..), defaultWorld, getCell)


suite : Test
suite =
    describe "World"
        [ describe "basic"
            [ test "should show example test" <|
                \_ ->
                    Expect.equal True True
            ]
        , describe "defaultWorld"
            [ test "should get a default world where everything is walkable" <|
                \_ ->
                    let
                        allWalkable =
                            defaultWorld
                                |> Vector29.foldl
                                    (\vector31 acc ->
                                        if acc == False then
                                            False

                                        else
                                            Vector31.foldl
                                                (\tile isAcc ->
                                                    if isAcc == False then
                                                        False

                                                    else
                                                        tile == Walkable
                                                )
                                                True
                                                vector31
                                    )
                                    True
                    in
                    Expect.equal allWalkable True
            , test "should be able to get a cell tile" <|
                \_ ->
                    let
                        walkable =
                            case defaultWorld |> getCell Vector29.Index0 Vector31.Index0 of
                                Walkable ->
                                    True

                                NotWalkable ->
                                    False
                    in
                    Expect.equal walkable True
            ]
        ]
