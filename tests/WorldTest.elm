module WorldTest exposing (..)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
import Vector29
import Vector31
import World exposing (Col(..), Row(..), TileType(..), defaultWorld, getCell, setCell)


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
                            case defaultWorld |> getCell Row0 Col0 of
                                Walkable ->
                                    True

                                NotWalkable ->
                                    False
                    in
                    Expect.equal walkable True
            , test "should be able to set a tile" <|
                \_ ->
                    let
                        world =
                            defaultWorld

                        starterTile =
                            world
                                |> getCell Row0 Col0

                        updatedTile =
                            setCell Row0 Col0 NotWalkable world
                                |> getCell Row0 Col0

                        firstTileRight =
                            case starterTile of
                                Walkable ->
                                    True

                                NotWalkable ->
                                    False

                        secondTileCorrect =
                            case updatedTile of
                                Walkable ->
                                    False

                                NotWalkable ->
                                    True

                        legit =
                            firstTileRight == True && secondTileCorrect == True
                    in
                    Expect.equal legit True
            ]
        ]
