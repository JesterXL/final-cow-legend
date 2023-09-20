module Main exposing (main)

import AStar exposing (findPath, straightLineCost)
import Animator exposing (color)
import Array exposing (Array)
import Battle exposing (Attack(..), AttackMissesDeathProtectedTargets(..), BattlePower(..), CharacterStats, Defense(..), Element(..), EquippedRelics, Evade(..), Formation(..), Gold(..), HitPoints(..), HitRate(..), HitResult(..), Item(..), Level(..), MBlock(..), MagicDefense(..), MagicPoints(..), MagicPower(..), Monster(..), MonsterStats, PlayableCharacter, Relic(..), Speed(..), SpellPower(..), Stamina(..), Vigor(..), XP(..), dirk, fireSpell, getDamage, getHit, getRandomNumberFromRange, hitResultToString, lockeStats, lockeTarget, playableLocke, playableSabin, playableTerra, terraAttacker, terraStats)
import Browser
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyUp, onVisibilityChange)
import Canvas exposing (Point, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Canvas.Texture exposing (sprite)
import Color
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random
import Set


type alias Model =
    { initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , paused : Bool
    , time : Int
    , gameSetupStatus : GameSetupStatus
    , leftPressed : Bool
    , rightPressed : Bool
    , upPressed : Bool
    , downPressed : Bool
    , cameraX : Float
    , cameraY : Float
    , characterX : Float
    , characterY : Float
    , characterRow : Int
    , characterCol : Int
    , characterFacingDirection : FacingDirection
    , characterFrameTime : Int
    , characterFrame : Int
    , world : World
    }


type FacingDirection
    = North
    | South
    | East
    | West


type GameSetupStatus
    = SettingUp
    | SetupComplete Sprites
    | SetupFailed


type alias Sprites =
    { cursor : Canvas.Texture.Texture
    , towerBase : Canvas.Texture.Texture
    , mainCharacterSouth1 : Canvas.Texture.Texture
    , mainCharacterSouth2 : Canvas.Texture.Texture
    , mainCharacterNorth1 : Canvas.Texture.Texture
    , mainCharacterNorth2 : Canvas.Texture.Texture
    , mainCharacterWest1 : Canvas.Texture.Texture
    , mainCharacterWest2 : Canvas.Texture.Texture
    , mainCharacterEast1 : Canvas.Texture.Texture
    , mainCharacterEast2 : Canvas.Texture.Texture
    }


initialModel : Random.Seed -> Model
initialModel seed =
    { initialSeed = seed
    , currentSeed = seed
    , paused = False
    , time = 0
    , gameSetupStatus = SettingUp
    , leftPressed = False
    , rightPressed = False
    , upPressed = False
    , downPressed = False
    , cameraX = 0
    , cameraY = 0
    , characterX = 72
    , characterY = 64
    , characterRow = 4
    , characterCol = 4
    , characterFacingDirection = South
    , characterFrameTime = 0
    , characterFrame = 0
    , world = defaultWorld
    }


type Direction
    = LeftPressed
    | RightPressed
    | LeftReleased
    | RightReleased
    | UpPressed
    | UpReleased
    | DownPressed
    | DownReleased
    | EnterPressed
    | EnterReleased
    | WPressed
    | WReleased
    | APressed
    | AReleased
    | SPressed
    | SReleased
    | DPressed
    | DReleased
    | Other


keyDecoderPressed : Decode.Decoder Msg
keyDecoderPressed =
    Decode.map toDirectionPressed (Decode.field "key" Decode.string)


toDirectionPressed : String -> Msg
toDirectionPressed string =
    let
        _ =
            Debug.log "key pressed" string
    in
    case string of
        "ArrowLeft" ->
            MoveCursor LeftPressed

        "ArrowRight" ->
            MoveCursor RightPressed

        "ArrowUp" ->
            MoveCursor UpPressed

        "ArrowDown" ->
            MoveCursor DownPressed

        "Enter" ->
            MoveCursor EnterPressed

        "w" ->
            MoveCursor WPressed

        "a" ->
            MoveCursor APressed

        "s" ->
            MoveCursor SPressed

        "d" ->
            MoveCursor DPressed

        _ ->
            MoveCursor Other


toDirectionReleased : String -> Msg
toDirectionReleased string =
    case string of
        "ArrowLeft" ->
            MoveCursor LeftReleased

        "ArrowRight" ->
            MoveCursor RightReleased

        "ArrowUp" ->
            MoveCursor UpReleased

        "ArrowDown" ->
            MoveCursor DownReleased

        "Enter" ->
            MoveCursor EnterReleased

        "w" ->
            MoveCursor WReleased

        "a" ->
            MoveCursor AReleased

        "s" ->
            MoveCursor SReleased

        "d" ->
            MoveCursor DReleased

        _ ->
            MoveCursor Other


keyDecoderReleased : Decode.Decoder Msg
keyDecoderReleased =
    Decode.map toDirectionReleased (Decode.field "key" Decode.string)


type alias World =
    Array (Array TileType)


type TileType
    = Walkable
    | NotWalkable


defaultWorld : World
defaultWorld =
    -- Array.repeat 32 (Array.repeat 35 NotWalkable)
    Array.repeat 8 (Array.repeat 8 NotWalkable)
        |> setCell (Row 0) (Col 0) NotWalkable
        |> setCell (Row 1) (Col 0) NotWalkable
        |> setCell (Row 2) (Col 0) NotWalkable
        |> setCell (Row 3) (Col 0) NotWalkable
        |> setCell (Row 4) (Col 0) NotWalkable
        |> setCell (Row 5) (Col 0) NotWalkable
        |> setCell (Row 6) (Col 0) NotWalkable
        |> setCell (Row 7) (Col 0) NotWalkable
        |> setCell (Row 0) (Col 1) NotWalkable
        |> setCell (Row 1) (Col 1) NotWalkable
        |> setCell (Row 2) (Col 1) NotWalkable
        |> setCell (Row 3) (Col 1) NotWalkable
        |> setCell (Row 4) (Col 1) NotWalkable
        |> setCell (Row 5) (Col 1) NotWalkable
        |> setCell (Row 6) (Col 1) NotWalkable
        |> setCell (Row 7) (Col 1) NotWalkable
        |> setCell (Row 0) (Col 2) NotWalkable
        |> setCell (Row 1) (Col 2) NotWalkable
        |> setCell (Row 2) (Col 2) NotWalkable
        |> setCell (Row 3) (Col 2) NotWalkable
        |> setCell (Row 4) (Col 2) NotWalkable
        |> setCell (Row 5) (Col 2) NotWalkable
        |> setCell (Row 6) (Col 2) NotWalkable
        |> setCell (Row 7) (Col 2) NotWalkable
        |> setCell (Row 0) (Col 3) NotWalkable
        |> setCell (Row 1) (Col 3) NotWalkable
        |> setCell (Row 2) (Col 3) NotWalkable
        |> setCell (Row 3) (Col 3) NotWalkable
        |> setCell (Row 4) (Col 3) NotWalkable
        |> setCell (Row 5) (Col 3) NotWalkable
        |> setCell (Row 6) (Col 3) NotWalkable
        |> setCell (Row 7) (Col 3) NotWalkable
        |> setCell (Row 0) (Col 4) NotWalkable
        |> setCell (Row 1) (Col 4) NotWalkable
        |> setCell (Row 2) (Col 4) NotWalkable
        |> setCell (Row 3) (Col 4) NotWalkable
        |> setCell (Row 4) (Col 4) Walkable
        |> setCell (Row 5) (Col 4) Walkable
        |> setCell (Row 6) (Col 4) Walkable
        |> setCell (Row 7) (Col 4) Walkable
        |> setCell (Row 0) (Col 5) NotWalkable
        |> setCell (Row 1) (Col 5) NotWalkable
        |> setCell (Row 2) (Col 5) NotWalkable
        |> setCell (Row 3) (Col 5) NotWalkable
        |> setCell (Row 4) (Col 5) NotWalkable
        |> setCell (Row 5) (Col 5) NotWalkable
        |> setCell (Row 6) (Col 5) NotWalkable
        |> setCell (Row 7) (Col 5) NotWalkable


type Row
    = Row Int


type Col
    = Col Int


getCell : Row -> Col -> World -> Maybe TileType
getCell (Row row) (Col col) world =
    -- let
    --     _ =
    --         Debug.log "getCell, row:" row
    --     _ =
    --         Debug.log "getCell, col:" col
    -- in
    Array.get row world
        |> Maybe.andThen
            (\rowItem ->
                --     let
                --         _ =
                --             Debug.log "rowItem is:" rowItem
                --     in
                Array.get col rowItem
            )


setCell : Row -> Col -> TileType -> World -> World
setCell (Row row) (Col col) newValue world =
    Array.get row world
        |> Maybe.andThen
            (\rowList ->
                let
                    updatedRowList =
                        Array.set col newValue rowList

                    updatedWorld =
                        Array.set row updatedRowList world
                in
                Just updatedWorld
            )
        |> Maybe.withDefault world


type Msg
    = TogglePause
    | Frame Float
    | TextureLoaded (Maybe Canvas.Texture.Texture)
    | MoveCursor Direction
    | VisibilityChange Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        Frame timePassed ->
            case model.gameSetupStatus of
                SettingUp ->
                    ( model, Cmd.none )

                SetupFailed ->
                    ( model, Cmd.none )

                SetupComplete sprites ->
                    let
                        timePassedInt =
                            round timePassed

                        ( cameraX, cameraY ) =
                            updateCamera model timePassed

                        ( newCharacterFrameTime, newCharacterFrame ) =
                            updateCharacterFrameTime (model.characterFrameTime + timePassedInt) model.characterFrame

                        ( newCharacterX, newCharacterY ) =
                            updateCharacterXAndY model timePassed cameraX cameraY

                        updatedModel =
                            { model
                                | time = model.time + timePassedInt
                                , gameSetupStatus = SetupComplete sprites
                                , cameraX = cameraX
                                , cameraY = cameraY
                                , characterFrameTime = newCharacterFrameTime
                                , characterFrame = newCharacterFrame
                                , characterX = newCharacterX
                                , characterY = newCharacterY
                            }
                    in
                    ( updatedModel, Cmd.none )

        TextureLoaded Nothing ->
            ( { model | gameSetupStatus = SetupFailed }, Cmd.none )

        TextureLoaded (Just texture) ->
            ( { model
                | gameSetupStatus =
                    SetupComplete
                        { cursor =
                            Canvas.Texture.sprite
                                { x = 68
                                , y = 217
                                , width = 32
                                , height = 32
                                }
                                texture
                        , towerBase =
                            Canvas.Texture.sprite
                                { x = 5
                                , y = 5
                                , width = 608
                                , height = 565
                                }
                                texture
                        , mainCharacterSouth1 =
                            Canvas.Texture.sprite
                                { x = 1
                                , y = 903
                                , width = 16
                                , height = 16
                                }
                                texture
                        , mainCharacterSouth2 =
                            Canvas.Texture.sprite
                                { x = 1
                                , y = 1022
                                , width = 16
                                , height = 16
                                }
                                texture
                        , mainCharacterNorth1 =
                            Canvas.Texture.sprite
                                { x = 26
                                , y = 903
                                , width = 16
                                , height = 16
                                }
                                texture
                        , mainCharacterNorth2 =
                            Canvas.Texture.sprite
                                { x = 25
                                , y = 1022
                                , width = 16
                                , height = 16
                                }
                                texture
                        , mainCharacterWest1 =
                            Canvas.Texture.sprite
                                { x = 50
                                , y = 903
                                , width = 16
                                , height = 16
                                }
                                texture
                        , mainCharacterWest2 =
                            Canvas.Texture.sprite
                                { x = 74
                                , y = 1021
                                , width = 16
                                , height = 16
                                }
                                texture
                        , mainCharacterEast1 =
                            Canvas.Texture.sprite
                                { x = 74
                                , y = 903
                                , width = 16
                                , height = 16
                                }
                                texture
                        , mainCharacterEast2 =
                            Canvas.Texture.sprite
                                { x = 50
                                , y = 1022
                                , width = 16
                                , height = 16
                                }
                                texture
                        }
              }
            , Cmd.none
            )

        MoveCursor direction ->
            case direction of
                LeftPressed ->
                    ( { model | leftPressed = True, characterFacingDirection = West }, Cmd.none )

                LeftReleased ->
                    ( { model | leftPressed = False }, Cmd.none )

                RightPressed ->
                    ( { model | rightPressed = True, characterFacingDirection = East }, Cmd.none )

                RightReleased ->
                    ( { model | rightPressed = False }, Cmd.none )

                UpPressed ->
                    ( { model | upPressed = True, characterFacingDirection = North }, Cmd.none )

                UpReleased ->
                    ( { model | upPressed = False }, Cmd.none )

                DownPressed ->
                    ( { model | downPressed = True, characterFacingDirection = South }, Cmd.none )

                DownReleased ->
                    ( { model | downPressed = False }, Cmd.none )

                EnterPressed ->
                    let
                        datWorldString =
                            Array.map
                                debugTileRowToString
                                model.world

                        -- |> Array.foldl
                        --     (\rowStr tileTypeString ->
                        --         rowStr ++ tileTypeString ++ "\n"
                        --     )
                        --     ""
                        _ =
                            Debug.log "datWorldString" datWorldString

                        datPath =
                            findPath
                                straightLineCost
                                (\( x, y ) ->
                                    Set.singleton ( x + 1, y )
                                        |> Set.insert ( x, y + 1 )
                                        |> Set.insert ( x - 1, y )
                                        |> Set.insert ( x, y - 1 )
                                )
                                ( 0, 0 )
                                ( 2, 0 )

                        _ =
                            Debug.log "datPath" datPath
                    in
                    ( model, Cmd.none )

                EnterReleased ->
                    ( model, Cmd.none )

                WPressed ->
                    let
                        canMoveNorth =
                            case getCell (Row (model.characterRow - 1)) (Col model.characterCol) model.world of
                                Just tileType ->
                                    if tileType == Walkable then
                                        True

                                    else
                                        False

                                Nothing ->
                                    False

                        _ =
                            Debug.log "canMoveNorth" canMoveNorth
                    in
                    if canMoveNorth == True then
                        ( { model | characterRow = model.characterRow - 1 }, Cmd.none )

                    else
                        ( model, Cmd.none )

                WReleased ->
                    ( model, Cmd.none )

                APressed ->
                    ( model, Cmd.none )

                AReleased ->
                    ( model, Cmd.none )

                SPressed ->
                    let
                        canMoveSouth =
                            case getCell (Row (model.characterRow + 1)) (Col model.characterCol) model.world of
                                Just tileType ->
                                    if tileType == Walkable then
                                        True

                                    else
                                        False

                                Nothing ->
                                    False

                        _ =
                            Debug.log "canMoveSouth" canMoveSouth
                    in
                    if canMoveSouth == True then
                        ( { model | characterRow = model.characterRow + 1 }, Cmd.none )

                    else
                        ( model, Cmd.none )

                SReleased ->
                    ( model, Cmd.none )

                DPressed ->
                    ( model, Cmd.none )

                DReleased ->
                    ( model, Cmd.none )

                Other ->
                    ( model, Cmd.none )

        VisibilityChange vis ->
            case vis of
                Hidden ->
                    ( { model | paused = True }, Cmd.none )

                Visible ->
                    ( { model | paused = False }, Cmd.none )


debugTileRowToString : Array TileType -> String
debugTileRowToString row =
    Array.map
        (\tileType ->
            if tileType == Walkable then
                " 0 "

            else
                " 1 "
        )
        row
        |> Array.foldl
            (\rowStr tileTypeString ->
                rowStr ++ tileTypeString
            )
            ""


updateCamera : Model -> Float -> ( Float, Float )
updateCamera model timePassed =
    let
        pixels =
            10 / timePassed
    in
    if model.leftPressed then
        ( model.cameraX + pixels, model.cameraY )

    else if model.rightPressed then
        ( model.cameraX - pixels, model.cameraY )

    else if model.upPressed then
        ( model.cameraX, model.cameraY + pixels )

    else if model.downPressed then
        ( model.cameraX, model.cameraY - pixels )

    else
        ( model.cameraX, model.cameraY )


updateCharacterXAndY : Model -> Float -> Float -> Float -> ( Float, Float )
updateCharacterXAndY model timePassed cameraX cameraY =
    ( toFloat model.characterCol * 16 + cameraX, toFloat model.characterRow * 16 + cameraY )


updateCharacterFrameTime : Int -> Int -> ( Int, Int )
updateCharacterFrameTime characterFrameTime characterFrame =
    if characterFrameTime > 200 then
        if characterFrame == 0 then
            ( 0, 1 )

        else
            ( 0, 0 )

    else
        ( characterFrameTime, characterFrame )


view : Model -> Html Msg
view model =
    div [ class "flex flex-row" ]
        [ case model.gameSetupStatus of
            SettingUp ->
                Canvas.toHtmlWith
                    { width = gameWidth
                    , height = gameHeight
                    , textures = [ Canvas.Texture.loadFromImageUrl "FFL-TowerBase.png" TextureLoaded ]
                    }
                    []
                    [ Canvas.text
                        [ font { size = 48, family = "sans-serif" }, align Center ]
                        ( 50, 50 )
                        "Loading textures..."
                    ]

            SetupFailed ->
                Canvas.toHtmlWith
                    { width = gameWidth
                    , height = gameHeight
                    , textures = []
                    }
                    []
                    [ Canvas.text
                        [ font { size = 48, family = "sans-serif" }, align Center ]
                        ( 50, 50 )
                        "Setup failed."
                    ]

            SetupComplete sprites ->
                Canvas.toHtmlWith
                    { width = gameWidth
                    , height = gameHeight
                    , textures = []
                    }
                    -- [ class "block scale-[2] pixel-art" ]
                    [ class "block pixel-art" ]
                    ([ shapes
                        [ fill (Color.rgb 0.85 0.92 1) ]
                        [ rect ( 0, 0 ) gameWidthFloat gameHeightFloat ]
                     ]
                        ++ [ Canvas.texture
                                [ Canvas.Settings.Advanced.imageSmoothing False ]
                                ( model.cameraX, model.cameraY )
                                sprites.towerBase
                           ]
                        ++ getCharacterFrame model sprites
                        ++ drawWorld model
                    )
        ]


getCharacterFrame : Model -> Sprites -> List Canvas.Renderable
getCharacterFrame model sprites =
    case model.characterFacingDirection of
        North ->
            if model.characterFrame == 0 then
                [ Canvas.texture
                    [ Canvas.Settings.Advanced.imageSmoothing False ]
                    ( model.characterX, model.characterY )
                    sprites.mainCharacterNorth1
                ]

            else
                [ Canvas.texture
                    [ Canvas.Settings.Advanced.imageSmoothing False ]
                    ( model.characterX, model.characterY )
                    sprites.mainCharacterNorth2
                ]

        South ->
            if model.characterFrame == 0 then
                [ Canvas.texture
                    [ Canvas.Settings.Advanced.imageSmoothing False ]
                    ( model.characterX, model.characterY )
                    sprites.mainCharacterSouth1
                ]

            else
                [ Canvas.texture
                    [ Canvas.Settings.Advanced.imageSmoothing False ]
                    ( model.characterX, model.characterY )
                    sprites.mainCharacterSouth2
                ]

        East ->
            if model.characterFrame == 0 then
                [ Canvas.texture
                    [ Canvas.Settings.Advanced.imageSmoothing False ]
                    ( model.characterX, model.characterY )
                    sprites.mainCharacterEast1
                ]

            else
                [ Canvas.texture
                    [ Canvas.Settings.Advanced.imageSmoothing False ]
                    ( model.characterX, model.characterY )
                    sprites.mainCharacterEast2
                ]

        West ->
            if model.characterFrame == 0 then
                [ Canvas.texture
                    [ Canvas.Settings.Advanced.imageSmoothing False ]
                    ( model.characterX, model.characterY )
                    sprites.mainCharacterWest1
                ]

            else
                [ Canvas.texture
                    [ Canvas.Settings.Advanced.imageSmoothing False ]
                    ( model.characterX, model.characterY )
                    sprites.mainCharacterWest2
                ]



-- :: [ Canvas.texture
--         []
--         ( toFloat sabin.x, toFloat sabin.y )
--         sprites.sabin
--    ]
-- ++ (List.indexedMap
--         (\index spriteCharacter ->
--             drawBar spriteCharacter.atbGauge index
--         )
--         model.characters
--         |> List.concatMap
--             (\bar -> bar)
--    )
-- ++ (List.indexedMap
--         (\index spriteMonster ->
--             drawEnemy sprites spriteMonster index
--         )
--         model.enemies
--         |> List.concatMap
--             (\monster -> monster)
--    )
-- ++ drawMenu model model.battleTimer sprites
-- ++ drawCursor model sprites
-- ++ drawDebug model


gameWidth : Int
gameWidth =
    160



-- 10 16x16 tiles
-- 1st map is 608 pixels width, 512 walkable, 32 total tiles


gameHeight : Int
gameHeight =
    144



-- 9 16x16 tiles
-- 1st map is 565 pixels height, 560 walkable, 35 total tiles


gameWidthFloat : Float
gameWidthFloat =
    toFloat gameWidth


gameHeightFloat : Float
gameHeightFloat =
    toFloat gameHeight


pauseButton : Bool -> Html Msg
pauseButton paused =
    if paused == True then
        button [ onClick TogglePause ] [ Html.text "Unpause" ]

    else
        button [ onClick TogglePause ] [ Html.text "Pause" ]


getCursorDirectionToString : Model -> String
getCursorDirectionToString { leftPressed, rightPressed, upPressed, downPressed } =
    if leftPressed == True then
        "Left"

    else if rightPressed == True then
        "Right"

    else if upPressed == True then
        "Up"

    else if downPressed == True then
        "Down"

    else
        "None"


drawWorld : Model -> List Canvas.Renderable
drawWorld model =
    [ Canvas.group
        [ Canvas.Settings.Advanced.alpha 0.5, Canvas.Settings.Advanced.transform [ Canvas.Settings.Advanced.translate (-8 + model.cameraX) (-11 + model.cameraY) ] ]
        (Array.indexedMap
            (\rowIndex row ->
                Array.indexedMap
                    (\colIndex cell ->
                        drawCell (Row rowIndex) (Col colIndex) cell
                    )
                    row
                    |> Array.toList
                    |> List.concatMap
                        (\cell -> cell)
            )
            model.world
            |> Array.toList
            |> List.concatMap
                (\cell -> cell)
        )
    ]


drawCell : Row -> Col -> TileType -> List Canvas.Renderable
drawCell (Row row) (Col col) tileType =
    [ shapes
        [ if tileType == Walkable then
            fill Color.green

          else
            fill Color.red
        ]
        [ rect ( (toFloat col + 1) * 16, (toFloat row + 1) * 16 ) 16 16 ]
    , shapes [ stroke Color.lightGreen ] [ rect ( (toFloat col + 1) * 16, (toFloat row + 1) * 16 ) 16 16 ]
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel (Random.initialSeed 42), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused == False then
        -- animator |> Animator.toSubscription Tick model
        Sub.batch
            [ onVisibilityChange VisibilityChange
            , onAnimationFrameDelta Frame
            , onKeyDown keyDecoderPressed
            , onKeyUp keyDecoderReleased
            ]

    else
        onVisibilityChange VisibilityChange


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
