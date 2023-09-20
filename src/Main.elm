module Main exposing (main)

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
import Time


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
    , characterFacingDirection : FacingDirection
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
    , mainCharacterSouth : Canvas.Texture.Texture
    , mainCharacterNorth : Canvas.Texture.Texture
    , mainCharacterWest : Canvas.Texture.Texture
    , mainCharacterEast : Canvas.Texture.Texture
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
    , characterFacingDirection = South
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

        _ ->
            MoveCursor Other


keyDecoderReleased : Decode.Decoder Msg
keyDecoderReleased =
    Decode.map toDirectionReleased (Decode.field "key" Decode.string)


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

                        updatedModel =
                            { model
                                | time = model.time + timePassedInt
                                , gameSetupStatus = SetupComplete sprites
                                , cameraX = cameraX
                                , cameraY = cameraY
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
                        , mainCharacterSouth =
                            Canvas.Texture.sprite
                                { x = 1
                                , y = 903
                                , width = 16
                                , height = 16
                                }
                                texture
                        , mainCharacterNorth =
                            Canvas.Texture.sprite
                                { x = 26
                                , y = 903
                                , width = 16
                                , height = 16
                                }
                                texture
                        , mainCharacterWest =
                            Canvas.Texture.sprite
                                { x = 50
                                , y = 903
                                , width = 16
                                , height = 16
                                }
                                texture
                        , mainCharacterEast =
                            Canvas.Texture.sprite
                                { x = 74
                                , y = 903
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

                RightPressed ->
                    ( { model | rightPressed = True, characterFacingDirection = East }, Cmd.none )

                LeftReleased ->
                    ( { model | leftPressed = False, characterFacingDirection = West }, Cmd.none )

                RightReleased ->
                    ( { model | rightPressed = False, characterFacingDirection = East }, Cmd.none )

                UpPressed ->
                    ( { model | upPressed = True, characterFacingDirection = North }, Cmd.none )

                UpReleased ->
                    ( { model | upPressed = False, characterFacingDirection = North }, Cmd.none )

                DownPressed ->
                    ( { model | downPressed = True, characterFacingDirection = South }, Cmd.none )

                DownReleased ->
                    ( { model | downPressed = False, characterFacingDirection = South }, Cmd.none )

                EnterPressed ->
                    ( model, Cmd.none )

                EnterReleased ->
                    ( model, Cmd.none )

                Other ->
                    ( model, Cmd.none )

        VisibilityChange vis ->
            case vis of
                Hidden ->
                    ( { model | paused = True }, Cmd.none )

                Visible ->
                    ( { model | paused = False }, Cmd.none )


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
                        ++ [ Canvas.texture
                                [ Canvas.Settings.Advanced.imageSmoothing False ]
                                ( model.characterX, model.characterY )
                                (case model.characterFacingDirection of
                                    North ->
                                        sprites.mainCharacterNorth

                                    South ->
                                        sprites.mainCharacterSouth

                                    East ->
                                        sprites.mainCharacterEast

                                    West ->
                                        sprites.mainCharacterWest
                                )
                           ]
                    )
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


gameHeight : Int
gameHeight =
    144



-- 9 16x16 tiles


gameWidthFloat : Float
gameWidthFloat =
    toFloat gameWidth


gameHeightFloat : Float
gameHeightFloat =
    toFloat gameHeight



-- drawEnemy : Sprites -> SpriteMonster -> Int -> List Canvas.Renderable
-- drawEnemy sprites spriteMonster index =
--     let
--         offsetIndex =
--             toFloat index + 0
--     in
--     [ Canvas.texture
--         []
--         -- ( 20, 40 + offsetIndex * 40 )
--         ( toFloat spriteMonster.x, toFloat spriteMonster.y )
--         sprites.rhobite
--     ]


pauseButton : Bool -> Html Msg
pauseButton paused =
    if paused == True then
        button [ onClick TogglePause ] [ Html.text "Unpause" ]

    else
        button [ onClick TogglePause ] [ Html.text "Pause" ]



-- drawMenu : Model -> BattleTimer -> Sprites -> List Canvas.Renderable
-- drawMenu model battleTimer sprites =
--     case model.battleState of
--         CharacterOrMonsterReady ->
--             [ shapes [ fill Color.blue ] [ rect ( 20, 200 ) 300 100 ]
--             , shapes [ stroke Color.white ] [ rect ( 20, 200 ) 300 100 ]
--             ]
--                 ++ List.map
--                     (\menuItem ->
--                         Canvas.text
--                             [ font { size = 26, family = "Final Fantasy VI SNESa" }, align Left, fill Color.white ]
--                             ( menuItem.x, menuItem.y )
--                             menuItem.text
--                     )
--                     model.menuItems
--         _ ->
--             [ shapes [ fill Color.blue ] [ rect ( 20, 200 ) 300 100 ]
--             , shapes [ stroke Color.white ] [ rect ( 20, 200 ) 300 100 ]
--             ]
-- drawCursor : Model -> Sprites -> List Canvas.Renderable
-- drawCursor model sprites =
--     case model.selectionTarget of
--         Nothing ->
--             []
--         -- type SelectionTarget
--         --     = MenuItemSelectionTarget MenuItem
--         --     | CharacterSelectionTarget SpriteCharacter
--         --     | MonsterSelectionTarget SpriteMonster
--         Just selectionTarget ->
--             case selectionTarget of
--                 MenuItemSelectionTarget menuItem ->
--                     [ Canvas.texture
--                         []
--                         ( menuItem.x - 32, menuItem.y - 13 )
--                         sprites.cursor
--                     ]
--                 CharacterSelectionTarget spriteCharacter ->
--                     [ Canvas.texture
--                         []
--                         ( spriteCharacter.x - 32, spriteCharacter.y - 13 )
--                         sprites.cursor
--                     ]
--                 MonsterSelectionTarget spriteMonster ->
--                     [ Canvas.texture
--                         []
--                         ( spriteMonster.x - 32, spriteMonster.y - 13 )
--                         sprites.cursor
--                     ]
-- drawDebug : Model -> List Canvas.Renderable
-- drawDebug model =
--     [ Canvas.text
--         [ font { size = 14, family = "sans-serif" }, align Left, fill Color.black ]
--         ( 200, 15 )
--         ("Battle State: "
--             ++ battleStateToString model.battleState
--         )
--     , Canvas.text
--         [ font { size = 14, family = "sans-serif" }, align Left, fill Color.black ]
--         ( 200, 30 )
--         ("Cursor Direction: "
--             ++ getCursorDirectionToString model
--         )
--     ]


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
