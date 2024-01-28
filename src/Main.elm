port module Main exposing (main)

import AStar exposing (findPath, straightLineCost)
import Animator exposing (color)
import Array exposing (Array)
import Base64.Encode as Base64Encode
import Battle exposing (Attack(..), AttackMissesDeathProtectedTargets(..), BattlePower(..), CharacterStats, Defense(..), Element(..), EquippedRelics, Evade(..), Formation(..), Gold(..), HitPoints(..), HitRate(..), HitResult(..), Item(..), Level(..), MBlock(..), MagicDefense(..), MagicPoints(..), MagicPower(..), Monster(..), MonsterStats, PlayableCharacter, Relic(..), Speed(..), SpellPower(..), Stamina(..), Vigor(..), XP(..), dirk, fireSpell, getDamage, getHit, getRandomNumberFromRange, hitResultToString, lockeStats, lockeTarget, playableLocke, playableSabin, playableTerra, terraAttacker, terraStats)
import Browser
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyUp, onVisibilityChange)
import Bytes exposing (Bytes)
import Canvas exposing (Point, rect, shapes)
import Canvas.Settings as CanvasSettings exposing (fill, stroke)
import Canvas.Settings.Advanced
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Canvas.Texture exposing (Texture, sprite)
import Color
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Set
import Task exposing (Task)
import Vector29
import Vector31
import Zip exposing (Zip)
import Zip.Entry



---- Ports ----


port loadImageURL : String -> Cmd msg


port onImageFromJavaScript : (Decode.Value -> msg) -> Sub msg


port getCanvasBoundingRect : () -> Cmd msg


port onCanvasBoundingRect : (Decode.Value -> msg) -> Sub msg


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
    , canvasBoundingRect : CanvasBoundingRect
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
    | MapDataLoaded { json : String, image : String, png : Bytes, sprites : Sprites }
    | LevelLoaded
        { sprites : Sprites
        , mapImage : Texture
        , offsetX : Float
        , offsetY : Float
        , tiles : World
        , canvasScale : Float
        }


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


type alias World =
    Vector29.Vector29 (Vector31.Vector31 TileType)


defaultWorld : World
defaultWorld =
    Vector31.initializeFromInt (\_ -> NotWalkable)
        |> Vector29.repeat


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
    , canvasBoundingRect = { x = 0, y = 0, width = 0, height = 0 }
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


type TileType
    = Walkable
    | NotWalkable


type Row
    = Row Vector29.Index


type Col
    = Col Vector31.Index


getCell : Row -> Col -> World -> TileType
getCell (Row row) (Col col) world =
    let
        rowVector =
            Vector29.get row world

        tile =
            Vector31.get col rowVector
    in
    tile


setCell : Row -> Col -> TileType -> World -> World
setCell (Row row) (Col col) newValue world =
    let
        rowVector =
            Vector29.get row world

        updatedColVector =
            Vector31.set col newValue rowVector

        updatedRowVector =
            Vector29.set row updatedColVector world
    in
    updatedRowVector


type Msg
    = TogglePause
    | Frame Float
    | TextureLoaded (Maybe Canvas.Texture.Texture)
    | MoveCursor Direction
    | VisibilityChange Visibility
    | LoadLevel
    | LoadedLevel File
    | GotZip (Maybe Zip)
    | ImageLoadedFromJavaScript Decode.Value
    | CanvasBoundingRectLoaded Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        Frame timePassed ->
            case model.gameSetupStatus of
                LevelLoaded { sprites } ->
                    let
                        timePassedInt =
                            round timePassed

                        ( cameraX, cameraY ) =
                            updateCamera model timePassed

                        ( newCharacterFrameTime, newCharacterFrame ) =
                            updateCharacterFrameTime (model.characterFrameTime + timePassedInt) model.characterFrame

                        ( newCharacterX, newCharacterY ) =
                            updateCharacterXAndY model timePassed cameraX cameraY sprites.mainCharacterEast1

                        updatedModel =
                            { model
                                | time = model.time + timePassedInt
                                , cameraX = cameraX
                                , cameraY = cameraY
                                , characterFrameTime = newCharacterFrameTime
                                , characterFrame = newCharacterFrame
                                , characterX = newCharacterX
                                , characterY = newCharacterY
                            }
                    in
                    ( updatedModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
                    ( { model | leftPressed = True }, Cmd.none )

                LeftReleased ->
                    ( { model | leftPressed = False }, Cmd.none )

                RightPressed ->
                    ( { model | rightPressed = True }, Cmd.none )

                RightReleased ->
                    ( { model | rightPressed = False }, Cmd.none )

                UpPressed ->
                    ( { model | upPressed = True }, Cmd.none )

                UpReleased ->
                    ( { model | upPressed = False }, Cmd.none )

                DownPressed ->
                    ( { model | downPressed = True }, Cmd.none )

                DownReleased ->
                    ( { model | downPressed = False }, Cmd.none )

                EnterPressed ->
                    -- let
                    --     datWorldString =
                    --         Array.map
                    --             debugTileRowToString
                    --             model.world
                    --     -- |> Array.foldl
                    --     --     (\rowStr tileTypeString ->
                    --     --         rowStr ++ tileTypeString ++ "\n"
                    --     --     )
                    --     --     ""
                    --     _ =
                    --         Debug.log "datWorldString" datWorldString
                    --     datPath =
                    --         findPath
                    --             straightLineCost
                    --             (\( x, y ) ->
                    --                 Set.singleton ( x + 1, y )
                    --                     |> Set.insert ( x, y + 1 )
                    --                     |> Set.insert ( x - 1, y )
                    --                     |> Set.insert ( x, y - 1 )
                    --             )
                    --             ( 0, 0 )
                    --             ( 2, 0 )
                    --     _ =
                    --         Debug.log "datPath" datPath
                    -- in
                    ( model, Cmd.none )

                EnterReleased ->
                    ( model, Cmd.none )

                WPressed ->
                    let
                        canMoveNorth =
                            case model.characterRow - 1 |> Vector29.intToIndex of
                                Nothing ->
                                    False

                                Just index29 ->
                                    case model.characterCol |> Vector31.intToIndex of
                                        Nothing ->
                                            False

                                        Just index31 ->
                                            getCell (Row index29) (Col index31) model.world == Walkable

                        _ =
                            Debug.log "canMoveNorth" canMoveNorth
                    in
                    if canMoveNorth == True then
                        ( { model | characterRow = model.characterRow - 1, characterFacingDirection = North }, Cmd.none )

                    else
                        ( { model | characterFacingDirection = North }, Cmd.none )

                WReleased ->
                    ( model, Cmd.none )

                APressed ->
                    ( { model | characterFacingDirection = West }, Cmd.none )

                AReleased ->
                    ( model, Cmd.none )

                SPressed ->
                    let
                        canMoveSouth =
                            case model.characterRow + 1 |> Vector29.intToIndex of
                                Nothing ->
                                    False

                                Just index29 ->
                                    case model.characterCol |> Vector31.intToIndex of
                                        Nothing ->
                                            False

                                        Just index31 ->
                                            getCell (Row index29) (Col index31) model.world == Walkable

                        _ =
                            Debug.log "canMoveSouth" canMoveSouth
                    in
                    if canMoveSouth == True then
                        ( { model | characterRow = model.characterRow + 1, characterFacingDirection = South }, Cmd.none )

                    else
                        ( { model | characterFacingDirection = South }, Cmd.none )

                SReleased ->
                    ( model, Cmd.none )

                DPressed ->
                    ( { model | characterFacingDirection = East }, Cmd.none )

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

        LoadLevel ->
            ( model, Select.file [ "application/zip" ] LoadedLevel )

        LoadedLevel file ->
            ( model, file |> File.toBytes |> Task.map Zip.fromBytes |> Task.perform GotZip )

        GotZip Nothing ->
            ( model, Cmd.none )

        GotZip (Just zip) ->
            let
                isMapPNG =
                    \entry -> Zip.Entry.basename entry == "map.png"

                isMapJSON =
                    \entry -> Zip.Entry.basename entry == "map.json"

                mapJSONMaybe =
                    Zip.entries zip
                        |> List.filter isMapJSON
                        |> List.head

                mapPNGMaybe =
                    Zip.entries zip
                        |> List.filter isMapPNG
                        |> List.head
            in
            case mapJSONMaybe of
                Nothing ->
                    ( model, Cmd.none )

                Just mapJSON ->
                    case mapPNGMaybe of
                        Nothing ->
                            ( model, Cmd.none )

                        Just mapPNG ->
                            case Zip.Entry.toString mapJSON of
                                Result.Err mapJSONErr ->
                                    let
                                        _ =
                                            Debug.log "failed convert mapJSON entry to string" mapJSONErr
                                    in
                                    ( model, Cmd.none )

                                Result.Ok jsonString ->
                                    case Zip.Entry.toBytes mapPNG of
                                        Result.Err mapPNGErr ->
                                            let
                                                _ =
                                                    Debug.log "failed convert png entry to bytes" mapPNGErr
                                            in
                                            ( model, Cmd.none )

                                        Result.Ok pngBytes ->
                                            let
                                                imageBase64 =
                                                    Base64Encode.encode (Base64Encode.bytes pngBytes)
                                            in
                                            case model.gameSetupStatus of
                                                SetupComplete sprites ->
                                                    ( { model | gameSetupStatus = MapDataLoaded { json = jsonString, image = imageBase64, png = pngBytes, sprites = sprites } }, loadImageURL imageBase64 )

                                                _ ->
                                                    let
                                                        _ =
                                                            Debug.log "order of oerations failure, can't load things if we don't have sprites" False
                                                    in
                                                    ( model, Cmd.none )

        ImageLoadedFromJavaScript image ->
            case Canvas.Texture.fromDomImage image of
                Nothing ->
                    ( model, Cmd.none )

                Just texture ->
                    let
                        { width, height } =
                            Canvas.Texture.dimensions texture

                        sprite =
                            Canvas.Texture.sprite
                                { x = 0
                                , y = 0
                                , width = width
                                , height = height
                                }
                                texture
                    in
                    case model.gameSetupStatus of
                        MapDataLoaded mapData ->
                            let
                                jsonDoc =
                                    case Decode.decodeString jsonDecoder mapData.json of
                                        Result.Err err ->
                                            let
                                                _ =
                                                    Debug.log "failed to decode json" err
                                            in
                                            { imageOffsetX = 0.0
                                            , imageOffsetY = 0.0
                                            , canvasScale = 1.0
                                            , tiles = [ [] ]
                                            }

                                        Result.Ok parsedJSONDoc ->
                                            let
                                                _ =
                                                    Debug.log "parsed json successfully" parsedJSONDoc
                                            in
                                            parsedJSONDoc
                            in
                            ( { model
                                | gameSetupStatus =
                                    LevelLoaded
                                        { sprites = mapData.sprites
                                        , mapImage = sprite
                                        , offsetX = jsonDoc.imageOffsetX
                                        , offsetY = jsonDoc.imageOffsetY
                                        , tiles = listToWorld jsonDoc.tiles
                                        , canvasScale = jsonDoc.canvasScale
                                        }
                              }
                            , getCanvasBoundingRect ()
                            )

                        _ ->
                            ( model, Cmd.none )

        CanvasBoundingRectLoaded boundingRectValue ->
            let
                boundingRect =
                    getCanvasBoundingRectElseDefault boundingRectValue
            in
            ( { model
                | canvasBoundingRect =
                    { x = boundingRect.x
                    , y = boundingRect.y
                    , width = boundingRect.width
                    , height = boundingRect.height
                    }
              }
            , Cmd.none
            )



---- Update bottom ----


jsonDecoder : Decode.Decoder JSONDecodedDocument
jsonDecoder =
    Decode.map4 JSONDecodedDocument
        (Decode.field "imageOffsetX" Decode.float)
        (Decode.field "imageOffsetY" Decode.float)
        (Decode.field "canvasScale" Decode.float)
        (Decode.field "tiles" (Decode.list (Decode.list Decode.string)))


type alias JSONDecodedDocument =
    { imageOffsetX : Float
    , imageOffsetY : Float
    , canvasScale : Float
    , tiles : List (List String)
    }


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


updateCharacterXAndY : Model -> Float -> Float -> Float -> Canvas.Texture.Texture -> ( Float, Float )
updateCharacterXAndY model timePassed cameraX cameraY characterTexture =
    ( toFloat model.characterCol * 16 + cameraX + (Canvas.Texture.dimensions characterTexture).width / 2
    , toFloat model.characterRow * 16 + cameraY + (Canvas.Texture.dimensions characterTexture).height / 6
    )


updateCharacterFrameTime : Int -> Int -> ( Int, Int )
updateCharacterFrameTime characterFrameTime characterFrame =
    if characterFrameTime > 200 then
        if characterFrame == 0 then
            ( 0, 1 )

        else
            ( 0, 0 )

    else
        ( characterFrameTime, characterFrame )


listToWorld : List (List String) -> Vector29.Vector29 (Vector31.Vector31 TileType)
listToWorld list =
    case Vector29.fromList list of
        Nothing ->
            defaultWorld

        Just ( leftOvers, vector30 ) ->
            if List.length leftOvers > 0 then
                defaultWorld

            else
                Vector29.map
                    (\row ->
                        case Vector31.fromList row of
                            Nothing ->
                                Vector31.repeat NotWalkable

                            Just ( leftOverCols, vector31 ) ->
                                if List.length leftOverCols > 0 then
                                    Vector31.repeat NotWalkable

                                else
                                    Vector31.map
                                        (\tileTypeString ->
                                            case tileTypeString of
                                                "Walkable" ->
                                                    Walkable

                                                _ ->
                                                    NotWalkable
                                        )
                                        vector31
                    )
                    vector30


getCanvasBoundingRectElseDefault : Encode.Value -> CanvasBoundingRect
getCanvasBoundingRectElseDefault boundingRectValue =
    Decode.decodeValue
        decodeBoundingRect
        boundingRectValue
        |> Result.withDefault
            { x = 0
            , y = 0
            , width = 0
            , height = 0
            }


type alias CanvasBoundingRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


decodeBoundingRect : Decode.Decoder CanvasBoundingRect
decodeBoundingRect =
    Decode.map4 CanvasBoundingRect
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)


view : Model -> Html Msg
view model =
    div [ class "flex flex-row" ]
        [ button [ type_ "button", onClick LoadLevel ] [ text "Open File" ]
        , case model.gameSetupStatus of
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

            SetupComplete _ ->
                Canvas.toHtmlWith
                    { width = gameWidth
                    , height = gameHeight
                    , textures = []
                    }
                    []
                    [ Canvas.text
                        [ font { size = 48, family = "sans-serif" }, align Center ]
                        ( 50, 50 )
                        "Sprites loaded, waiting for level..."
                    ]

            MapDataLoaded _ ->
                Canvas.toHtmlWith
                    { width = gameWidth
                    , height = gameHeight
                    , textures = []
                    }
                    []
                    [ Canvas.text
                        [ font { size = 48, family = "sans-serif" }, align Center ]
                        ( 50, 50 )
                        "Sprites and Map JSON loaded, waiting on level image..."
                    ]

            LevelLoaded { sprites, mapImage, offsetX, offsetY, tiles, canvasScale } ->
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
                        -- ++ [ Canvas.texture
                        --         [ Canvas.Settings.Advanced.imageSmoothing False ]
                        --         ( model.cameraX, model.cameraY )
                        --         sprites.towerBase
                        --    ]
                        ++ getCharacterFrame model sprites
                        ++ drawWorld tiles model.cameraX model.cameraY
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


drawWorld : World -> Float -> Float -> List Canvas.Renderable
drawWorld world cameraX cameraY =
    [ Canvas.group
        [ Canvas.Settings.Advanced.alpha 0.5
        , Canvas.Settings.Advanced.transform
            [ Canvas.Settings.Advanced.translate (-8 + cameraX) (-11 + cameraY)
            ]
        ]
        (Vector29.indexedMap
            (\rowIndex row ->
                Vector31.indexedMap
                    (\colIndex cell ->
                        drawCell (Row rowIndex) (Col colIndex) cell
                    )
                    row
                    |> Vector31.toList
                    |> List.concatMap
                        (\cell -> cell)
            )
            world
            |> Vector29.toList
            |> List.concatMap
                (\cell -> cell)
        )
    ]


drawCell : Row -> Col -> TileType -> List Canvas.Renderable
drawCell (Row row) (Col col) tileType =
    let
        rowInt =
            Vector29.indexToInt row

        colInt =
            Vector31.indexToInt col
    in
    [ shapes
        [ if tileType == Walkable then
            CanvasSettings.fill Color.green

          else
            CanvasSettings.fill Color.red
        ]
        [ rect ( Basics.toFloat colInt * 16, Basics.toFloat rowInt * 16 ) 16 16 ]
    , shapes [ CanvasSettings.stroke Color.lightGreen ] [ rect ( Basics.toFloat colInt * 16, Basics.toFloat rowInt * 16 ) 16 16 ]
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
            , onImageFromJavaScript ImageLoadedFromJavaScript
            , onCanvasBoundingRect CanvasBoundingRectLoaded
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
