port module Main exposing (FacingDirection(..), getCharacterDesiredCol, getCharacterDesiredRow, main)

import AStar exposing (findPath, straightLineCost)
import Animator exposing (color)
import Array exposing (Array)
import Base64.Encode as Base64Encode
import Battle exposing (Attack(..), AttackMissesDeathProtectedTargets(..), BattlePower(..), CharacterStats, Defense(..), Element(..), EquippedRelics, Evade(..), Formation(..), Gold(..), HitPoints(..), HitRate(..), HitResult(..), Item(..), Level(..), MBlock(..), MagicDefense(..), MagicPoints(..), MagicPower(..), Monster(..), MonsterStats, PlayableCharacter, Relic(..), Speed(..), SpellPower(..), Stamina(..), Vigor(..), XP(..), dirk, fireSpell, getDamage, getHit, getRandomNumberFromRange, hitResultToString, lockeStats, lockeTarget, playableLocke, playableSabin, playableTerra, terraAttacker, terraStats)
import Browser
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyUp, onVisibilityChange)
import Bytes exposing (Bytes)
import Canvas exposing (Point, lineTo, moveTo, path, rect, shapes)
import Canvas.Settings as CanvasSettings exposing (fill, stroke)
import Canvas.Settings.Advanced
import Canvas.Settings.Line exposing (LineCap(..), LineJoin(..), lineCap, lineJoin, lineWidth)
import Canvas.Settings.Text exposing (TextAlign(..), align, font, maxWidth)
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
import UUID exposing (UUID)
import Vector29
import Vector31
import World exposing (Col(..), Row(..), TileType(..), World, colToIndex, colToInt, defaultWorld, getCell, indexToCol, indexToRow, intToCol, intToRow, rowToIndex, rowToInt)
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
    , speaking : Bool
    , menuScreenStatus : MenuScreenStatus
    , speechText : String
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
    , characterRow : Row
    , characterCol : Col
    , characterFacingDirection : FacingDirection
    , characterFrameTime : Int
    , characterFrame : Int
    , world : World
    , offsetX : Float
    , offsetY : Float
    , canvasScale : Float
    , canvasBoundingRect : CanvasBoundingRect
    , npcs : Array NPC
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
    | MapDataLoaded
        { json : String
        , image : String
        , png : Bytes
        , sprites : Sprites
        }
    | LevelLoaded
        { sprites : Sprites
        , mapImage : Texture
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


type alias NPC =
    { id : String
    , sprite : Texture
    , row : Row
    , col : Col
    , x : Float
    , y : Float
    , responses : Array String
    , responseIndex : Int
    }


type MenuScreenStatus
    = NoMenuScreen
    | SpeechScreen String
    | Inn InnKeeperStatus
    | WeaponShop ShopStatus
    | PotionShop ShopStatus
    | CharacterConfig CharacterScreenStatus
    | HouseOfLife HouseOfLifeScreenStatus


type InnKeeperStatus
    = InnDecision
    | InnOk


type ShopStatus
    = ShopWaiting
    | ShopSell (List Item)
    | ShopConfirmSell Item
    | ShopSold Item
    | ShopBuy (List Item)
    | ShopBought Item


type alias Item =
    { name : String, durability : Int, gold : Int, itemType : ItemType }


type ItemType
    = Weapon
    | Armor
    | Spellbook
    | Potion


type CharacterScreenStatus
    = CharacterScreenWaiting
    | CharacterScreenAbilities
    | CharacterScreenItems
    | CharacterScreenEquip EquipScreen
    | CharacterScreenSave


type EquipScreen
    = EquipChooseCharacter
    | EquipChooseSlot


type CharacterOrderScreenStatus
    = OrderChooseSpeed
    | OrderChooseCharacter
    | OrderChooseSlot
    | OrderChooseSlotConfirmed


type HouseOfLifeScreenStatus
    = HouseOfLifeChooseCharacter
    | HouseOfLifeConfirm


type BattleScreen
    = BattleScreenWaiting
    | BattleScreenFightOrRun
    | BattleScreenCharacterTurn
    | BattleScreenCharacterTurnChooseTarget
    | BattleScreenMonsterTurn
    | BattleScreenWin
    | BattleScreenLost


initialModel : Random.Seed -> Model
initialModel seed =
    { initialSeed = seed
    , currentSeed = seed
    , paused = False
    , speaking = False
    , speechText = ""
    , menuScreenStatus = NoMenuScreen
    , time = 0
    , gameSetupStatus = SettingUp
    , leftPressed = False
    , rightPressed = False
    , upPressed = False
    , downPressed = False
    , cameraX = 0
    , cameraY = 0
    , characterX = 0
    , characterY = 0
    , characterRow = Row1
    , characterCol = Col1
    , characterFacingDirection = South
    , characterFrameTime = 0
    , characterFrame = 0
    , world = defaultWorld
    , offsetX = 0
    , offsetY = 0
    , canvasScale = 0
    , canvasBoundingRect = { x = 0, y = 0, width = 0, height = 0 }
    , npcs = Array.fromList []
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
    | AButtonPressed
    | AButtonReleased
    | BButtonPressed
    | BButtonReleased
    | StartPressed
    | StartReleased
    | SelectPressed
    | SelectReleased
    | Other


keyDecoderPressed : Decode.Decoder Msg
keyDecoderPressed =
    Decode.map toDirectionPressed (Decode.field "key" Decode.string)


toDirectionPressed : String -> Msg
toDirectionPressed string =
    -- let
    --     _ =
    --         Debug.log "key pressed" string
    -- in
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

        "l" ->
            MoveCursor AButtonPressed

        "k" ->
            MoveCursor BButtonPressed

        "i" ->
            MoveCursor StartPressed

        "j" ->
            MoveCursor SelectPressed

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

        "l" ->
            MoveCursor AButtonPressed

        "k" ->
            MoveCursor BButtonPressed

        "i" ->
            MoveCursor StartPressed

        "j" ->
            MoveCursor SelectPressed

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
    | LoadLevel
    | LoadedLevel File
    | GotZip (Maybe Zip)
    | ImageLoadedFromJavaScript Decode.Value
    | CanvasBoundingRectLoaded Decode.Value
    | Talk



---- Update top ----


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

                        -- ( cameraX, cameraY ) =
                        --     updateCamera model timePassed
                        ( newCharacterFrameTime, newCharacterFrame ) =
                            updateCharacterFrameTime (model.characterFrameTime + timePassedInt) model.characterFrame

                        ( newCharacterX, newCharacterY ) =
                            -- updateCharacterXAndY model timePassed cameraX cameraY sprites.mainCharacterEast1
                            updateCharacterXAndYNoCamera model timePassed sprites.mainCharacterEast1

                        -- (cameraX, cameraY) =
                        --     newCharacterX -
                        updatedNPCs =
                            updateNPCsXY model.npcs

                        updatedModel =
                            { model
                                | time = model.time + timePassedInt
                                , cameraX = -newCharacterX
                                , cameraY = -newCharacterY
                                , characterFrameTime = newCharacterFrameTime
                                , characterFrame = newCharacterFrame
                                , characterX = newCharacterX
                                , characterY = newCharacterY
                                , npcs = updatedNPCs
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

                AButtonPressed ->
                    let
                        _ =
                            Debug.log "npc" (getTargetNPCCharacterIsFacing model)
                    in
                    if model.speaking == False then
                        case getTargetNPCCharacterIsFacing model of
                            Nothing ->
                                ( model, Cmd.none )

                            Just npcFound ->
                                let
                                    ( updatedNPC, speechText ) =
                                        getNextNPCSpeechText npcFound
                                in
                                ( { model
                                    | speaking = True
                                    , speechText = speechText
                                    , npcs = updateNPCs updatedNPC model.npcs
                                  }
                                , Cmd.none
                                )

                    else
                        ( { model | speaking = False }, Cmd.none )

                AButtonReleased ->
                    ( model, Cmd.none )

                BButtonPressed ->
                    if model.menuScreenStatus == CharacterConfig CharacterScreenWaiting then
                        ( { model | menuScreenStatus = NoMenuScreen }, Cmd.none )

                    else
                        ( model, Cmd.none )

                BButtonReleased ->
                    ( model, Cmd.none )

                StartPressed ->
                    if model.menuScreenStatus == NoMenuScreen then
                        ( { model | menuScreenStatus = CharacterConfig CharacterScreenWaiting }, Cmd.none )

                    else
                        ( model, Cmd.none )

                StartReleased ->
                    ( model, Cmd.none )

                SelectPressed ->
                    ( model, Cmd.none )

                SelectReleased ->
                    ( model, Cmd.none )

                WPressed ->
                    let
                        desiredRow =
                            getCharacterDesiredRow North model.characterRow
                                |> Maybe.withDefault model.characterRow

                        _ =
                            Debug.log "characterRow, desiredRow" ( model.characterRow, desiredRow )
                    in
                    if canMove North model.characterRow model.characterCol model.world == True then
                        ( { model | characterRow = desiredRow, characterFacingDirection = North }, Cmd.none )

                    else
                        ( { model | characterFacingDirection = North }, Cmd.none )

                WReleased ->
                    ( model, Cmd.none )

                APressed ->
                    let
                        desiredCol =
                            getCharacterDesiredCol West model.characterCol
                                |> Maybe.withDefault model.characterCol
                    in
                    if canMove West model.characterRow model.characterCol model.world == True then
                        ( { model | characterCol = desiredCol, characterFacingDirection = West }, Cmd.none )

                    else
                        ( { model | characterFacingDirection = West }, Cmd.none )

                AReleased ->
                    ( model, Cmd.none )

                SPressed ->
                    let
                        desiredRow =
                            getCharacterDesiredRow South model.characterRow
                                |> Maybe.withDefault model.characterRow
                    in
                    if canMove South model.characterRow model.characterCol model.world == True then
                        ( { model | characterRow = desiredRow, characterFacingDirection = South }, Cmd.none )

                    else
                        ( { model | characterFacingDirection = South }, Cmd.none )

                SReleased ->
                    ( model, Cmd.none )

                DPressed ->
                    let
                        desiredCol =
                            getCharacterDesiredCol East model.characterCol
                                |> Maybe.withDefault model.characterCol
                    in
                    if canMove East model.characterRow model.characterCol model.world == True then
                        ( { model | characterCol = desiredCol, characterFacingDirection = East }, Cmd.none )

                    else
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
                            case listToWorld jsonDoc.tiles of
                                Err reason ->
                                    let
                                        _ =
                                            Debug.log "listToWorld failed" reason
                                    in
                                    ( model, Cmd.none )

                                Ok parsedWorld ->
                                    let
                                        ( uuidv4, newSeed ) =
                                            uuidV4 model.currentSeed

                                        ( uuidv4Again, newSeedAgain ) =
                                            uuidV4 newSeed
                                    in
                                    ( { model
                                        | currentSeed = newSeedAgain
                                        , gameSetupStatus =
                                            LevelLoaded
                                                { sprites = mapData.sprites
                                                , mapImage = sprite
                                                }
                                        , world = parsedWorld
                                        , offsetX = jsonDoc.imageOffsetX
                                        , offsetY = jsonDoc.imageOffsetY
                                        , canvasScale = jsonDoc.canvasScale
                                        , npcs =
                                            Array.fromList
                                                [ { id = uuidv4
                                                  , sprite = mapData.sprites.mainCharacterSouth1
                                                  , row = Row4
                                                  , col = Col1
                                                  , x = 0
                                                  , y = 0
                                                  , responses = Array.fromList [ "Gen-Bu has hidden\nthe key to the\ndoor in the Statue\n of Hero." ]
                                                  , responseIndex = 0
                                                  }
                                                , { id = uuidv4Again
                                                  , sprite = mapData.sprites.mainCharacterSouth1
                                                  , row = Row8
                                                  , col = Col4
                                                  , x = 0
                                                  , y = 0
                                                  , responses = Array.fromList [ "This tower leads\nto Paradise." ]
                                                  , responseIndex = 0
                                                  }
                                                ]
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

        Talk ->
            ( { model | paused = True, speaking = True }, Cmd.none )



---- Update bottom ----


uuidV4 : Random.Seed -> ( String, Random.Seed )
uuidV4 currentSeed =
    let
        ( num, newSeed ) =
            Random.step UUID.generator currentSeed
    in
    ( UUID.toRepresentation UUID.Urn num, newSeed )


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


canMove : FacingDirection -> Row -> Col -> World -> Bool
canMove facingDirection characterRow characterCol world =
    let
        desiredRow =
            getCharacterDesiredRow facingDirection characterRow

        _ =
            Debug.log "desiredRow" desiredRow

        desiredCol =
            getCharacterDesiredCol facingDirection characterCol

        _ =
            Debug.log "desiredCol" desiredCol
    in
    case desiredRow of
        Nothing ->
            False

        Just rowIndex ->
            case desiredCol of
                Nothing ->
                    False

                Just colIndex ->
                    getCell rowIndex colIndex world == Walkable


getCharacterDesiredRow : FacingDirection -> Row -> Maybe Row
getCharacterDesiredRow facingDirection characterRow =
    case facingDirection of
        North ->
            characterRow
                |> rowToInt
                |> (\value ->
                        value - 1
                   )
                |> intToRow

        South ->
            characterRow
                |> rowToInt
                |> (\value ->
                        value + 1
                   )
                |> intToRow

        _ ->
            Just characterRow


getCharacterDesiredCol : FacingDirection -> Col -> Maybe Col
getCharacterDesiredCol facingDirection characterCol =
    case facingDirection of
        East ->
            characterCol
                |> colToInt
                |> (\value ->
                        value + 1
                   )
                |> intToCol

        West ->
            characterCol
                |> colToInt
                |> (\value ->
                        value - 1
                   )
                |> intToCol

        _ ->
            Just characterCol


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
    ( toFloat (model.characterCol |> colToInt) * 16 + cameraX + (Canvas.Texture.dimensions characterTexture).width / 2 - 8
    , toFloat (model.characterRow |> rowToInt) * 16 + cameraY + (Canvas.Texture.dimensions characterTexture).height / 6 - 8
    )


updateCharacterXAndYNoCamera : Model -> Float -> Canvas.Texture.Texture -> ( Float, Float )
updateCharacterXAndYNoCamera model timePassed characterTexture =
    ( toFloat (model.characterCol |> colToInt) * 16 + (Canvas.Texture.dimensions characterTexture).width / 2 - 8
    , toFloat (model.characterRow |> rowToInt) * 16 + (Canvas.Texture.dimensions characterTexture).height / 6 - 8
    )


updateNPCsXY : Array NPC -> Array NPC
updateNPCsXY npcs =
    Array.map
        (\npc ->
            { npc
                | x = toFloat (npc.col |> colToInt) * 16 + (Canvas.Texture.dimensions npc.sprite).width / 2 - 8
                , y = toFloat (npc.row |> rowToInt) * 16 + (Canvas.Texture.dimensions npc.sprite).width / 2 - 8
            }
        )
        npcs


updateCharacterFrameTime : Int -> Int -> ( Int, Int )
updateCharacterFrameTime characterFrameTime characterFrame =
    if characterFrameTime > 200 then
        if characterFrame == 0 then
            ( 0, 1 )

        else
            ( 0, 0 )

    else
        ( characterFrameTime, characterFrame )


listToWorld : List (List String) -> Result String World
listToWorld list =
    case Vector29.fromList list of
        Nothing ->
            Err "1. Vector29.fromList list failed."

        Just ( leftOvers, vector31 ) ->
            if List.length leftOvers > 0 then
                Err "2. List.length leftOvers > 0"

            else if
                Vector29.foldl
                    (\row acc ->
                        case Vector31.fromList row of
                            Nothing ->
                                False

                            Just ( leftOverCols, _ ) ->
                                List.length leftOverCols == 0
                    )
                    False
                    vector31
                    == True
            then
                Vector29.indexedMap
                    (\rowIndex row ->
                        case Vector31.fromList row of
                            Nothing ->
                                -- Err "3. Vector31.fromList row"
                                Vector31.repeat NotWalkable

                            Just ( leftOverCols, currentVector31 ) ->
                                if List.length leftOverCols > 0 then
                                    Vector31.repeat NotWalkable

                                else
                                    Vector31.indexedMap
                                        (\colIndex tileTypeString ->
                                            -- let
                                            --     _ =
                                            --         Debug.log "what the" (rowColTileToString (Row rowIndex) (Col colIndex) tileTypeString)
                                            -- in
                                            case tileTypeString of
                                                "Walkable" ->
                                                    Walkable

                                                _ ->
                                                    NotWalkable
                                        )
                                        currentVector31
                    )
                    vector31
                    |> Ok

            else
                Err "Vector31 had parsing problems from JSON"


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


getTargetNPCCharacterIsFacing : Model -> Maybe NPC
getTargetNPCCharacterIsFacing model =
    getCharacterFacingTargetXY
        model.characterRow
        model.characterCol
        model.characterFacingDirection
        |> Maybe.andThen
            (\( targetRow, targetCol ) ->
                Array.filter
                    (\npcItem ->
                        npcItem.row == targetRow && npcItem.col == targetCol
                    )
                    model.npcs
                    |> Array.get 0
            )


getNextNPCSpeechText : NPC -> ( NPC, String )
getNextNPCSpeechText npc =
    let
        responseLength =
            Array.length npc.responses
    in
    if responseLength == 0 then
        ( npc, "ERROR: NPC has no responses." )

    else if responseLength == 1 then
        ( npc
        , Array.get 0 npc.responses
            |> Maybe.withDefault "ERROR: No response found at index."
        )

    else if npc.responseIndex < responseLength - 1 then
        ( { npc
            | responseIndex = npc.responseIndex + 1
          }
        , Array.get npc.responseIndex npc.responses
            |> Maybe.withDefault "ERROR: No response found at index."
        )

    else
        ( { npc
            | responseIndex = 0
          }
        , Array.get npc.responseIndex npc.responses
            |> Maybe.withDefault "ERROR: No response found at index."
        )


updateNPCs : NPC -> Array NPC -> Array NPC
updateNPCs npc npcs =
    Array.map
        (\npcItem ->
            if npcItem.id /= npc.id then
                npcItem

            else
                npc
        )
        npcs


getCharacterFacingTargetXY : Row -> Col -> FacingDirection -> Maybe ( Row, Col )
getCharacterFacingTargetXY row col facingDirection =
    getCharacterDesiredRow facingDirection row
        |> Maybe.andThen
            (\targetRow ->
                getCharacterDesiredCol facingDirection col
                    |> Maybe.andThen
                        (\targetCol ->
                            Just ( targetRow, targetCol )
                        )
            )


view : Model -> Html Msg
view model =
    div
        [ class "flex flex-row overflow-hidden" ]
        [ Canvas.toHtmlWith
            { width = gameWidth
            , height = gameHeight
            , textures = []
            }
            [ class "block pixel-art" ]
            ([ shapes
                [ fill (Color.rgb 0 1 0) ]
                [ rect ( 0, 0 ) gameWidthFloat gameHeightFloat ]
             ]
                ++ characterConfigScreen model
            )
        ]



-- [
-- case model.gameSetupStatus of
--     SettingUp ->
--         Canvas.toHtmlWith
--             { width = gameWidth
--             , height = gameHeight
--             , textures = [ Canvas.Texture.loadFromImageUrl "FFL-TowerBase.png" TextureLoaded ]
--             }
--             []
--             [ Canvas.text
--                 [ font { size = 48, family = "sans-serif" }, align Center ]
--                 ( 50, 50 )
--                 "Loading textures..."
--             ]
--     SetupFailed ->
--         Canvas.toHtmlWith
--             { width = gameWidth
--             , height = gameHeight
--             , textures = []
--             }
--             []
--             [ Canvas.text
--                 [ font { size = 48, family = "sans-serif" }, align Center ]
--                 ( 50, 50 )
--                 "Setup failed."
--             ]
--     SetupComplete _ ->
--         Canvas.toHtmlWith
--             { width = gameWidth
--             , height = gameHeight
--             , textures = []
--             }
--             []
--             [ Canvas.text
--                 [ font { size = 48, family = "sans-serif" }, align Center ]
--                 ( 50, 50 )
--                 "Sprites loaded, waiting for level..."
--             ]
--     MapDataLoaded _ ->
--         Canvas.toHtmlWith
--             { width = gameWidth
--             , height = gameHeight
--             , textures = []
--             }
--             []
--             [ Canvas.text
--                 [ font { size = 48, family = "sans-serif" }, align Center ]
--                 ( 50, 50 )
--                 "Sprites and Map JSON loaded, waiting on level image..."
--             ]
--     LevelLoaded { sprites, mapImage } ->
--         case model.menuScreenStatus of
--             NoMenuScreen ->
--                 Canvas.toHtmlWith
--                     { width = gameWidth
--                     , height = gameHeight
--                     , textures = []
--                     }
--                     -- [ class "block scale-[2] pixel-art" ]
--                     [ class "block pixel-art" ]
--                     ([ shapes
--                         [ fill (Color.rgb 0.85 0.92 1) ]
--                         [ rect ( 0, 0 ) gameWidthFloat gameHeightFloat ]
--                      ]
--                         ++ [ Canvas.group
--                                 [ Canvas.Settings.Advanced.transform
--                                     [ Canvas.Settings.Advanced.translate
--                                         (model.cameraX + 80)
--                                         (model.cameraY + 60)
--                                     ]
--                                 ]
--                                 ([]
--                                     ++ [ Canvas.texture
--                                             [ Canvas.Settings.Advanced.imageSmoothing False ]
--                                             ( -model.offsetX, -model.offsetY )
--                                             mapImage
--                                        ]
--                                     ++ drawWorld model.world model.cameraX model.cameraY
--                                     ++ getCharacterFrame model sprites
--                                     ++ drawNPCs model.npcs sprites
--                                 )
--                            ]
--                         ++ showSpeaking
--                             model.speaking
--                             model.speechText
--                     )
--             CharacterConfig CharacterScreenWaiting ->
--                 Canvas.toHtmlWith
--                     { width = gameWidth
--                     , height = gameHeight
--                     , textures = []
--                     }
--                     [ class "block pixel-art" ]
--                     ([ shapes
--                         [ fill (Color.rgb 0 1 0) ]
--                         [ rect ( 0, 0 ) gameWidthFloat gameHeightFloat ]
--                      ]
--                         ++ characterConfigScreen model
--                     )
--             _ ->
--                 Canvas.toHtmlWith
--                     { width = gameWidth
--                     , height = gameHeight
--                     , textures = []
--                     }
--                     [ class "block pixel-art" ]
--                     [ shapes
--                         [ fill (Color.rgb 1 0 0) ]
--                         [ rect ( 0, 0 ) gameWidthFloat gameHeightFloat ]
--                     ]
-- , div [ class "flex flex-col" ]
--     [ button [ type_ "button", onClick LoadLevel ] [ text "Open File" ]
--     , div [] [ text ("Character X: " ++ (model.characterX |> String.fromFloat)) ]
--     , div [] [ text ("Character Y: " ++ (model.characterY |> String.fromFloat)) ]
--     , div [] [ text ("Camera X: " ++ (model.cameraX |> String.fromFloat)) ]
--     , div [] [ text ("Camera Y: " ++ (model.cameraY |> String.fromFloat)) ]
--     ]
-- ]


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


drawNPCs : Array NPC -> Sprites -> List Canvas.Renderable
drawNPCs npcs sprites =
    Array.foldl
        (\npc acc ->
            acc
                ++ [ Canvas.texture
                        [ Canvas.Settings.Advanced.imageSmoothing False ]
                        ( npc.x, npc.y )
                        sprites.mainCharacterSouth1
                   ]
        )
        []
        npcs


showSpeaking : Bool -> String -> List Canvas.Renderable
showSpeaking speaking speech =
    if speaking == True then
        drawSpeaking speech

    else
        []


drawSpeaking : String -> List Canvas.Renderable
drawSpeaking speech =
    let
        margin =
            6

        fontSize =
            8

        startY =
            gameHeightFloat / 2 + margin

        lines =
            String.split "\n" speech
                |> List.indexedMap
                    (\index str ->
                        Canvas.text
                            [ font { size = fontSize, family = "FinalFantasyAdventureGB-Pixel" }
                            , align Left
                            ]
                            ( 9, fontSize * (toFloat index + 1) + (fontSize * toFloat index) + startY + margin )
                            (String.trim str)
                    )

        textCorner =
            3

        textCorner2 =
            4

        textCorner3 =
            5
    in
    [ shapes
        [ fill (Color.rgb 1 1 1) ]
        [ rect
            ( 0, startY )
            gameWidthFloat
            (gameHeightFloat - startY)
        ]
    , shapes
        [ lineWidth 1
        , lineCap RoundCap
        , stroke (Color.rgb 0 0 0)
        ]
        [ path
            ( 0, startY )
            [ moveTo ( textCorner, startY )
            , lineTo ( gameWidthFloat - textCorner, startY )
            , lineTo ( gameWidthFloat, startY + textCorner )
            , lineTo ( gameWidthFloat, startY + (gameHeightFloat / 2) - margin - textCorner )
            , lineTo ( gameWidthFloat - textCorner, startY + (gameHeightFloat / 2) - margin )
            , lineTo ( textCorner, startY + (gameHeightFloat / 2) - margin )
            , lineTo ( 0, startY + (gameHeightFloat / 2) - margin - textCorner )
            , lineTo ( 0, startY + textCorner )
            , lineTo ( textCorner, startY )
            ]
        ]
    , shapes
        [ lineWidth 1
        , lineCap RoundCap
        , stroke (Color.rgb 0.5 0.5 0.5)
        ]
        [ path
            ( 0, startY )
            [ moveTo ( textCorner2 - 1, startY + 1 )
            , lineTo ( gameWidthFloat - textCorner, startY + 1 )
            , lineTo ( gameWidthFloat - 1, startY + textCorner )
            , lineTo ( gameWidthFloat - 1, startY + (gameHeightFloat / 2) - margin - textCorner )
            , lineTo ( gameWidthFloat - textCorner, startY + (gameHeightFloat / 2) - margin - 1 )
            , lineTo ( textCorner, startY + (gameHeightFloat / 2) - margin - 1 )
            , lineTo ( 1, startY + (gameHeightFloat / 2) - margin - textCorner )
            , lineTo ( 1, startY + textCorner )
            , lineTo ( textCorner2 - 1, startY + 1 )
            ]
        ]
    , shapes
        [ lineWidth 1
        , lineCap RoundCap
        , stroke (Color.rgb 0.3 0.3 0.3)
        ]
        [ path
            ( 0, startY )
            [ moveTo ( textCorner2 - 1, startY + 2 )
            , lineTo ( gameWidthFloat - textCorner, startY + 2 )
            , lineTo ( gameWidthFloat - 2, startY + textCorner )
            , lineTo ( gameWidthFloat - 2, startY + (gameHeightFloat / 2) - margin - textCorner )
            , lineTo ( gameWidthFloat - textCorner, startY + (gameHeightFloat / 2) - margin - 2 )
            , lineTo ( textCorner, startY + (gameHeightFloat / 2) - margin - 2 )
            , lineTo ( 2, startY + (gameHeightFloat / 2) - margin - textCorner )
            , lineTo ( 2, startY + textCorner )
            , lineTo ( textCorner2 - 1, startY + 2 )
            ]
        ]
    ]
        ++ lines



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
            [-- Canvas.Settings.Advanced.translate (-8 + cameraX) (-11 + cameraY)
             --   Canvas.Settings.Advanced.translate cameraX cameraY
            ]
        ]
        (Vector29.indexedMap
            (\rowIndex row ->
                Vector31.indexedMap
                    (\colIndex cell ->
                        drawCell (rowIndex |> indexToRow) (colIndex |> indexToCol) cell
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
drawCell row col tileType =
    let
        rowInt =
            rowToInt row

        colInt =
            colToInt col
    in
    [ shapes
        [ if tileType == Walkable then
            CanvasSettings.fill Color.green

          else
            CanvasSettings.fill Color.red
        ]
        [ rect ( Basics.toFloat colInt * 16, Basics.toFloat rowInt * 16 ) 16 16 ]
    , shapes
        [ CanvasSettings.stroke Color.lightGreen ]
        [ rect
            ( Basics.toFloat colInt * 16
            , Basics.toFloat rowInt * 16
            )
            16
            16
        ]
    ]


characterConfigScreen : Model -> List Canvas.Renderable
characterConfigScreen model =
    let
        windowWidth =
            120.0

        windowHeight =
            40
    in
    [ shapes
        [ fill (Color.rgb 0.1 0.3 0.1) ]
        [ rect
            ( 0, 0 )
            gameWidthFloat
            gameHeightFloat
        ]
    ]
        ++ drawWindow
            20
            20
            windowWidth
            windowHeight
        ++ [ text ( 26, 32 ) "Position"
           , text ( 100, 32 ) "1F"
           , text ( 26, 51 ) "Maximum"
           , text ( 100, 51 ) "1F"
           ]
        ++ drawWindow
            20
            103
            windowWidth
            windowHeight
        ++ [ text ( 26, 120 ) "Abil"
           , text ( 63, 120 ) "Item"
           , text ( 100, 120 ) "Equip"
           , text ( 26, 138 ) "Save"
           , textRight ( 135, 138 ) "142GP"
           ]


drawWindow : Float -> Float -> Float -> Float -> List Canvas.Renderable
drawWindow x y width height =
    [ shapes
        [ fill (Color.rgb 1 1 1) ]
        [ rect
            ( x, y )
            width
            height
        ]
    , shapes
        [ lineWidth 1
        , lineCap RoundCap
        , stroke (Color.rgb 0 0 0)
        ]
        [ path
            ( x, y )
            [ lineTo ( x + width, y )
            , lineTo ( x + width, y + height )
            , lineTo ( x, y + height )
            , lineTo ( x, y )
            ]
        ]
    ]


text : ( Float, Float ) -> String -> Canvas.Renderable
text xy label =
    Canvas.text
        [ font { size = 7, family = "FinalFantasyAdventureGB-Pixel" }
        , align Left
        ]
        xy
        label


textRight : ( Float, Float ) -> String -> Canvas.Renderable
textRight xy label =
    Canvas.text
        [ font { size = 7, family = "FinalFantasyAdventureGB-Pixel" }
        , align Right
        ]
        xy
        label


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel (Random.initialSeed 42), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused == False then
        Sub.batch
            [ onVisibilityChange VisibilityChange
            , onAnimationFrameDelta Frame
            , onKeyDown keyDecoderPressed
            , onKeyUp keyDecoderReleased
            , onImageFromJavaScript ImageLoadedFromJavaScript
            , onCanvasBoundingRect CanvasBoundingRectLoaded
            ]

    else if model.paused == False && model.speaking == True then
        Sub.batch
            [ onVisibilityChange VisibilityChange
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
