module UI.FileDrop exposing (Config, Model, Msg, ReturnType(..), init, subscriptions, transparent, update, view)

import DropZone as DZ
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import File exposing (File)
import Html.Attributes as HA
import Json.Decode as D
import Return exposing (Return)
import UI.Button as Button
import UI.Color as Color
import UI.FileDrop.Ports as Ports
import UI.Font as Font
import UI.Input as Input


type alias Model =
    { dzModel : DZ.Model
    , isWindowDraggedOver : Bool
    }


type Msg
    = DzMsg (DZ.DropZoneMessage (List File))
    | WindowDragStart
    | WindowDragEnd


init : Model
init =
    { isWindowDraggedOver = False
    , dzModel = DZ.init
    }


type ReturnType msg
    = Continue Model
    | FilesDropped (List File) (Return msg Model)


update : Msg -> Model -> ReturnType msg
update msg model =
    case msg of
        DzMsg subMsg ->
            let
                newDropZone =
                    DZ.update subMsg model.dzModel

                newModel =
                    { model | dzModel = newDropZone }
            in
            case subMsg of
                DZ.Drop files ->
                    FilesDropped files
                        (newModel
                            |> Return.singleton
                            |> Return.command (Ports.dropHandled ())
                        )

                _ ->
                    Continue newModel

        WindowDragStart ->
            Continue { model | isWindowDraggedOver = True }

        WindowDragEnd ->
            Continue { model | isWindowDraggedOver = False }


decodeFiles : D.Decoder (List File)
decodeFiles =
    D.at [ "dataTransfer", "files" ] <|
        D.oneOrMore (::) File.decoder


transparent : (Msg -> msg) -> Model -> Element msg
transparent toMsg model =
    let
        zoneAttrs =
            DZ.dropZoneEventHandlers decodeFiles
                |> List.map htmlAttribute
                |> List.map (mapAttribute (toMsg << DzMsg))
                |> List.append
                    [ width <| fill
                    , height <| fill
                    ]
    in
    if model.isWindowDraggedOver then
        el zoneAttrs <|
            if DZ.isHovering model.dzModel then
                viewHovering 0.9

            else
                none

    else
        none


viewHovering opacity =
    el
        [ width fill
        , height fill
        , Background.color <| Color.opacify opacity Color.black
        , Font.color Color.white
        ]
    <|
        el [ centerX, centerY, Font.size 30 ] <|
            let
                fire =
                    el
                        [ Font.emoji
                        , Font.size 35
                        ]
                    <|
                        text "🔥"
            in
            row [ spacing 30 ]
                [ fire
                , text "Drop it"
                , fire
                ]


type alias Config =
    { dropLabel : String
    , buttonLabel : String
    , id : String
    }


view : (Msg -> msg) -> Config -> Model -> Element msg
view toMsg config model =
    let
        zoneAttrs =
            DZ.dropZoneEventHandlers decodeFiles
                |> List.map htmlAttribute
                |> List.map (mapAttribute (toMsg << DzMsg))
                |> List.append
                    [ width <| fill
                    , height <| fill
                    ]
    in
    el zoneAttrs <|
        if DZ.isHovering model.dzModel then
            viewHovering 1.0

        else
            column [ centerX, centerY, spacing 10 ] <|
                [ el [ centerX ] <| text config.dropLabel
                , el [ centerX ] <| text "or"
                , el [ centerX ] <|
                    Input.files
                        { label = Button.showAsButton <| text config.buttonLabel
                        , onChange = toMsg << DzMsg << DZ.Drop
                        , id = config.id
                        }
                ]


subscriptions : (Msg -> msg) -> Sub msg
subscriptions toMsg =
    Sub.map toMsg <|
        Sub.batch
            [ Ports.windowDragStart (always WindowDragStart)
            , Ports.windowDragEnd (always WindowDragEnd)
            ]
