module ContentManager exposing
    ( FileInput(..)
    , Model
    , ViewConfig
    , closeContextMenu
    , init
    , selection
    , subscriptions
    , update
    , view
    , selectNone)

import ContentManager.Content as Content
import ContentManager.Msg exposing (Msg(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (labelHidden)
import Html.Attributes as HA
import Math
import Return exposing (Return)
import UI.Attributes exposing (height_, vh, vw, width_)
import UI.Border as Border
import UI.Button as Button
import UI.Color as Color
import UI.Events.Custom as Custom
import UI.FileDrop as FileDrop
import UI.Input as Input
import UI.Layout as Layout


type alias Model i =
    { zoom : Float
    , fileDrop : FileDrop.Model
    , selected : Dict String i
    , showContextMenu : Bool
    }


init : Model i
init =
    { zoom = 0
    , fileDrop = FileDrop.init
    , selected = Dict.empty
    , showContextMenu = False
    }


selectNone model =
    { model | selected = Dict.empty }


-- TODO solve this another way


closeContextMenu model =
    { model | showContextMenu = False }


update : Content.Config i -> Msg i -> Model i -> Return (Msg i) (Model i)
update contentConfig msg model =
    case msg of
        ItemClicked item ->
            let
                itemKey =
                    contentConfig.getKey item

                updateSelected =
                    if Dict.member itemKey model.selected then
                        Dict.remove itemKey

                    else
                        Dict.insert itemKey item
            in
            { model | selected = updateSelected model.selected }
                |> Return.singleton

        SelectedAll items ->
            items
                |> List.map (\item -> ( contentConfig.getKey item, item ))
                |> Dict.fromList
                |> (\newSelected -> { model | selected = newSelected })
                |> Return.singleton

        SelectedNone ->
            { model | selected = Dict.empty }
                |> Return.singleton

        FileInputChanged files ->
            model
                |> Return.singleton
                |> Return.command
                    (contentConfig.onFilesAdded
                        |> Maybe.map (\addFiles -> addFiles files)
                        |> Maybe.withDefault Cmd.none
                    )

        FileDropMsg subMsg ->
            case FileDrop.update subMsg model.fileDrop of
                FileDrop.Continue newModel ->
                    { model | fileDrop = newModel }
                        |> Return.singleton

                FileDrop.FilesDropped droppedFiles fdReturn ->
                    fdReturn
                        |> Return.map
                            (\newModel ->
                                { model | fileDrop = newModel }
                            )
                        |> Return.command
                            (contentConfig.onFilesAdded
                                |> Maybe.map (\addFiles -> addFiles droppedFiles)
                                |> Maybe.withDefault Cmd.none
                            )

        ZoomChanged newZoom ->
            { model | zoom = newZoom }
                |> Return.singleton

        ItemsRemoved is ->
            model
                |> Return.singleton
                |> Return.command (contentConfig.onItemsRemoved is)

        OpenContextMenu ->
            { model | showContextMenu = True }
                |> Return.singleton

        CloseContextMenu ->
            { model | showContextMenu = False }
                |> Return.singleton

        DeleteAll ->
            { model | selected = Dict.empty, showContextMenu = False }
                |> Return.singleton
                |> Return.command
                    (contentConfig.onItemsRemoved <| selection model)


type FileInput
    = FileInputDisabled (Element Never)
    | FileInputEnabled FileDrop.Config


type alias ViewConfig i msg =
    { toMsg : Msg i -> msg
    , items : List i
    , contentConfig : Content.Config i
    , title : String
    , fileInput : FileInput
    , contextMenuItems : List ( String, msg )
    }


viewEmpty : FileInput -> Model i -> Element (Msg i)
viewEmpty fileInput model =
    column
        [ width fill
        , height fill
        ]
        [ case fileInput of
            FileInputDisabled empty ->
                empty
                    |> Element.map never
                    |> el [ centerX, centerY ]

            FileInputEnabled fileDropConfig ->
                FileDrop.view FileDropMsg fileDropConfig model.fileDrop
        ]


header : List (Element msg) -> Element msg
header =
    row
        [ width fill
        , paddingXY Layout.global.paddingX 0
        , Border.bottom 2
        , spacing 30
        , height <| px <| Layout.contentManager.headerHeight
        , Background.color Color.white
        ]


zoomSteps : Float
zoomSteps =
    5


selection : Model i -> List i
selection model =
    model.selected
        |> Dict.toList
        |> List.map Tuple.second


viewAddMore : FileInput -> Int -> List (Element (Msg i))
viewAddMore fileInputConfig thumbnailSize =
    case fileInputConfig of
        FileInputEnabled { id } ->
            [ Input.files
                { label = Content.addMore thumbnailSize
                , onChange = FileInputChanged
                , id = id
                }
            ]

        FileInputDisabled _ ->
            []


css k v =
    htmlAttribute <| HA.style k v


viewContextMenu : List ( String, msg ) -> (Msg i -> msg) -> Element msg
viewContextMenu userItems toMsg =
    let
        items =
            ( "Delete selected", toMsg DeleteAll )
                :: userItems

        viewItem ix ( label, msg ) =
            el
                ([ onClick msg
                 , mouseOver
                    [ Background.color Color.accent
                    , Font.color Color.white
                    ]
                 , pointer
                 , padding 10
                 , width fill
                 ]
                    ++ (if List.length items == 1 then
                            [ Border.roundEach
                                { topLeft = 10
                                , bottomLeft = 10
                                , bottomRight = 10
                                , topRight = 0
                                }
                            ]

                        else if ix == 0 then
                            [ Border.roundEach
                                { topLeft = 10
                                , bottomLeft = 0
                                , bottomRight = 0
                                , topRight = 0
                                }
                            ]

                        else if ix == (List.length items - 1) && List.length items > 1 then
                            [ Border.roundEach
                                { topLeft = 0
                                , bottomLeft = 10
                                , bottomRight = 10
                                , topRight = 0
                                }
                            ]

                        else
                            []
                       )
                )
            <|
                text label
    in
    column
        [ Font.size 15
        , Background.color <| Color.black
        , Font.color <| Color.white
        , Border.roundEach
            { topLeft = 10
            , bottomLeft = 10
            , bottomRight = 10
            , topRight = 0
            }
        , width <| px 300
        , moveLeft 280
        , moveUp 30
        ]
    <|
        (items
            |> List.indexedMap viewItem
            |> List.intersperse
                (el [ Border.bottom 1, width fill ] none)
        )


view : Model i -> ViewConfig i msg -> Element msg
view model config =
    column
        [ width fill
        , height fill
        , inFront <|
            Element.map config.toMsg <|
                if model.showContextMenu then
                    el
                        [ onClick <| CloseContextMenu
                        , width_ <| vw 100
                        , height_ <| vh 100
                        , css "position" "fixed"
                        , css "top" "0"
                        , css "left" "0"
                        , css "z-index" "10"
                        ]
                        none

                else
                    none
        ]
        [ header <|
            case config.items of
                [] ->
                    [ text config.title ]

                _ ->
                    [ text config.title
                    , Element.map config.toMsg <|
                        Input.slider
                            { onChange = ZoomChanged
                            , label = labelHidden "zoom"
                            , min = 0
                            , max = zoomSteps
                            , value = model.zoom
                            , step = Just 0.1
                            }
                    , row [ spacing 10 ]
                        [ Element.map config.toMsg <|
                            Button.default
                                { onPress = Just <| SelectedAll config.items
                                , label = text "select all"
                                }
                        , Element.map config.toMsg <|
                            Button.default
                                { onPress = Just SelectedNone
                                , label = text "select none"
                                }
                        , el
                            [ below <|
                                if model.showContextMenu then
                                    viewContextMenu config.contextMenuItems config.toMsg

                                else
                                    none
                            ]
                          <|
                            Element.map config.toMsg <|
                                el
                                    [ Font.size 18
                                    , Font.extraLight
                                    , css "transform" "rotate(90deg)"
                                    ]
                                <|
                                    Button.text "•••"
                                        [ Custom.onClick
                                            { message = OpenContextMenu
                                            , stopPropagation = True
                                            , preventDefault = False
                                            }
                                        ]
                        ]
                    ]
        , case config.items of
            [] ->
                Element.map config.toMsg <|
                    viewEmpty config.fileInput model

            _ ->
                Element.map config.toMsg <|
                    let
                        minThumbnail =
                            100

                        maxThumbnail =
                            500

                        thumbnailSize =
                            round <|
                                Math.scale
                                    0
                                    zoomSteps
                                    minThumbnail
                                    maxThumbnail
                                    model.zoom
                    in
                    el
                        [ clipY
                        , width fill
                        , height fill
                        , htmlAttribute <|
                            let
                                heightPx =
                                    (Layout.contentManager.headerHeight
                                        + Layout.workspace.footerHeight
                                        + Layout.workspace.headerHeight
                                        |> String.fromInt
                                    )
                                        ++ "px"
                            in
                            HA.style "height" <| String.join " " [ "calc(100vh -", heightPx, ")" ]
                        , scrollbars
                        , inFront <| FileDrop.transparent FileDropMsg model.fileDrop
                        ]
                    <|
                        (config.items
                            |> List.map
                                (\item ->
                                    config.contentConfig.viewThumbnail item
                                        { size = thumbnailSize
                                        , isSelected =
                                            Dict.member
                                                (config.contentConfig.getKey item)
                                                model.selected
                                        }
                                )
                            |> (\thumbs -> thumbs ++ viewAddMore config.fileInput thumbnailSize)
                            |> wrappedRow
                                [ spacing 10
                                , padding <| Layout.global.paddingX
                                , scrollbarY
                                , width fill
                                ]
                        )
        ]


subscriptions : (Msg i -> msg) -> Sub msg
subscriptions toMsg =
    Sub.map toMsg <|
        FileDrop.subscriptions FileDropMsg
