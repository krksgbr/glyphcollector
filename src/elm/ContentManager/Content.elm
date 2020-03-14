module ContentManager.Content exposing
    ( Config
    , addMore
    , avg
    , image
    , imagePromise
    , matchedGlyph
    )

import Avg
import ContentManager.Msg as Msg exposing (Msg(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import File exposing (File)
import Html as H
import Html.Attributes as HA
import IPC.Types exposing (Avg, Image, MatchedGlyph, ProjectReq, Req(..))
import Image exposing (Promise(..))
import Project
import UI.Background exposing (PositionX(..), PositionY(..), Size(..))
import UI.Border as Border
import UI.Button as Button
import UI.Color as Color
import UI.Events.Custom as Custom
import UI.Spinner exposing (spinner)


type alias ThumbnailArgs =
    { size : Int
    , isSelected : Bool
    }


type alias Thumbnail i =
    i -> ThumbnailArgs -> Element (Msg i)


type alias Config i =
    { viewThumbnail : Thumbnail i
    , getKey : i -> String
    , onFilesAdded : Maybe (List File -> Cmd (Msg i))
    , onItemsRemoved : List i -> Cmd (Msg i)
    }


viewBackgroundImage : String -> Attribute msg
viewBackgroundImage src =
    UI.Background.image
        (File.pathToUrl src)
        ( CenterX, CenterY )
        Contain


addMore size =
    column
        [ Background.color Color.white
        , Border.width 2
        , Border.rounded 10
        , mouseOver
            [ Background.color Color.black
            , Font.color Color.white
            ]
        , pointer
        ]
        [ el
            [ width <| px (size + 30)
            , height <| px size
            ]
          <|
            el
                [ centerX
                , centerY
                , Font.size 75
                , Font.extraLight
                ]
            <|
                text "+"
        , el
            [ width fill
            , height <| px 30
            , paddingXY 10 0
            ]
          <|
            el [ centerY, centerX ] <|
                text "Add more"
        ]


viewThumbnail :
    { onRemove : msg
    , onClick : msg
    , isSelected : Bool
    , size : Int
    , label : String
    , backgroundAttr : Attribute msg
    }
    -> Element msg
viewThumbnail args =
    column
        [ Background.color Color.white
        , Border.width 2
        , Border.rounded 10
        , onClick args.onClick
        , Font.color <|
            if args.isSelected then
                Color.accent

            else
                Color.black
        ]
        [ el
            [ args.backgroundAttr
            , width <| px (args.size + 30)
            , height <| px args.size
            , Border.bottom 2
            , inFront <|
                if args.isSelected then
                    el
                        [ Background.color <| Color.opacify 0.1 Color.accent
                        , width <| px (args.size + 30)
                        , height <| px args.size

                        --                        , Border.rounded 10
                        ]
                    <|
                        none

                else
                    none
            ]
          <|
            none
        , row
            [ paddingXY 10 0
            , height <| px 30
            , spaceEvenly
            , width fill
            ]
            [ html <|
                H.div
                    [ HA.style "white-space" "nowrap"
                    , HA.style "overflow" "hidden"
                    , HA.style "text-overflow" "ellipsis"
                    , HA.style "width" <| String.fromInt args.size ++ "px"
                    ]
                    [ H.text args.label ]
            , Button.text "x"
                [ Custom.onClick
                    { message = args.onRemove
                    , stopPropagation = True
                    , preventDefault = False
                    }
                ]
            ]
        ]


avg : Config Avg.Promise
avg =
    let
        thumbnail : Thumbnail Avg.Promise
        thumbnail avgp args =
            let
                { backgroundImage, imageLabel } =
                    case avgp of
                        Avg.Pending fileName ->
                            { backgroundImage =
                                inFront <|
                                    spinner
                            , imageLabel = fileName
                            }

                        Avg.Resolved avg_ ->
                            { backgroundImage = viewBackgroundImage avg_.image.thumbnail
                            , imageLabel = avg_.image.name
                            }
            in
            viewThumbnail
                { backgroundAttr = backgroundImage
                , label = imageLabel
                , size = args.size
                , isSelected = args.isSelected
                , onClick = ItemClicked avgp
                , onRemove = ItemsRemoved [ avgp ]
                }
    in
    { viewThumbnail = thumbnail
    , getKey =
        \avgp ->
            case avgp of
                Avg.Pending fileName ->
                    fileName

                Avg.Resolved avg_ ->
                    avg_.image.id
    , onFilesAdded = Nothing
    , onItemsRemoved = Project.deleteAvgs
    }


image : { deleteFiles : List Image -> Cmd (Msg Image) } -> Config Image
image reqs =
    let
        thumbnail : Thumbnail Image
        thumbnail img args =
            viewThumbnail
                { backgroundAttr = viewBackgroundImage img.thumbnail
                , label = img.name
                , size = args.size
                , isSelected = args.isSelected
                , onClick = ItemClicked img
                , onRemove = ItemsRemoved [ img ]
                }
    in
    { viewThumbnail = thumbnail
    , getKey = .id
    , onFilesAdded = Nothing
    , onItemsRemoved = reqs.deleteFiles
    }


matchedGlyph : Config MatchedGlyph
matchedGlyph =
    let
        thumbnail : Thumbnail MatchedGlyph
        thumbnail mg args =
            viewThumbnail
                { backgroundAttr = viewBackgroundImage mg.image.thumbnail
                , label = mg.image.name
                , size = args.size
                , isSelected = args.isSelected
                , onClick = ItemClicked mg
                , onRemove = ItemsRemoved [ mg ]
                }
    in
    { viewThumbnail = thumbnail
    , getKey = .image >> .id
    , onFilesAdded = Nothing
    , onItemsRemoved = Project.deleteMatchedGlyphs
    }


imagePromise :
    { importFiles : List File -> Cmd (Msg Promise)
    , deleteFiles : List Promise -> Cmd (Msg Promise)
    }
    -> Config Promise
imagePromise reqs =
    let
        thumbnail : Thumbnail Promise
        thumbnail iimg args =
            let
                { backgroundImage, imageLabel } =
                    case iimg of
                        Pending { fileName } ->
                            { backgroundImage =
                                inFront <|
                                    spinner
                            , imageLabel = fileName
                            }

                        Resolved img ->
                            { backgroundImage = viewBackgroundImage img.thumbnail
                            , imageLabel = img.name
                            }
            in
            viewThumbnail
                { backgroundAttr = backgroundImage
                , label = imageLabel
                , size = args.size
                , isSelected = args.isSelected
                , onClick = ItemClicked iimg
                , onRemove = ItemsRemoved [ iimg ]
                }
    in
    { viewThumbnail = thumbnail
    , getKey =
        \iimg ->
            case iimg of
                Pending { fileName } ->
                    fileName

                Resolved { id } ->
                    id
    , onFilesAdded = Just reqs.importFiles
    , onItemsRemoved = reqs.deleteFiles
    }
