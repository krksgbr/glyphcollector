module Workspace exposing (Model, Msg(..), init, subscriptions, update, view)

import Avg
import ContentManager as CM
import ContentManager.Content as CM
import ContentManager.Msg as CM
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import File
import Html.Attributes as HA
import IPC.Types
    exposing
        ( Avg
        , GlyphCollection
        , Image
        , MatchedGlyph
        , ProjectModel
        , ProjectReq(..)
        , ProjectView(..)
        , Req(..)
        , TMStatus
        )
import Image
import Project
import Return exposing (Return)
import Return.Extra as Return
import UI.Attributes exposing (vh, width_)
import UI.Border as Border
import UI.Button as Button
import UI.Color as Color
import UI.Cursor as Cursor
import UI.Layout as Layout
import UI.Popup as Popup
import UI.Spinner exposing (spinner)


type alias Model =
    { templateCM : CM.Model Image.Promise
    , sourceCM : CM.Model Image.Promise
    , collectionsCM : CM.Model MatchedGlyph
    , avgCM : CM.Model Avg.Promise
    , showFeedBack : Bool
    }


type ImageType
    = Template
    | Source
    | TMResult
    | Avg


type Msg
    = TemplateCMMsg (CM.Msg Image.Promise)
    | SourceCMMsg (CM.Msg Image.Promise)
    | CollectionsCMMsg (CM.Msg MatchedGlyph)
    | AvgCMMsg (CM.Msg Avg.Promise)
    | CollectGlyphs
    | CancelCollectGlyphs
    | GenerateAvgs
    | ReqSetView ProjectView
    | Close
    | ShowCollectionsDirectory String
    | ShowAvgsDirectory String
    | ShowFeedBack
    | HideFeedBack


init : Model
init =
    { templateCM = CM.init
    , sourceCM = CM.init
    , collectionsCM = CM.init
    , avgCM = CM.init
    , showFeedBack = False
    }


updateImageManager toMsg subMsg subModel config =
    CM.update config subMsg subModel
        |> Return.mapCmd toMsg


templateCMConfig =
    CM.imagePromise
        { importFiles = Project.importTemplates
        , deleteFiles = Project.deleteTemplates
        }


sourceCMConfig =
    CM.imagePromise
        { importFiles = Project.importSources
        , deleteFiles = Project.deleteSources
        }


collectionsCMConfig =
    CM.matchedGlyph


avgCMConfig =
    CM.avg


getSelection model =
    { templates =
        CM.selection model.templateCM
            |> Image.catResolved
            |> List.sortBy .name
    , sources =
        CM.selection model.sourceCM
            |> Image.catResolved
            |> List.sortBy .name
    , collection =
        CM.selection model.collectionsCM
    }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        TemplateCMMsg subMsg ->
            CM.update
                templateCMConfig
                subMsg
                model.templateCM
                |> Return.mapBoth TemplateCMMsg
                    (\newModel ->
                        { model
                            | templateCM = newModel
                        }
                    )

        SourceCMMsg subMsg ->
            CM.update
                sourceCMConfig
                subMsg
                model.sourceCM
                |> Return.mapBoth SourceCMMsg
                    (\newModel ->
                        { model
                            | sourceCM = newModel
                        }
                    )

        CollectionsCMMsg subMsg ->
            CM.update
                collectionsCMConfig
                subMsg
                model.collectionsCM
                |> Return.mapBoth CollectionsCMMsg
                    (\newModel ->
                        { model
                            | collectionsCM = newModel
                        }
                    )

        AvgCMMsg subMsg ->
            CM.update
                avgCMConfig
                subMsg
                model.avgCM
                |> Return.mapBoth AvgCMMsg
                    (\newModel ->
                        { model
                            | avgCM = newModel
                        }
                    )

        CollectGlyphs ->
            let
                selection =
                    getSelection model
            in
            model
                |> Return.singleton
                |> Return.command
                    (Project.matchTemplate
                        { templates = selection.templates
                        , sources = selection.sources
                        }
                    )

        CancelCollectGlyphs ->
            model
                |> Return.singleton
                |> Return.command Project.cancelTemplateMatching

        GenerateAvgs ->
            let
                selected =
                    CM.selection model.collectionsCM
            in
            model
                |> Return.singleton
                |> Return.command (Project.genAvg selected)

        ReqSetView projectView ->
            let
                newModel =
                    case projectView of
                        Collections _ ->
                            { model
                                | collectionsCM =
                                    CM.selectNone model.collectionsCM
                                , avgCM = CM.selectNone model.avgCM
                            }

                        _ ->
                            model
            in
            newModel
                |> Return.singleton
                |> Return.command
                    (Project.setView projectView)

        Close ->
            init
                |> Return.singleton
                |> Return.sendReq UnloadProject

        ShowAvgsDirectory glyphName ->
            -- TODO solve this another way
            { model | avgCM = CM.closeContextMenu model.avgCM }
                |> Return.singleton
                |> Return.command (Project.showAvgsDirectory glyphName)

        ShowCollectionsDirectory glyphName ->
            { model
                | collectionsCM =
                    CM.closeContextMenu model.collectionsCM
            }
                |> Return.singleton
                |> Return.command
                    (Project.showCollectionsDirectory glyphName)

        ShowFeedBack ->
            { model | showFeedBack = True }
                |> Return.singleton

        HideFeedBack ->
            { model | showFeedBack = False }
                |> Return.singleton


frame : ProjectModel -> Bool -> Element Msg -> Element Msg
frame project showFeedBack e =
    let
        maybeNavToCollections =
            project.imP.collections
                |> List.head
                |> Maybe.map (Collections << .glyphName)
                |> Maybe.map ReqSetView

        activeRouteAttrs =
            [ Font.underline ]
    in
    column
        [ width fill
        , height fill
        , inFront <|
            if showFeedBack then
                let
                    lines =
                        [ "Thanks for trying out GlyphCollector. "
                        , "If you'd like to provide feedback, please send an email to "
                        , "krks.gbr@gmail.com"
                        ]
                            |> List.map
                                (\t ->
                                    paragraph [ htmlAttribute <| HA.class "selectable" ]
                                        [ text t
                                        ]
                                )
                            |> column [ spacing 10 ]
                in
                Popup.view
                    lines
                    { onDismissed = HideFeedBack }

            else
                none
        ]
        [ row
            [ width fill
            , height <| px Layout.workspace.headerHeight
            , Background.color Color.black
            , Font.color Color.white
            , spacing 30
            , paddingXY Layout.global.paddingX 0
            ]
            ([ el [ pointer, onClick Close ] <| text "Home"
             , el [ Font.size 20 ] <| text project.name
             , el
                ([ pointer
                 , onClick <| ReqSetView Sources
                 ]
                    ++ (case project.view of
                            Sources ->
                                activeRouteAttrs

                            _ ->
                                []
                       )
                )
               <|
                text "Sources"
             , case maybeNavToCollections of
                Just navtoCollections ->
                    el
                        ([ pointer, onClick navtoCollections ]
                            ++ (case project.view of
                                    Collections _ ->
                                        activeRouteAttrs

                                    _ ->
                                        []
                               )
                        )
                    <|
                        text "Collections"

                Nothing ->
                    el
                        [ pointer
                        , Font.color <| rgb 0.6 0.6 0.6
                        , Cursor.notAllowed
                        ]
                    <|
                        text "Results"
             ]
                |> List.intersperse (el [] <| text ">")
                |> (\items ->
                        items
                            ++ [ Button.text "Feedback" [ alignRight, onClick <| ShowFeedBack ]
                               ]
                   )
            )
        , e
        ]


viewFooter children =
    row
        [ Border.top 2
        , height <| px Layout.workspace.footerHeight
        , width fill
        , paddingXY Layout.global.paddingX 0
        ]
        [ row [ alignRight ] children
        ]


getTMStatus : ProjectModel -> Maybe TMStatus
getTMStatus project =
    project.imP.process
        |> Maybe.andThen
            (\{ status } ->
                status
            )


view :
    ProjectModel
    -> Model
    -> Element Msg
view project model =
    frame project model.showFeedBack <|
        case getTMStatus project of
            Just tmStatus ->
                column
                    [ width fill
                    , height fill
                    ]
                    [ el
                        [ height <| px 30
                        , Background.color Color.accent
                        , width_ <| vh (100 * tmStatus.pct)
                        ]
                      <|
                        none
                    , column [ height fill, width fill, padding 50 ]
                        [ column [ Font.size 30, centerY, centerX, spacing 50 ]
                            [ el [ centerX ] <| text "Collecting glyphs..."
                            , spinner
                            , row [ spacing 50 ]
                                [ image [ width <| px 300 ]
                                    { src = File.pathToUrl tmStatus.source.thumbnail
                                    , description = ""
                                    }
                                , image [ height <| px 150 ]
                                    { src = File.pathToUrl tmStatus.template.thumbnail
                                    , description = ""
                                    }
                                ]
                            ]
                        , row [ alignRight ]
                            [ Button.custom
                                { size = Button.Normal
                                , color = Button.Accent
                                }
                                { onPress = Just CancelCollectGlyphs
                                , label = text "Cancel"
                                }
                            ]
                        ]
                    ]

            Nothing ->
                case project.view of
                    Sources ->
                        column [ width fill, height fill ]
                            [ row [ width fill, height fill ]
                                [ el [ width fill, height fill, Border.right 2 ] <|
                                    CM.view model.templateCM
                                        { toMsg = TemplateCMMsg
                                        , items = project.templates |> Image.imageStoreToList
                                        , contentConfig = templateCMConfig
                                        , title = "Sample Glyphs"
                                        , contextMenuItems = []
                                        , fileInput =
                                            CM.FileInputEnabled
                                                { id = "templates"
                                                , buttonLabel = "Select them"
                                                , dropLabel = "Drop sample glyphs here"
                                                }
                                        }
                                , CM.view model.sourceCM
                                    { toMsg = SourceCMMsg
                                    , items = project.sources |> Image.imageStoreToList
                                    , contentConfig = sourceCMConfig
                                    , title = "Source documents"
                                    , contextMenuItems = []
                                    , fileInput =
                                        CM.FileInputEnabled
                                            { id = "sources"
                                            , buttonLabel = "Select them"
                                            , dropLabel = "Drop source documents here"
                                            }
                                    }
                                ]
                            , viewFooter
                                [ Button.custom
                                    { size = Button.Normal
                                    , color = Button.Accent
                                    }
                                    { onPress =
                                        let
                                            selection =
                                                getSelection model
                                        in
                                        if List.isEmpty selection.templates || List.isEmpty selection.sources then
                                            Nothing

                                        else
                                            Just CollectGlyphs
                                    , label = text "Collect glyphs"
                                    }
                                ]
                            ]

                    Collections glyphName ->
                        let
                            currentCollection =
                                project.imP.collections
                                    |> List.filter (\c -> c.glyphName == glyphName)
                                    |> List.head

                            chooseGlyphName =
                                project.imP.collections
                                    |> List.map (\c -> c.glyphName)
                                    |> List.indexedMap
                                        (\i gn ->
                                            let
                                                isFirst =
                                                    i == 0

                                                isActive =
                                                    gn == glyphName
                                            in
                                            el
                                                ([ onClick <| ReqSetView <| Collections gn
                                                 , pointer
                                                 , Font.size 20
                                                 , centerX
                                                 , Font.center
                                                 , paddingXY 10 10
                                                 , Border.rounded 10
                                                 , above <|
                                                    if isFirst then
                                                        none

                                                    else
                                                        el
                                                            [ Border.top 1
                                                            , width <| px 10
                                                            , height <| px 0
                                                            , centerX
                                                            ]
                                                        <|
                                                            none
                                                 ]
                                                    ++ (if isActive then
                                                            [ Font.color Color.white
                                                            , Background.color Color.black
                                                            ]

                                                        else
                                                            []
                                                       )
                                                )
                                            <|
                                                text <| String.left 3 gn
                                        )
                        in
                        column [ width fill, height fill ]
                            [ row [ width fill, height fill ]
                                [ el
                                    [ Border.right 2
                                    , height fill
                                    , paddingXY 10 0
                                    , htmlAttribute <|
                                        HA.style "padding-top" "105px"
                                    ]
                                  <|
                                    column
                                        [ centerX
                                        ]
                                    <|
                                        chooseGlyphName
                                , el [ width fill, height fill, Border.right 2 ] <|
                                    CM.view model.collectionsCM
                                        { toMsg = CollectionsCMMsg
                                        , items =
                                            currentCollection
                                                |> Maybe.map .matches
                                                |> Maybe.withDefault []
                                        , contentConfig = collectionsCMConfig
                                        , title = "Collected Glyphs"
                                        , contextMenuItems =
                                            [ ( "Show directory"
                                              , ShowCollectionsDirectory glyphName
                                              )
                                            ]
                                        , fileInput =
                                            CM.FileInputDisabled
                                                (text <|
                                                    String.join ""
                                                        [ "Your collection of "
                                                        , glyphName
                                                        , "'s "
                                                        , "will appear here."
                                                        ]
                                                )
                                        }
                                , CM.view model.avgCM
                                    { toMsg = AvgCMMsg
                                    , items =
                                        currentCollection
                                            |> Maybe.map .averages
                                            |> Maybe.withDefault []
                                            |> List.map Avg.Resolved
                                            |> List.append
                                                (case project.imP.genAvgProcess of
                                                    Just ( _, pendingFileName ) ->
                                                        [ Avg.Pending pendingFileName ]

                                                    Nothing ->
                                                        []
                                                )
                                    , contentConfig = avgCMConfig
                                    , contextMenuItems =
                                        [ ( "Show directory", ShowAvgsDirectory glyphName )
                                        ]
                                    , title = "Averaged Glyphs"
                                    , fileInput =
                                        CM.FileInputDisabled (text "The averages your create will appear here.")
                                    }
                                ]
                            , viewFooter <|
                                [ Button.custom
                                    { size = Button.Normal
                                    , color = Button.Accent
                                    }
                                    { onPress =
                                        let
                                            selection =
                                                getSelection model
                                        in
                                        if List.isEmpty selection.collection then
                                            Nothing

                                        else
                                            Just GenerateAvgs
                                    , label = text "Generate Average"
                                    }
                                ]
                            ]


subscriptions : Sub Msg
subscriptions =
    Sub.batch <|
        [ CM.subscriptions TemplateCMMsg
        , CM.subscriptions SourceCMMsg
        , CM.subscriptions CollectionsCMMsg
        , CM.subscriptions AvgCMMsg
        ]
