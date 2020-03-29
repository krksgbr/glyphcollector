module Workspace exposing (Model, init, subscriptions, update, view)

import Avg
import ContentManager as CM
import Element exposing (..)
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
import Workspace.Collections
import Workspace.Frame as Workspace
import Workspace.Msgs exposing (Msg(..))
import Workspace.Sources
import Workspace.TemplateMatching


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


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        TemplateCMMsg subMsg ->
            CM.update
                Workspace.Sources.templateCMConfig
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
                Workspace.Sources.sourceCMConfig
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
                Workspace.Collections.cmConfig
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
                Workspace.Collections.avgCMConfig
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
                    Workspace.Sources.getSelection model
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

        NoOp ->
            model |> Return.singleton


getTMStatus : ProjectModel -> Maybe TMStatus
getTMStatus project =
    project.imP.process
        |> Maybe.andThen
            (\{ status } ->
                status
            )


view : ProjectModel -> Model -> Element Msg
view project model =
    Workspace.frame project model.showFeedBack <|
        case getTMStatus project of
            Just tmStatus ->
                Workspace.TemplateMatching.viewStatus tmStatus

            Nothing ->
                case project.view of
                    Sources ->
                        Workspace.Sources.view project model

                    Collections glyphName ->
                        Workspace.Collections.view glyphName project model


subscriptions : Sub Msg
subscriptions =
    Sub.batch <|
        [ CM.subscriptions TemplateCMMsg
        , CM.subscriptions SourceCMMsg
        , CM.subscriptions CollectionsCMMsg
        , CM.subscriptions AvgCMMsg
        ]
