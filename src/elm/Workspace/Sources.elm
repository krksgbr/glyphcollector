module Workspace.Sources exposing (..)

import ContentManager as CM
import ContentManager.Content as CM
import Element exposing (..)
import IPC.Types exposing (ProjectModel)
import Image
import Project
import UI.Border as Border
import Workspace.Frame as Workspace
import Workspace.Msgs as Workspace


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


getSelection model =
    { templates =
        CM.selection model.templateCM
            |> Image.catResolved
            |> List.sortBy .name
    , sources =
        CM.selection model.sourceCM
            |> Image.catResolved
            |> List.sortBy .name
    }


type alias ContentManagers a =
    { a | templateCM : CM.Model Image.Promise, sourceCM : CM.Model Image.Promise }


view : ProjectModel -> ContentManagers a -> Element Workspace.Msg
view project model =
    column [ width fill, height fill ]
        [ row [ width fill, height fill ]
            [ el [ width fill, height fill, Border.right 2 ] <|
                CM.view model.templateCM
                    { toMsg = Workspace.TemplateCMMsg
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
                { toMsg = Workspace.SourceCMMsg
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
        , Workspace.footer
            { actionLabel = "Collect Glyphs"
            , back = Workspace.Close
            , action =
                let
                    selection =
                        getSelection model
                in
                if List.isEmpty selection.templates || List.isEmpty selection.sources then
                    Nothing

                else
                    Just Workspace.CollectGlyphs
            }
        ]
