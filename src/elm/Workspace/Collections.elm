module Workspace.Collections exposing (..)

import Avg
import ContentManager as CM
import ContentManager.Content as CM
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Html.Attributes as HA
import IPC.Types exposing (ProjectModel, ProjectView(..))
import UI.Border as Border
import UI.Color as Color
import Workspace.Frame as Workspace
import Workspace.Msgs as Workspace


cmConfig : CM.Config IPC.Types.MatchedGlyph
cmConfig =
    CM.matchedGlyph


avgCMConfig : CM.Config Avg.Promise
avgCMConfig =
    CM.avg


getSelection model =
    CM.selection model.collectionsCM


type alias ContentManagers a =
    { a | collectionsCM : CM.Model IPC.Types.MatchedGlyph, avgCM : CM.Model Avg.Promise }


view : String -> ProjectModel -> ContentManagers a -> Element Workspace.Msg
view glyphName project model =
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
                            ([ onClick <| Workspace.ReqSetView <| Collections gn
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
                            text <|
                                String.left 3 gn
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
                    { toMsg = Workspace.CollectionsCMMsg
                    , items =
                        currentCollection
                            |> Maybe.map .matches
                            |> Maybe.withDefault []
                    , contentConfig = cmConfig
                    , title = "Collected Glyphs"
                    , contextMenuItems =
                        [ ( "Show directory"
                          , Workspace.ShowCollectionsDirectory glyphName
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
                { toMsg = Workspace.AvgCMMsg
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
                    [ ( "Show directory", Workspace.ShowAvgsDirectory glyphName )
                    ]
                , title = "Averaged Glyphs"
                , fileInput =
                    CM.FileInputDisabled (text "The averages your create will appear here.")
                }
            ]
        , Workspace.footer
            { back = Workspace.ReqSetView Sources
            , actionLabel = "Generate Average"
            , action =
                let
                    selection =
                        getSelection model
                in
                if List.isEmpty selection then
                    Nothing

                else
                    Just Workspace.GenerateAvgs
            }
        ]
