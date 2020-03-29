module Workspace.Navigation exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Html as H
import Html.Attributes as HA
import IPC.Types exposing (ProjectModel, ProjectView(..))
import UI.Button as Button
import UI.Color as Color
import UI.Layout as Layout
import Workspace.Msgs as Workspace


type alias PowerlineConfig =
    { active : Bool
    , onClick : Maybe Workspace.Msg
    , first : Bool
    , text : String
    }


powerline : PowerlineConfig -> Element Workspace.Msg
powerline config =
    let
        { fontColor, backgroundColor } =
            if config.active then
                { fontColor = Color.white
                , backgroundColor = Color.accent
                }

            else if config.onClick == Nothing then
                { backgroundColor = Color.grey 0.3
                , fontColor = Color.grey 0.7
                }

            else
                { backgroundColor = Color.black
                , fontColor = Color.white
                }
    in
    row
        [ height <| px Layout.workspace.headerHeight
        , onClick <| Maybe.withDefault Workspace.NoOp config.onClick
        , pointer
        , Font.color fontColor
        ]
        [ el
            [ height fill
            , htmlAttribute <| HA.style "position" "relative"
            , Background.color backgroundColor
            ]
          <|
            el
                [ centerY
                , paddingEach
                    { left =
                        if config.first then
                            Layout.global.paddingX

                        else
                            Layout.global.paddingX + 5
                    , right = Layout.global.paddingX // 2
                    , top = 0
                    , bottom = 0
                    }
                ]
            <|
                text config.text
        , html <|
            H.div
                [ HA.style "position" "absolute"
                , HA.style "right" "-1em"
                , HA.style "z-index" "1"
                ]
                [ H.div
                    [ HA.style "border-top" "1em solid transparent"
                    , HA.style "border-bottom" "1em solid transparent"
                    , HA.style "border-left" "1em solid"
                    , HA.style "color" <| Color.toCss Color.white
                    ]
                    []
                , H.div
                    [ HA.style "border-top" "1em solid transparent"
                    , HA.style "border-bottom" "1em solid transparent"
                    , HA.style "border-left" "1em solid"
                    , HA.style "position" "absolute"
                    , HA.style "top" "0"
                    , HA.style "right" "1px"
                    , HA.style "color" <| Color.toCss <| backgroundColor
                    ]
                    []
                ]
        ]


navigation : ProjectModel -> Element Workspace.Msg
navigation project =
    let
        maybeNavToCollections =
            project.imP.collections
                |> List.head
                |> Maybe.map (Collections << .glyphName)
                |> Maybe.map Workspace.ReqSetView
    in
    row
        [ width fill
        , Background.color Color.black
        , Font.color Color.white
        ]
        ([ powerline
            { onClick = Just Workspace.Close
            , text = "Projects"
            , active = False
            , first = True
            }
         , powerline
            { onClick = Just <| Workspace.ReqSetView Sources
            , text = project.name
            , active = project.view == Sources
            , first = False
            }
         , powerline
            { onClick = maybeNavToCollections
            , text = "Collections"
            , active =
                case project.view of
                    Collections _ ->
                        True

                    _ ->
                        False
            , first = False
            }
         ]
            |> (\items ->
                    items
                        ++ [ Button.text "Feedback"
                                [ paddingEach
                                    { top = 0
                                    , left = 0
                                    , right = Layout.global.paddingX
                                    , bottom = 0
                                    }
                                , alignRight
                                , onClick <| Workspace.ShowFeedBack
                                ]
                           ]
               )
        )
