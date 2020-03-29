module Workspace.Frame exposing (..)

import Element exposing (..)
import Html.Attributes as HA
import IPC.Types exposing (ProjectModel)
import UI.Border as Border
import UI.Button as Button
import UI.Layout as Layout
import UI.Popup as Popup
import Workspace.Msgs as Workspace
import Workspace.Navigation as Workspace


frame : ProjectModel -> Bool -> Element Workspace.Msg -> Element Workspace.Msg
frame project showFeedBack e =
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
                    { onDismissed = Workspace.HideFeedBack }

            else
                none
        ]
        [ Workspace.navigation project
        , e
        ]


type alias FooterConfig msg =
    { back : msg, action : Maybe msg, actionLabel : String }


footer : FooterConfig msg -> Element msg
footer config =
    row
        [ Border.top 2
        , height <| px Layout.workspace.footerHeight
        , width fill
        , paddingXY Layout.global.paddingX 0
        ]
        [ el [ alignLeft ] <|
            Button.custom
                { size = Button.Normal
                , color = Button.Accent
                }
                { onPress = Just <| config.back
                , label = text "Back"
                }
        , el [ alignRight ] <|
            Button.custom
                { size = Button.Normal
                , color = Button.Accent
                }
                { onPress = config.action
                , label = text config.actionLabel
                }
        ]
