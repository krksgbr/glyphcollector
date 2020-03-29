module Workspace.TemplateMatching exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import File
import IPC.Types exposing (TMStatus)
import UI.Attributes exposing (vh, width_)
import UI.Button as Button
import UI.Color as Color
import UI.Spinner exposing (spinner)
import Workspace.Msgs as Workspace


viewStatus : TMStatus -> Element Workspace.Msg
viewStatus tmStatus =
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
                    { onPress = Just Workspace.CancelCollectGlyphs
                    , label = text "Cancel"
                    }
                ]
            ]
        ]
