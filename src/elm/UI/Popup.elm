module UI.Popup exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Button as Button
import UI.Color as Color


view : Element msg -> { onDismissed : msg } -> Element msg
view child config =
    el
        [ width fill
        , height fill
        , Background.color
            (Color.white
                |> Color.opacify 0.5
            )
        ]
    <|
        el
            [ Border.width 4
            , Background.color
                Color.white
            , centerX
            , centerY
            , padding 20
            ]
        <|
            column [ width fill, height fill, centerX, centerY, Font.center, spacing 10 ]
                [ child
                , el [ alignRight, alignBottom ] <|
                    Button.default
                        { label = text "OK"
                        , onPress = Just config.onDismissed
                        }
                ]
