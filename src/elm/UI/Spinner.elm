module UI.Spinner exposing (spinner)

import Element exposing (..)
import Html.Attributes as HA


spinner : Element msg
spinner =
    el
        [ htmlAttribute <| HA.class "spinner"
        , centerY
        , centerX
        ]
    <|
        none
