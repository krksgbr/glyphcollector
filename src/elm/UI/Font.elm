module UI.Font exposing (emoji)

import Element exposing (htmlAttribute)
import Element.Font as Font
import Html.Attributes as HA


emoji : Element.Attribute msg
emoji =
    htmlAttribute <|
        HA.style "font-family" "\"Noto Color Emoji\""



--    Font.family
--        [ Font.typeface "Noto Color Emoji"
--        , Font.typeface "`Apple Color Emoji"
--        , Font.typeface "Segoe UI Emoji"
--        ]
