module UI.Cursor exposing (notAllowed)

import Element exposing (htmlAttribute)
import Html.Attributes as HA


notAllowed : Element.Attribute msg
notAllowed =
    htmlAttribute <| HA.style "cursor" "not-allowed"
