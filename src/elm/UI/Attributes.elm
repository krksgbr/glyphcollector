module UI.Attributes exposing (height_, vh, vw, width_)

import Element
import Html.Attributes as HA


type Unit
    = Pct Float
    | Vw Float
    | Vh Float


vw : Float -> Unit
vw =
    Vw


vh : Float -> Unit
vh =
    Vh


pct : Float -> Unit
pct =
    Pct


unitToString : Unit -> String
unitToString unit =
    case unit of
        Pct val ->
            String.fromFloat val ++ "%"

        Vw val ->
            String.fromFloat val ++ "vw"

        Vh val ->
            String.fromFloat val ++ "vh"


size s =
    Element.htmlAttribute
        << HA.style s
        << unitToString


height_ : Unit -> Element.Attribute msg
height_ =
    size "height"


width_ : Unit -> Element.Attribute msg
width_ =
    size "width"
