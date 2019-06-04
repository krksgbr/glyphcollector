module UI.Border exposing (bottom, left, noGlow, none, right, top)

import Element
import Element.Border as Border
import UI.Color as Color


noBorder =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    }


bottom : Int -> Element.Attribute msg
bottom w =
    Border.widthEach
        { noBorder | bottom = w }


top : Int -> Element.Attribute msg
top w =
    Border.widthEach
        { noBorder | top = w }


left : Int -> Element.Attribute msg
left w =
    Border.widthEach
        { noBorder | left = w }


right : Int -> Element.Attribute msg
right w =
    Border.widthEach
        { noBorder | right = w }


none : Element.Attribute msg
none =
    Border.widthEach
        { top = 0
        , bottom = 0
        , left = 0
        , right = 0
        }


noGlow : Element.Attr decorative msg
noGlow =
    Border.glow Color.transparent 0.0
