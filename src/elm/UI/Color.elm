module UI.Color exposing (accent, background, black, disabled, error, green, grey, opacify, red, toCss, transparent, white)

import Element exposing (Color)


green : Color
green =
    Element.rgb 0 1 0


red : Color
red =
    Element.rgb 1 0 0


error : Color
error =
    red


black : Color
black =
    Element.rgb 0 0 0


grey : Float -> Color
grey n =
    Element.rgb n n n


white : Color
white =
    Element.rgb 1 1 1


disabled : Color
disabled =
    Element.rgb 0.8 0.8 0.8


background : Color
background =
    Element.rgb 0.93 0.93 0.93


transparent : Color
transparent =
    Element.rgba 0 0 0 0


accent : Color
accent =
    Element.rgb255 123 0 255


opacify : Float -> Color -> Color
opacify opacity color =
    color
        |> Element.toRgb
        |> (\c -> { c | alpha = opacity })
        |> Element.fromRgb


toCss color =
    let
        rgba =
            Element.toRgb color

        str =
            String.fromInt << round

        str255 =
            str << (*) 255
    in
    "rgba(" ++ str255 rgba.red ++ "," ++ str255 rgba.green ++ "," ++ str255 rgba.blue ++ "," ++ str255 rgba.alpha ++ ")"
