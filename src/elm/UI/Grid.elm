module UI.Grid exposing (grid)

import Element exposing (..)
import Html.Attributes as HA


htmlAttributes =
    List.map htmlAttribute


grid attrs children =
    el attrs <|
        column
            (htmlAttributes <|
                [ HA.style "display" "grid"
                , HA.style "grid-template-columns" "1fr 1fr 1fr"
                , HA.style "grid-template-rows" "repeat(3, minmax(75px, auto)) "
                ]
            )
            children
