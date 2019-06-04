module UI.Events.Custom exposing (onClick)

import Element exposing (Attribute)
import Html.Events
import Json.Decode as Decode


custom name decoder =
    Element.htmlAttribute <| Html.Events.custom name decoder


onClick :
    { message : msg
    , stopPropagation : Bool
    , preventDefault : Bool
    }
    -> Attribute msg
onClick config =
    custom "click" <|
        Decode.succeed
            config
