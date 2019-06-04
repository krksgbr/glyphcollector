module UI.Background exposing
    ( PositionX(..)
    , PositionY(..)
    , Size(..)
    , image
    )

import Element
import Html as H
import Html.Attributes as HA


type PositionY
    = Top
    | Bottom
    | CenterY


type PositionX
    = Left
    | Right
    | CenterX


type Size
    = Cover
    | Contain


positionYToString position =
    case position of
        Top ->
            "top"

        Bottom ->
            "bottom"

        CenterY ->
            "center"


positionXToString position =
    case position of
        Left ->
            "left"

        Right ->
            "right"

        CenterX ->
            "center"


positionProp : ( PositionX, PositionY ) -> ( String, String )
positionProp ( x, y ) =
    ( "background-position"
    , [ positionXToString x, positionYToString y ] |> String.join " "
    )


sizeProp size =
    ( "background-size"
    , case size of
        Cover ->
            "cover"

        Contain ->
            "contain"
    )


urlProp url =
    ( "background"
    , "url(" ++ "\"" ++ url ++ "\"" ++ ")"
    )


renderProp ( p, v ) =
    [ p, v ]
        |> String.join ":"


style styles =
    styles
        |> List.map renderProp
        |> String.join ";"
        |> HA.attribute "style"


image : String -> ( PositionX, PositionY ) -> Size -> Element.Attribute msg
image url position size =
    Element.htmlAttribute <|
        style
            [ urlProp url
            , positionProp position
            , sizeProp size
            , ( "background-repeat", "no-repeat" )
            ]
