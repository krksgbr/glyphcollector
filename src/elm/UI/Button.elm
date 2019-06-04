module UI.Button exposing
    ( Color(..)
    , Size(..)
    , custom
    , default
    , error
    , showAsButton
    , text
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import Html.Attributes as HA
import UI.Color as Color
import UI.Cursor as Cursor


type Size
    = Small
    | Normal


type Color
    = Default
    | Accent


type alias Config =
    { size : Size
    , color : Color
    }


custom : Config -> { onPress : Maybe msg, label : Element msg } -> Element msg
custom config btn =
    let
        isDisabled =
            btn.onPress == Nothing

        sizeAttrs =
            case config.size of
                Small ->
                    sizeSmall

                Normal ->
                    sizeNormal

        colorAttrs =
            case config.color of
                Default ->
                    colorDefault isDisabled

                Accent ->
                    colorAccent isDisabled
    in
    EI.button
        (attrCommon isDisabled ++ sizeAttrs ++ colorAttrs)
        btn


default : { onPress : Maybe msg, label : Element msg } -> Element msg
default =
    custom
        { size = Small
        , color = Default
        }


attrCommon isDisabled =
    if isDisabled then
        [ Cursor.notAllowed ]

    else
        []



-- size


sizeNormal =
    [ height <| px 40
    , paddingXY 15 0
    , Border.rounded 10
    ]


sizeSmall =
    [ height <| px 25
    , paddingXY 7 0
    , Border.rounded 5
    ]



--- color


colorDefault : Bool -> List (Attr () msg)
colorDefault isDisabled =
    [ Background.color Color.white
    , Font.color Color.black
    , Border.width 1
    , mouseOver
        [ Background.color Color.black
        , Font.color Color.white
        ]
    ]
        ++ (if isDisabled then
                [ Border.color Color.disabled
                , Font.color Color.disabled
                , mouseOver []
                , focused []
                ]

            else
                []
           )


colorAccent : Bool -> List (Attr () msg)
colorAccent isDisabled =
    [ Background.color Color.black
    , Font.color Color.white
    , mouseOver
        [ Background.color Color.accent
        , Font.color Color.white
        ]
    ]
        ++ (if isDisabled then
                [ Background.color Color.disabled
                , Font.color <| rgb 0.5 0.5 0.5
                , mouseOver []
                , focused []
                ]

            else
                []
           )


error : { onPress : Maybe msg, label : Element msg } -> Element msg
error config =
    EI.button
        (attrCommon False
            ++ sizeNormal
            ++ [ Background.color <|
                    if config.onPress == Nothing then
                        Color.disabled

                    else
                        Color.white
               , mouseOver []
               , Font.color Color.red
               ]
        )
        config



-- Useful for something to masquarade as a button, such as a file input button
-- Look at Input.mkFileInput


showAsButton : Element a -> Element a
showAsButton element =
    EI.button
        (attrCommon False
            ++ sizeSmall
            ++ colorDefault False
        )
        { onPress = Nothing
        , label = element
        }


text : String -> List (Attribute msg) -> Element msg
text t attrs =
    el
        ([ mouseOver
            [ Font.color Color.accent
            ]
         , pointer
         , padding 5
         ]
            ++ attrs
        )
    <|
        Element.text t
