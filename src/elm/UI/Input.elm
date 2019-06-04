module UI.Input exposing (directory, file, files, slider, textField)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onLoseFocus)
import Element.Font as Font
import Element.Input as Input exposing (Label, Thumb, labelAbove)
import File exposing (File)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D exposing (Decoder)
import UI.Border as Border
import UI.Button as UI
import UI.Color as Color
import UI.Layout as Layout
import UI.Padding as Padding


type alias TextFieldAttributes =
    { label : String
    , placeholder : String
    }


type Error
    = ValidationFailed String
    | RequiredFieldIsEmpty


type alias TextFieldConfig msg =
    { onChange : String -> msg
    , onBlur : Maybe msg
    , disabled : Bool
    , value : String
    , error : Maybe Error
    , showError : Bool
    , attributes : TextFieldAttributes
    }


viewErrorMessage : String -> Element msg
viewErrorMessage message =
    el [ Font.color Color.error, Font.size 5, Padding.bottom 10 ] <| text message


wrapWithLabelAndError : String -> Maybe Error -> Bool -> Element msg -> Element msg
wrapWithLabelAndError labelStr error showError field =
    let
        defaultAttrs =
            { viewError = none
            , inputAttrs = [ Border.none ]
            , labelAttrs = []
            }

        { viewError, inputAttrs, labelAttrs } =
            if showError then
                case error of
                    Just err ->
                        let
                            errorMessage =
                                case err of
                                    RequiredFieldIsEmpty ->
                                        "This field is required."

                                    ValidationFailed message ->
                                        message
                        in
                        { viewError = viewErrorMessage errorMessage
                        , inputAttrs = [ Border.color Color.error, Border.bottom 1 ]
                        , labelAttrs = [ Font.color Color.error ]
                        }

                    Nothing ->
                        defaultAttrs

            else
                defaultAttrs
    in
    column [ spacing 5, width fill, alignTop ]
        [ label labelAttrs labelStr
        , el (inputAttrs ++ [ width fill ]) <| field
        , viewError
        ]


label : List (Element.Attribute msg) -> String -> Element msg
label attrs str =
    el ([ Font.size 15, paddingXY 3 0 ] ++ attrs) <| text str


textField : TextFieldConfig msg -> Element msg
textField { onChange, onBlur, disabled, value, error, showError, attributes } =
    let
        onBlur_ =
            onBlur
                |> Maybe.map onLoseFocus
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    wrapWithLabelAndError attributes.label error showError <|
        Input.text
            ([ Background.color Color.transparent
             , Font.size 20
             , Border.bottom 1
             , Border.color Color.black
             , Border.rounded 0
             , height <| px 30
             , paddingXY 3 10
             , focused [ Border.noGlow ]

             -- , onFocus config.onFocus
             ]
                ++ onBlur_
            )
            { onChange = onChange
            , label = labelAbove [] none
            , text = value
            , placeholder = Nothing
            }


mkDecoder : (File -> List File -> value) -> Decoder value
mkDecoder f =
    D.at [ "target", "files" ] <|
        D.oneOrMore f File.decoder


decodeFiles : D.Decoder (List File)
decodeFiles =
    mkDecoder (::)


decodeFile : Decoder File
decodeFile =
    mkDecoder (\f _ -> f)


mkFileInput : Element msg -> String -> Decoder msg -> List (H.Attribute msg) -> Element msg
mkFileInput labelEl id decoder attrs =
    -- the extra wrap in el[] is necessary here so that spacing set on wrapper elements will apply to this element
    el [] <|
        html <|
            H.div []
                [ H.label
                    [ HA.for id
                    , HA.style "cursor" "pointer"
                    , HA.style "display" "flex"
                    , HA.style "align-items" "center"
                    ]
                    [ Element.layoutWith
                        { options =
                            Layout.options
                                ++ [ Element.noStaticStyleSheet
                                   ]
                        }
                        Layout.attrs
                      <|
                        labelEl
                    ]
                , H.input
                    ([ HA.type_ "file"
                     , HA.id id
                     , HA.name "file"
                     , HE.on "change" decoder
                     , HA.style "display" "none"
                     ]
                        ++ attrs
                    )
                    []
                ]


type alias FileInputConfig value msg =
    { label : Element msg
    , onChange : value -> msg
    , id : String
    }


file : FileInputConfig File msg -> Element msg
file config =
    mkFileInput config.label
        config.id
        (D.map config.onChange decodeFile)
        []


files : FileInputConfig (List File) msg -> Element msg
files config =
    mkFileInput config.label
        config.id
        (D.map config.onChange decodeFiles)
        [ HA.attribute "multiple" "true"
        ]


directory : FileInputConfig File msg -> Element msg
directory config =
    mkFileInput config.label
        config.id
        (D.map config.onChange decodeFile)
        [ HA.attribute "webkitdirectory" "true"
        ]


slider :
    { onChange : Float -> msg
    , label : Label msg
    , min : Float
    , max : Float
    , value : Float
    , step : Maybe Float
    }
    -> Element msg
slider config =
    let
        thumb =
            Input.thumb
                [ Element.width (Element.px 16)
                , Element.height (Element.px 16)
                , Border.rounded 8
                , Border.width 1
                , Background.color Color.black
                ]
    in
    Input.slider
        [ height <| px 30
        , behindContent <|
            el
                [ Background.color Color.black
                , Border.rounded 2
                , width fill
                , height <| px 2
                , centerY
                ]
                none
        ]
        { onChange = config.onChange
        , label = config.label
        , min = config.min
        , max = config.max
        , value = config.value
        , step = config.step
        , thumb = thumb
        }
