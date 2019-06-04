module TemplateMatching exposing
    ( Model
    , Msg
    , Response(..)
    , handleResponse
    , init
    , update
    , view
    )

import Element exposing (..)
import Element.Font as Font
import File exposing (File)
import IPC
import IPC.Types exposing (Req(..), Res(..), TMResult)
import Return exposing (Return)
import UI.Button exposing (normal)
import UI.FileDrop as FileDrop


type alias Model =
    { sources : FileDrop.Model
    , templates : FileDrop.Model
    , isProcessing : Bool
    }


type Msg
    = SourcesDropMsg FileDrop.Msg
    | TemplatesDropMsg FileDrop.Msg
    | DoProcess
    | DoCancel


type Response
    = ReceiveResults (Result String (List TMResult))
    | Canceled


init : Model
init =
    { sources = FileDrop.init
    , templates = FileDrop.init
    , isProcessing = False
    }


viewFiles : String -> List File -> Element msg
viewFiles title files =
    let
        viewImage { url } =
            image
                [ centerY
                , height
                    (fill |> maximum 300)
                ]
                { src = url
                , description = ""
                }

        imgs =
            files
                |> List.map viewImage
    in
    column [ width fill, height fill ]
        [ el
            [ centerX
            , Font.size 30
            ]
          <|
            text title
        , row [ centerY, height fill, width fill, clipX, scrollbarX ]
            imgs
        ]


handleResponse : Response -> Model -> Return Msg Model
handleResponse response model =
    case response of
        Canceled ->
            { model | isProcessing = False }
                |> Return.singleton

        ReceiveResults result ->
            case result of
                Ok r ->
                    { model | isProcessing = False }
                        |> Return.singleton

                Err err ->
                    { model | isProcessing = False }
                        |> Return.singleton


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        SourcesDropMsg subMsg ->
            { model
                | sources = FileDrop.update subMsg model.sources
            }
                |> Return.singleton

        TemplatesDropMsg subMsg ->
            { model
                | templates = FileDrop.update subMsg model.templates
            }
                |> Return.singleton

        DoProcess ->
            { model
                | isProcessing = True
            }
                |> Return.singleton
                |> Return.command
                    (MatchTemplate
                        { templates = model.templates.files |> List.map .path
                        , sources = model.sources.files |> List.map .path
                        }
                        |> IPC.sendReq
                    )

        DoCancel ->
            model
                |> Return.singleton
                |> Return.command
                    (Cancel
                        |> IPC.sendReq
                    )


section : Element msg -> Element msg
section =
    el [ height <| fillPortion 1, width fill ]


view : Model -> Element Msg
view model =
    let
        haveTemplates =
            not <| List.isEmpty model.templates.files

        haveSources =
            not <| List.isEmpty model.sources.files

        canProcess =
            List.all identity <|
                [ haveTemplates
                , haveSources
                , not model.isProcessing
                ]
    in
    column [ height fill, width fill, spacing 10 ]
        [ column [ width fill, height fill, spacing 10 ]
            [ section <|
                if haveSources then
                    viewFiles "Sources" model.sources.files

                else
                    FileDrop.view SourcesDropMsg "Drop sources" model.sources
            , section <|
                if haveTemplates then
                    viewFiles "Templates" model.templates.files

                else
                    FileDrop.view TemplatesDropMsg "Drop templates" model.templates
            ]
        , row [ spacing 10, centerX ]
            [ normal
                { onPress =
                    if canProcess then
                        Just DoProcess

                    else
                        Nothing
                , label = Element.text "Process"
                }
            , normal
                { onPress =
                    if model.isProcessing then
                        Just DoCancel

                    else
                        Nothing
                , label = Element.text "Cancel"
                }
            ]
        ]
