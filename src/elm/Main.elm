module Main exposing (Model, main)

--import TemplateMatching

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Home
import Html exposing (Html)
import IPC
import IPC.Types exposing (Project, Res(..))
import Return exposing (Return)
import UI.Button as Button
import UI.Color as Color
import UI.Layout as Layout
import Workspace


type alias InitializedState =
    { remoteModel : IPC.Types.Model
    , homeModel : Home.Model
    , workspaceModel : Workspace.Model
    }


type State
    = WaitingForConnect
    | WaitingForRemote
    | Initialized InitializedState


type alias Model =
    { ipc : IPC.Model
    , state : State
    , error : Maybe String
    }


type Msg
    = IPCMsg IPC.Msg
    | HomeMsg Home.Msg
    | GoHome
    | WorkspaceMsg Workspace.Msg
    | DismissError



--    | TemplateMatchingMsg TemplateMatching.Msg


type alias Flags =
    ()


init : Flags -> Return Msg Model
init _ =
    IPC.init
        |> Return.mapBoth IPCMsg
            (\ipc ->
                { ipc = ipc
                , state = WaitingForConnect
                , error = Nothing
                }
            )



-- |> Debug.log "initialzied "


updateState : Model -> State -> Model
updateState model newState =
    { model | state = newState }


update : Msg -> Model -> Return Msg Model
update msg model =
    case ( msg, model.state ) of
        ( IPCMsg IPC.Connected, WaitingForConnect ) ->
            WaitingForRemote
                |> updateState model
                |> Return.singleton

        ( IPCMsg (IPC.GotResponse (Error err)), _ ) ->
            { model | error = Just err }
                |> Return.singleton

        ( IPCMsg (IPC.GotResponse (Ready newRemoteModel)), WaitingForRemote ) ->
            { model
                | state =
                    Initialized
                        { homeModel = Home.init
                        , workspaceModel = Workspace.init
                        , remoteModel = newRemoteModel
                        }
            }
                |> Return.singleton

        ( IPCMsg (IPC.GotResponse (ModelUpdated newRemoteModel)), Initialized state ) ->
            { state | remoteModel = newRemoteModel }
                |> Return.singleton
                |> Return.map Initialized
                |> Return.map (updateState model)

        ( IPCMsg subMsg, _ ) ->
            IPC.update subMsg model.ipc
                |> Return.mapBoth IPCMsg
                    (\ipc ->
                        { model | ipc = ipc }
                    )

        ( HomeMsg subMsg, Initialized state ) ->
            Home.update subMsg state.homeModel
                |> Return.mapBoth HomeMsg
                    (\newHomeModel ->
                        { state | homeModel = newHomeModel }
                    )
                |> Return.map Initialized
                |> Return.map (updateState model)

        ( WorkspaceMsg subMsg, Initialized state ) ->
            Workspace.update subMsg state.workspaceModel
                |> Return.mapBoth WorkspaceMsg
                    (\newWorkspaceModel ->
                        { state | workspaceModel = newWorkspaceModel }
                    )
                |> Return.map Initialized
                |> Return.map (updateState model)

        ( DismissError, _ ) ->
            { model | error = Nothing }
                |> Return.singleton

        ( _, _ ) ->
            model
                |> Return.singleton


viewError : String -> Element Msg
viewError err =
    el
        [ width fill
        , height fill
        , Background.color
            (Color.white
                |> Color.opacify 0.5
            )
        ]
    <|
        el
            [ width <| px 500
            , height <| px 500
            , Font.color Color.red
            , Border.width 4
            , Background.color
                Color.white
            , centerX
            , centerY
            , padding 20
            ]
        <|
            column [ width fill, height fill, centerX, centerY, Font.center, spacing 10 ]
                [ el [ width fill, centerY, Font.bold ] <| text "An error occured:"
                , el [ width fill, centerY ] <| text err
                , el [ alignRight, alignBottom ] <|
                    Button.error
                        { label = text "OK"
                        , onPress = Just DismissError
                        }
                ]


viewLoading =
    column [ Font.size 50, centerY, centerX ]
        [ text "one sec..."
        ]


frame maybeError =
    layoutWith
        { options =
            Layout.options
        }
        (Layout.attrs
            ++ [ inFront <|
                    (maybeError
                        |> Maybe.map viewError
                        |> Maybe.withDefault none
                    )
               , Background.color Color.background
               ]
        )


view : Model -> Html Msg
view model =
    frame model.error <|
        case model.state of
            WaitingForConnect ->
                viewLoading

            WaitingForRemote ->
                viewLoading

            Initialized state ->
                case state.remoteModel.project of
                    Just projectModel ->
                        Workspace.view projectModel
                            state.workspaceModel
                            |> Element.map WorkspaceMsg

                    Nothing ->
                        Home.view state.homeModel state.remoteModel.repo.projectRepo
                            |> Element.map HomeMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    [ IPC.subscriptions model.ipc |> Sub.map IPCMsg
    , Workspace.subscriptions |> Sub.map WorkspaceMsg
    ]
        |> Sub.batch



-- MODEL


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
