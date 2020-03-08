module IPC exposing (Model, Msg(..), init, sendReq, subscriptions, update)

import IPC.Decoder
import IPC.Encoder
import IPC.PortFunnels as PortFunnels exposing (FunnelDict, Handler(..))
import IPC.Types exposing (Req(..), Res)
import Json.Decode as JD
import Json.Encode as JE
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import Return exposing (Return)
import Task


url : String
url =
    "ws://localhost:9160"


type Msg
    = Process JE.Value
    | GotResponse Res
    | Error String
    | Connected


type alias Model =
    { state : PortFunnels.State
    , connected : Bool
    }


init : Return Msg Model
init =
    WebSocket.makeOpen url
        |> WebSocket.send PortFunnels.cmdPort
        |> Return.return
            { connected = False
            , state = PortFunnels.initialState
            }


sendReq : Req -> Cmd msg
sendReq req =
    req
        |> IPC.Encoder.encodeReq
        |> JE.encode 0
        |> WebSocket.makeSend url
        |> WebSocket.send PortFunnels.cmdPort


send : a -> Cmd a
send msg =
    Task.perform (always msg) (Task.succeed ())


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        Process v ->
            let
                r =
                    PortFunnels.processValue funnelDict v model.state model
            in
            case r of
                Err err ->
                    model
                        |> Return.singleton
                        |> Return.command (send <| Error err)

                Ok m ->
                    m

        _ ->
            model
                |> Return.singleton


socketHandler : Response -> PortFunnels.State -> Model -> Return Msg Model
socketHandler response state mdl =
    let
        model =
            { mdl | state = state }
    in
    case response of
        WebSocket.MessageReceivedResponse { message } ->
            let
                res =
                    JD.decodeString IPC.Decoder.decodeRes message
            in
            case res of
                Ok r ->
                    model
                        |> Return.singleton
                        |> Return.command
                            (send <| GotResponse r)

                Err e ->
                    model
                        |> Return.singleton
                        |> Return.command (send <| Error "Failed to decode response.")

        WebSocket.ConnectedResponse r ->
            -- let
            --     _ =
            --         Debug.log "Connected" r.description
            -- in
            model
                |> Return.singleton
                |> Return.command (send Connected)

        WebSocket.ClosedResponse { code, wasClean, expected } ->
            model
                |> Return.singleton

        WebSocket.ErrorResponse error ->
            model
                |> Return.singleton
                |> Return.command (send <| Error <| WebSocket.errorToString error)

        _ ->
            case WebSocket.reconnectedResponses response of
                [] ->
                    model |> Return.singleton

                [ ReconnectedResponse r ] ->
                    -- let
                    --     _ =
                    --         Debug.log "Reconnected" r.description
                    -- in
                    model
                        |> Return.singleton

                list ->
                    -- let
                    --     _ =
                    --         Debug.log "log" (Debug.toString list)
                    -- in
                    model
                        |> Return.singleton


handlers : List (Handler Model Msg)
handlers =
    [ WebSocketHandler socketHandler
    ]


subscriptions : Model -> Sub Msg
subscriptions =
    PortFunnels.subscriptions Process


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict handlers (\_ _ -> PortFunnels.cmdPort)


closedString : WebSocket.ClosedCode -> Bool -> Bool -> String
closedString code wasClean expected =
    "code: "
        ++ WebSocket.closedCodeToString code
        ++ ", "
        ++ (if wasClean then
                "clean"

            else
                "not clean"
           )
        ++ ", "
        ++ (if expected then
                "expected"

            else
                "NOT expected"
           )
