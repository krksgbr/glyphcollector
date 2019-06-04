module Return.Extra exposing (sendReq)

import IPC
import IPC.Types exposing (Req)
import Return exposing (Return)


sendReq : Req -> Return cmd msg -> Return cmd msg
sendReq req return =
    return
        |> Return.command
            (req
                |> IPC.sendReq
            )
