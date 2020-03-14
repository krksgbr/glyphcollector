port module AppSettings exposing (..)

import Json.Decode as JD
import Json.Encode as JE


type alias Settings =
    { ignoredReleaseTags : List String
    }


init : Settings
init =
    { ignoredReleaseTags = []
    }


decodeSettings : JD.Decoder Settings
decodeSettings =
    JD.map Settings
        (JD.field "ignoredReleaseTags" <| JD.list JD.string)


encodeSettings : Settings -> JE.Value
encodeSettings record =
    JE.object
        [ ( "ignoredReleaseTags", JE.list JE.string <| record.ignoredReleaseTags )
        ]


port updateAppSettings : Settings -> Cmd msg


port appSettingsUpdated : (Settings -> msg) -> Sub msg
