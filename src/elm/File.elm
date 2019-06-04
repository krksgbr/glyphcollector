module File exposing (File, decoder, pathToUrl)

import Element exposing (Element, column, el, image, row, text)
import Json.Decode as D


type alias File =
    { name : String
    , path : String
    , url : String
    , size : Int
    , lastModified : Int
    , fileType : String
    }


pathToUrl p =
    String.replace "\\" "/" p
    |> String.append "file://"


decoder : D.Decoder File
decoder =
    D.map6 File
        (D.field "name" D.string)
        (D.field "path" D.string)
        (D.field "path" D.string
            |> D.andThen (pathToUrl >> D.succeed)
        )
        (D.field "size" D.int)
        (D.field "lastModified" D.int)
        (D.field "type" D.string)
