module UI.Typography exposing (h2)

import Element exposing (el, text)
import Element.Font as Font


hn n t =
    el [ Font.size n ] <| text t


h2 =
    hn 20


h1 =
    hn 30


