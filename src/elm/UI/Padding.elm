module UI.Padding exposing (bottom)

import Element exposing (paddingEach)


none_ =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    }


bottom n =
    paddingEach <|
        { none_
            | bottom = n
        }