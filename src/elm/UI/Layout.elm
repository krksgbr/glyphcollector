module UI.Layout exposing (contentManager, global, workspace, attrs, options)

import Element exposing (focusStyle, inFront)
import Element.Background as Background
import Element.Font as Font
import UI.Color as Color


global =
    { paddingX = 20
    }


contentManager =
    { headerHeight = 50
    }


workspace =
    { footerHeight = 60
    , headerHeight = 30
    }


options =
    [ focusStyle
        { shadow =
            Just <|
                { color = Color.accent |> Color.opacify 0.2
                , offset = ( 0, 0 )
                , blur = 3
                , size = 3
                }
        , backgroundColor = Nothing
        , borderColor = Just Color.accent
        }
    ]


attrs =
    [ Font.family [ Font.typeface "whoismono" ]
    , Font.size 15
    ]
