module ContentManager.Msg exposing (Msg(..))

import File exposing (File)
import UI.FileDrop as FileDrop


type Msg i
    = ItemClicked i
    | SelectedAll (List i)
    | SelectedNone
    | FileInputChanged (List File)
    | FileDropMsg FileDrop.Msg
    | ZoomChanged Float
    | ItemsRemoved (List i)
    | OpenContextMenu
    | CloseContextMenu
    | DeleteAll
