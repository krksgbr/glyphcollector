module Avg exposing (Promise(..))

import IPC.Types exposing (Avg, Image)


type Promise
    = Pending String
    | Resolved Avg



