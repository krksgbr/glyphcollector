module Workspace.Msgs exposing (..)

import Avg
import ContentManager.Msg as CM
import IPC.Types exposing (MatchedGlyph, ProjectView)
import Image


type Msg
    = TemplateCMMsg (CM.Msg Image.Promise)
    | SourceCMMsg (CM.Msg Image.Promise)
    | CollectionsCMMsg (CM.Msg MatchedGlyph)
    | AvgCMMsg (CM.Msg Avg.Promise)
    | CollectGlyphs
    | CancelCollectGlyphs
    | GenerateAvgs
    | ReqSetView ProjectView
    | Close
    | ShowCollectionsDirectory String
    | ShowAvgsDirectory String
    | ShowFeedBack
    | HideFeedBack
    | NoOp
