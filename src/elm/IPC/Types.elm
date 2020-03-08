module IPC.Types exposing (..)

import Time exposing (Posix)


type alias Model =
    { project : Maybe ProjectModel
    , repo : RepoModel
    }

type Req
    = ProjectReq ProjectReq
    | RepoReq RepoReq
    | LoadProject Project
    | UnloadProject

type Res
    = Error String
    | Ready Model
    | ModelUpdated Model
    | BadReq

type alias ProjectModel =
    { templates : ImageStore
    , sources : ImageStore
    , name : String
    , createdAt : Int
    , updatedAt : Int
    , imP : ImPModel
    , directory : String
    , id : ProjectId
    , view : ProjectView
    }

type ProjectReq
    = ImPReq TMReq
    | ImportTemplates (List String)
    | CancelImportTemplates (List PendingImage)
    | ImportSources (List String)
    | CancelImportSources (List PendingImage)
    | DeleteTemplates (List Image)
    | DeleteSources (List Image)
    | ViewReq ProjectView
    | OpenCollectionsDirectory String
    | OpenAvgsDirectory String

type alias Image =
    { thumbnail : String
    , original : String
    , name : String
    , id : String
    }

type alias PendingImage =
    { filePath : String
    , fileName : String
    }

type alias ImageStore =
    { pending : Maybe (List PendingImage)
    , imported : List Image
    }

type alias GlyphCollection =
    { glyphName : String
    , matches : List MatchedGlyph
    , averages : List Avg
    }

type alias MatchedGlyph =
    { image : Image
    , score : Float
    , sourceImage : Image
    , templateImage : Image
    , glyphName : String
    }

type alias Avg =
    { image : Image
    , glyphName : String
    }

type ProjectView
    = Sources
    | Collections String

type alias TMInput =
    { templates : List Image
    , sources : List Image
    }

type alias TMStatus =
    { source : Image
    , template : Image
    , pct : Float
    }

type alias TMProcess =
    { threadId : String
    , status : Maybe TMStatus
    }

type alias ImPModel =
    { collections : List GlyphCollection
    , process : Maybe TMProcess
    , genAvgProcess : Maybe ((String, String))
    }

type TMReq
    = RunTemplateMatching TMInput
    | CancelTemplateMatching
    | DeleteMatchedGlyph MatchedGlyph
    | GenAvg (List MatchedGlyph)
    | DeleteAvg Avg
    | CancelGenAvg

type alias RepoModel =
    { projectRepo : List Project
    , version : String
    }

type RepoReq
    = CreateProject CreateProjectInput
    | RenameProject ProjectId String
    | DeleteProject ProjectId

type ProjectId
    = ProjectId String

unProjectId : ProjectId -> String
unProjectId (ProjectId x) = x

type alias CreateProjectInput =
    { directory : String
    , name : String
    }

type alias Project =
    { templates : List Image
    , sources : List Image
    , name : String
    , directory : String
    , createdAt : Int
    , updatedAt : Int
    , glyphCollections : List GlyphCollection
    , id : ProjectId
    }
