module IPC.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import IPC.ElmStreet exposing (..)
import IPC.Types exposing (..)


decodeModel : Decoder Model
decodeModel = D.succeed Model
    |> required "project" (nullable decodeProjectModel)
    |> required "repo" decodeRepoModel

decodeReq : Decoder Req
decodeReq =
    let decide : String -> Decoder Req
        decide x = case x of
            "ProjectReq" -> D.field "contents" <| D.map ProjectReq decodeProjectReq
            "RepoReq" -> D.field "contents" <| D.map RepoReq decodeRepoReq
            "LoadProject" -> D.field "contents" <| D.map LoadProject decodeProject
            "UnloadProject" -> D.succeed UnloadProject
            c -> D.fail <| "Req doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeRes : Decoder Res
decodeRes =
    let decide : String -> Decoder Res
        decide x = case x of
            "Error" -> D.field "contents" <| D.map Error D.string
            "Ready" -> D.field "contents" <| D.map Ready decodeModel
            "ModelUpdated" -> D.field "contents" <| D.map ModelUpdated decodeModel
            "BadReq" -> D.succeed BadReq
            c -> D.fail <| "Res doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeProjectModel : Decoder ProjectModel
decodeProjectModel = D.succeed ProjectModel
    |> required "templates" decodeImageStore
    |> required "sources" decodeImageStore
    |> required "name" D.string
    |> required "createdAt" D.int
    |> required "updatedAt" D.int
    |> required "imP" decodeImPModel
    |> required "directory" D.string
    |> required "id" decodeProjectId
    |> required "view" decodeProjectView

decodeProjectReq : Decoder ProjectReq
decodeProjectReq =
    let decide : String -> Decoder ProjectReq
        decide x = case x of
            "ImPReq" -> D.field "contents" <| D.map ImPReq decodeTMReq
            "ImportTemplates" -> D.field "contents" <| D.map ImportTemplates (D.list D.string)
            "CancelImportTemplates" -> D.field "contents" <| D.map CancelImportTemplates (D.list decodePendingImage)
            "ImportSources" -> D.field "contents" <| D.map ImportSources (D.list D.string)
            "CancelImportSources" -> D.field "contents" <| D.map CancelImportSources (D.list decodePendingImage)
            "DeleteTemplates" -> D.field "contents" <| D.map DeleteTemplates (D.list decodeImage)
            "DeleteSources" -> D.field "contents" <| D.map DeleteSources (D.list decodeImage)
            "ViewReq" -> D.field "contents" <| D.map ViewReq decodeProjectView
            "OpenCollectionsDirectory" -> D.field "contents" <| D.map OpenCollectionsDirectory D.string
            "OpenAvgsDirectory" -> D.field "contents" <| D.map OpenAvgsDirectory D.string
            c -> D.fail <| "ProjectReq doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeImage : Decoder Image
decodeImage = D.succeed Image
    |> required "thumbnail" D.string
    |> required "original" D.string
    |> required "name" D.string
    |> required "id" D.string

decodePendingImage : Decoder PendingImage
decodePendingImage = D.succeed PendingImage
    |> required "filePath" D.string
    |> required "fileName" D.string

decodeImageStore : Decoder ImageStore
decodeImageStore = D.succeed ImageStore
    |> required "pending" (nullable (D.list decodePendingImage))
    |> required "imported" (D.list decodeImage)

decodeGlyphCollection : Decoder GlyphCollection
decodeGlyphCollection = D.succeed GlyphCollection
    |> required "glyphName" D.string
    |> required "matches" (D.list decodeMatchedGlyph)
    |> required "averages" (D.list decodeAvg)

decodeMatchedGlyph : Decoder MatchedGlyph
decodeMatchedGlyph = D.succeed MatchedGlyph
    |> required "image" decodeImage
    |> required "score" D.float
    |> required "sourceImage" decodeImage
    |> required "templateImage" decodeImage
    |> required "glyphName" D.string

decodeAvg : Decoder Avg
decodeAvg = D.succeed Avg
    |> required "image" decodeImage
    |> required "glyphName" D.string

decodeProjectView : Decoder ProjectView
decodeProjectView =
    let decide : String -> Decoder ProjectView
        decide x = case x of
            "Sources" -> D.succeed Sources
            "Collections" -> D.field "contents" <| D.map Collections D.string
            c -> D.fail <| "ProjectView doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeTMInput : Decoder TMInput
decodeTMInput = D.succeed TMInput
    |> required "templates" (D.list decodeImage)
    |> required "sources" (D.list decodeImage)

decodeTMStatus : Decoder TMStatus
decodeTMStatus = D.succeed TMStatus
    |> required "source" decodeImage
    |> required "template" decodeImage
    |> required "pct" D.float

decodeTMProcess : Decoder TMProcess
decodeTMProcess = D.succeed TMProcess
    |> required "threadId" D.string
    |> required "status" (nullable decodeTMStatus)

decodeImPModel : Decoder ImPModel
decodeImPModel = D.succeed ImPModel
    |> required "collections" (D.list decodeGlyphCollection)
    |> required "process" (nullable decodeTMProcess)
    |> required "genAvgProcess" (nullable (elmStreetDecodePair D.string D.string))

decodeTMReq : Decoder TMReq
decodeTMReq =
    let decide : String -> Decoder TMReq
        decide x = case x of
            "RunTemplateMatching" -> D.field "contents" <| D.map RunTemplateMatching decodeTMInput
            "CancelTemplateMatching" -> D.succeed CancelTemplateMatching
            "DeleteMatchedGlyph" -> D.field "contents" <| D.map DeleteMatchedGlyph decodeMatchedGlyph
            "GenAvg" -> D.field "contents" <| D.map GenAvg (D.list decodeMatchedGlyph)
            "DeleteAvg" -> D.field "contents" <| D.map DeleteAvg decodeAvg
            "CancelGenAvg" -> D.succeed CancelGenAvg
            c -> D.fail <| "TMReq doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeRepoModel : Decoder RepoModel
decodeRepoModel = D.succeed RepoModel
    |> required "projectRepo" (D.list decodeProject)
    |> required "version" D.string

decodeRepoReq : Decoder RepoReq
decodeRepoReq =
    let decide : String -> Decoder RepoReq
        decide x = case x of
            "CreateProject" -> D.field "contents" <| D.map CreateProject decodeCreateProjectInput
            "RenameProject" -> D.field "contents" <| D.map2 RenameProject (D.index 0 decodeProjectId) (D.index 1 D.string)
            "DeleteProject" -> D.field "contents" <| D.map DeleteProject decodeProjectId
            c -> D.fail <| "RepoReq doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeProjectId : Decoder ProjectId
decodeProjectId = D.map ProjectId D.string

decodeCreateProjectInput : Decoder CreateProjectInput
decodeCreateProjectInput = D.succeed CreateProjectInput
    |> required "directory" D.string
    |> required "name" D.string

decodeProject : Decoder Project
decodeProject = D.succeed Project
    |> required "templates" (D.list decodeImage)
    |> required "sources" (D.list decodeImage)
    |> required "name" D.string
    |> required "directory" D.string
    |> required "createdAt" D.int
    |> required "updatedAt" D.int
    |> required "glyphCollections" (D.list decodeGlyphCollection)
    |> required "id" decodeProjectId
