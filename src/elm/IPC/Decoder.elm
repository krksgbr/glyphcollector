module IPC.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import IPC.ElmStreet exposing (..)
import IPC.Types as T


decodeModel : Decoder T.Model
decodeModel = D.succeed T.Model
    |> required "project" (nullable decodeProjectModel)
    |> required "repo" decodeRepoModel

decodeReq : Decoder T.Req
decodeReq =
    let decide : String -> Decoder T.Req
        decide x = case x of
            "ProjectReq" -> D.field "contents" <| D.map T.ProjectReq decodeProjectReq
            "RepoReq" -> D.field "contents" <| D.map T.RepoReq decodeRepoReq
            "LoadProject" -> D.field "contents" <| D.map T.LoadProject decodeProject
            "UnloadProject" -> D.succeed T.UnloadProject
            c -> D.fail <| "Req doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeRes : Decoder T.Res
decodeRes =
    let decide : String -> Decoder T.Res
        decide x = case x of
            "Error" -> D.field "contents" <| D.map T.Error D.string
            "Ready" -> D.field "contents" <| D.map T.Ready decodeModel
            "ModelUpdated" -> D.field "contents" <| D.map T.ModelUpdated decodeModel
            "BadReq" -> D.succeed T.BadReq
            c -> D.fail <| "Res doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeProjectModel : Decoder T.ProjectModel
decodeProjectModel = D.succeed T.ProjectModel
    |> required "templates" decodeImageStore
    |> required "sources" decodeImageStore
    |> required "name" D.string
    |> required "createdAt" D.int
    |> required "updatedAt" D.int
    |> required "imP" decodeImPModel
    |> required "directory" D.string
    |> required "id" decodeProjectId
    |> required "view" decodeProjectView

decodeProjectReq : Decoder T.ProjectReq
decodeProjectReq =
    let decide : String -> Decoder T.ProjectReq
        decide x = case x of
            "ImPReq" -> D.field "contents" <| D.map T.ImPReq decodeTMReq
            "ImportTemplates" -> D.field "contents" <| D.map T.ImportTemplates (D.list D.string)
            "CancelImportTemplates" -> D.field "contents" <| D.map T.CancelImportTemplates (D.list decodePendingImage)
            "ImportSources" -> D.field "contents" <| D.map T.ImportSources (D.list D.string)
            "CancelImportSources" -> D.field "contents" <| D.map T.CancelImportSources (D.list decodePendingImage)
            "DeleteTemplates" -> D.field "contents" <| D.map T.DeleteTemplates (D.list decodeImage)
            "DeleteSources" -> D.field "contents" <| D.map T.DeleteSources (D.list decodeImage)
            "ViewReq" -> D.field "contents" <| D.map T.ViewReq decodeProjectView
            "OpenCollectionsDirectory" -> D.field "contents" <| D.map T.OpenCollectionsDirectory D.string
            "OpenAvgsDirectory" -> D.field "contents" <| D.map T.OpenAvgsDirectory D.string
            c -> D.fail <| "ProjectReq doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeImage : Decoder T.Image
decodeImage = D.succeed T.Image
    |> required "thumbnail" D.string
    |> required "original" D.string
    |> required "name" D.string
    |> required "id" D.string

decodePendingImage : Decoder T.PendingImage
decodePendingImage = D.succeed T.PendingImage
    |> required "filePath" D.string
    |> required "fileName" D.string

decodeImageStore : Decoder T.ImageStore
decodeImageStore = D.succeed T.ImageStore
    |> required "pending" (nullable (D.list decodePendingImage))
    |> required "imported" (D.list decodeImage)

decodeGlyphCollection : Decoder T.GlyphCollection
decodeGlyphCollection = D.succeed T.GlyphCollection
    |> required "glyphName" D.string
    |> required "matches" (D.list decodeMatchedGlyph)
    |> required "averages" (D.list decodeAvg)

decodeMatchedGlyph : Decoder T.MatchedGlyph
decodeMatchedGlyph = D.succeed T.MatchedGlyph
    |> required "image" decodeImage
    |> required "score" D.float
    |> required "sourceImage" decodeImage
    |> required "templateImage" decodeImage
    |> required "glyphName" D.string

decodeAvg : Decoder T.Avg
decodeAvg = D.succeed T.Avg
    |> required "image" decodeImage
    |> required "glyphName" D.string

decodeProjectView : Decoder T.ProjectView
decodeProjectView =
    let decide : String -> Decoder T.ProjectView
        decide x = case x of
            "Sources" -> D.succeed T.Sources
            "Collections" -> D.field "contents" <| D.map T.Collections D.string
            c -> D.fail <| "ProjectView doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeTMInput : Decoder T.TMInput
decodeTMInput = D.succeed T.TMInput
    |> required "templates" (D.list decodeImage)
    |> required "sources" (D.list decodeImage)

decodeTMStatus : Decoder T.TMStatus
decodeTMStatus = D.succeed T.TMStatus
    |> required "source" decodeImage
    |> required "template" decodeImage
    |> required "pct" D.float

decodeTMProcess : Decoder T.TMProcess
decodeTMProcess = D.succeed T.TMProcess
    |> required "threadId" D.string
    |> required "status" (nullable decodeTMStatus)

decodeImPModel : Decoder T.ImPModel
decodeImPModel = D.succeed T.ImPModel
    |> required "collections" (D.list decodeGlyphCollection)
    |> required "process" (nullable decodeTMProcess)
    |> required "genAvgProcess" (nullable (elmStreetDecodePair D.string D.string))

decodeTMReq : Decoder T.TMReq
decodeTMReq =
    let decide : String -> Decoder T.TMReq
        decide x = case x of
            "RunTemplateMatching" -> D.field "contents" <| D.map T.RunTemplateMatching decodeTMInput
            "CancelTemplateMatching" -> D.succeed T.CancelTemplateMatching
            "DeleteMatchedGlyph" -> D.field "contents" <| D.map T.DeleteMatchedGlyph decodeMatchedGlyph
            "GenAvg" -> D.field "contents" <| D.map T.GenAvg (D.list decodeMatchedGlyph)
            "DeleteAvg" -> D.field "contents" <| D.map T.DeleteAvg decodeAvg
            "CancelGenAvg" -> D.succeed T.CancelGenAvg
            c -> D.fail <| "TMReq doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeRepoModel : Decoder T.RepoModel
decodeRepoModel = D.succeed T.RepoModel
    |> required "projectRepo" (D.list decodeProject)
    |> required "version" D.string

decodeRepoReq : Decoder T.RepoReq
decodeRepoReq =
    let decide : String -> Decoder T.RepoReq
        decide x = case x of
            "CreateProject" -> D.field "contents" <| D.map T.CreateProject decodeCreateProjectInput
            "RenameProject" -> D.field "contents" <| D.map2 T.RenameProject (D.index 0 decodeProjectId) (D.index 1 D.string)
            "DeleteProject" -> D.field "contents" <| D.map T.DeleteProject decodeProjectId
            c -> D.fail <| "RepoReq doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeProjectId : Decoder T.ProjectId
decodeProjectId = D.field "contents" <| D.map T.ProjectId D.string

decodeCreateProjectInput : Decoder T.CreateProjectInput
decodeCreateProjectInput = D.succeed T.CreateProjectInput
    |> required "directory" D.string
    |> required "name" D.string

decodeProject : Decoder T.Project
decodeProject = D.succeed T.Project
    |> required "templates" (D.list decodeImage)
    |> required "sources" (D.list decodeImage)
    |> required "name" D.string
    |> required "directory" D.string
    |> required "createdAt" D.int
    |> required "updatedAt" D.int
    |> required "glyphCollections" (D.list decodeGlyphCollection)
    |> required "id" decodeProjectId
