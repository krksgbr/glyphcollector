module IPC.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import IPC.ElmStreet exposing (..)
import IPC.Types as T


encodeModel : T.Model -> Value
encodeModel x = E.object
    [ ("tag", E.string "Model")
    , ("project", (elmStreetEncodeMaybe encodeProjectModel) x.project)
    , ("repo", encodeRepoModel x.repo)
    ]

encodeReq : T.Req -> Value
encodeReq x = E.object <| case x of
    T.ProjectReq x1 -> [("tag", E.string "ProjectReq"), ("contents", encodeProjectReq x1)]
    T.RepoReq x1 -> [("tag", E.string "RepoReq"), ("contents", encodeRepoReq x1)]
    T.LoadProject x1 -> [("tag", E.string "LoadProject"), ("contents", encodeProject x1)]
    T.UnloadProject  -> [("tag", E.string "UnloadProject"), ("contents", E.list identity [])]

encodeRes : T.Res -> Value
encodeRes x = E.object <| case x of
    T.Error x1 -> [("tag", E.string "Error"), ("contents", E.string x1)]
    T.Ready x1 -> [("tag", E.string "Ready"), ("contents", encodeModel x1)]
    T.ModelUpdated x1 -> [("tag", E.string "ModelUpdated"), ("contents", encodeModel x1)]
    T.BadReq  -> [("tag", E.string "BadReq"), ("contents", E.list identity [])]

encodeProjectModel : T.ProjectModel -> Value
encodeProjectModel x = E.object
    [ ("tag", E.string "ProjectModel")
    , ("templates", encodeImageStore x.templates)
    , ("sources", encodeImageStore x.sources)
    , ("name", E.string x.name)
    , ("createdAt", E.int x.createdAt)
    , ("updatedAt", E.int x.updatedAt)
    , ("imP", encodeImPModel x.imP)
    , ("directory", E.string x.directory)
    , ("id", encodeProjectId x.id)
    , ("view", encodeProjectView x.view)
    ]

encodeProjectReq : T.ProjectReq -> Value
encodeProjectReq x = E.object <| case x of
    T.ImPReq x1 -> [("tag", E.string "ImPReq"), ("contents", encodeTMReq x1)]
    T.ImportTemplates x1 -> [("tag", E.string "ImportTemplates"), ("contents", (E.list E.string) x1)]
    T.CancelImportTemplates x1 -> [("tag", E.string "CancelImportTemplates"), ("contents", (E.list encodePendingImage) x1)]
    T.ImportSources x1 -> [("tag", E.string "ImportSources"), ("contents", (E.list E.string) x1)]
    T.CancelImportSources x1 -> [("tag", E.string "CancelImportSources"), ("contents", (E.list encodePendingImage) x1)]
    T.DeleteTemplates x1 -> [("tag", E.string "DeleteTemplates"), ("contents", (E.list encodeImage) x1)]
    T.DeleteSources x1 -> [("tag", E.string "DeleteSources"), ("contents", (E.list encodeImage) x1)]
    T.ViewReq x1 -> [("tag", E.string "ViewReq"), ("contents", encodeProjectView x1)]
    T.OpenCollectionsDirectory x1 -> [("tag", E.string "OpenCollectionsDirectory"), ("contents", E.string x1)]
    T.OpenAvgsDirectory x1 -> [("tag", E.string "OpenAvgsDirectory"), ("contents", E.string x1)]

encodeImage : T.Image -> Value
encodeImage x = E.object
    [ ("tag", E.string "Image")
    , ("thumbnail", E.string x.thumbnail)
    , ("original", E.string x.original)
    , ("name", E.string x.name)
    , ("id", E.string x.id)
    ]

encodePendingImage : T.PendingImage -> Value
encodePendingImage x = E.object
    [ ("tag", E.string "PendingImage")
    , ("filePath", E.string x.filePath)
    , ("fileName", E.string x.fileName)
    ]

encodeImageStore : T.ImageStore -> Value
encodeImageStore x = E.object
    [ ("tag", E.string "ImageStore")
    , ("pending", (elmStreetEncodeMaybe (E.list encodePendingImage)) x.pending)
    , ("imported", (E.list encodeImage) x.imported)
    ]

encodeGlyphCollection : T.GlyphCollection -> Value
encodeGlyphCollection x = E.object
    [ ("tag", E.string "GlyphCollection")
    , ("glyphName", E.string x.glyphName)
    , ("matches", (E.list encodeMatchedGlyph) x.matches)
    , ("averages", (E.list encodeAvg) x.averages)
    ]

encodeMatchedGlyph : T.MatchedGlyph -> Value
encodeMatchedGlyph x = E.object
    [ ("tag", E.string "MatchedGlyph")
    , ("image", encodeImage x.image)
    , ("score", E.float x.score)
    , ("sourceImage", encodeImage x.sourceImage)
    , ("templateImage", encodeImage x.templateImage)
    , ("glyphName", E.string x.glyphName)
    ]

encodeAvg : T.Avg -> Value
encodeAvg x = E.object
    [ ("tag", E.string "Avg")
    , ("image", encodeImage x.image)
    , ("glyphName", E.string x.glyphName)
    ]

encodeProjectView : T.ProjectView -> Value
encodeProjectView x = E.object <| case x of
    T.Sources  -> [("tag", E.string "Sources"), ("contents", E.list identity [])]
    T.Collections x1 -> [("tag", E.string "Collections"), ("contents", E.string x1)]

encodeTMInput : T.TMInput -> Value
encodeTMInput x = E.object
    [ ("tag", E.string "TMInput")
    , ("templates", (E.list encodeImage) x.templates)
    , ("sources", (E.list encodeImage) x.sources)
    ]

encodeTMStatus : T.TMStatus -> Value
encodeTMStatus x = E.object
    [ ("tag", E.string "TMStatus")
    , ("source", encodeImage x.source)
    , ("template", encodeImage x.template)
    , ("pct", E.float x.pct)
    ]

encodeTMProcess : T.TMProcess -> Value
encodeTMProcess x = E.object
    [ ("tag", E.string "TMProcess")
    , ("threadId", E.string x.threadId)
    , ("status", (elmStreetEncodeMaybe encodeTMStatus) x.status)
    ]

encodeImPModel : T.ImPModel -> Value
encodeImPModel x = E.object
    [ ("tag", E.string "ImPModel")
    , ("collections", (E.list encodeGlyphCollection) x.collections)
    , ("process", (elmStreetEncodeMaybe encodeTMProcess) x.process)
    , ("genAvgProcess", (elmStreetEncodeMaybe (elmStreetEncodePair E.string E.string)) x.genAvgProcess)
    ]

encodeTMReq : T.TMReq -> Value
encodeTMReq x = E.object <| case x of
    T.RunTemplateMatching x1 -> [("tag", E.string "RunTemplateMatching"), ("contents", encodeTMInput x1)]
    T.CancelTemplateMatching  -> [("tag", E.string "CancelTemplateMatching"), ("contents", E.list identity [])]
    T.DeleteMatchedGlyph x1 -> [("tag", E.string "DeleteMatchedGlyph"), ("contents", encodeMatchedGlyph x1)]
    T.GenAvg x1 -> [("tag", E.string "GenAvg"), ("contents", (E.list encodeMatchedGlyph) x1)]
    T.DeleteAvg x1 -> [("tag", E.string "DeleteAvg"), ("contents", encodeAvg x1)]
    T.CancelGenAvg  -> [("tag", E.string "CancelGenAvg"), ("contents", E.list identity [])]

encodeRepoModel : T.RepoModel -> Value
encodeRepoModel x = E.object
    [ ("tag", E.string "RepoModel")
    , ("projectRepo", (E.list encodeProject) x.projectRepo)
    , ("version", E.string x.version)
    ]

encodeRepoReq : T.RepoReq -> Value
encodeRepoReq x = E.object <| case x of
    T.CreateProject x1 -> [("tag", E.string "CreateProject"), ("contents", encodeCreateProjectInput x1)]
    T.RenameProject x1 x2 -> [("tag", E.string "RenameProject"), ("contents", E.list identity [encodeProjectId x1, E.string x2])]
    T.DeleteProject x1 -> [("tag", E.string "DeleteProject"), ("contents", encodeProjectId x1)]

encodeProjectId : T.ProjectId -> Value
encodeProjectId pId = E.object [ ( "tag", E.string "ProjectId" ), ( "contents", E.string <| T.unProjectId pId ) ]

encodeCreateProjectInput : T.CreateProjectInput -> Value
encodeCreateProjectInput x = E.object
    [ ("tag", E.string "CreateProjectInput")
    , ("directory", E.string x.directory)
    , ("name", E.string x.name)
    ]

encodeProject : T.Project -> Value
encodeProject x = E.object
    [ ("tag", E.string "Project")
    , ("templates", (E.list encodeImage) x.templates)
    , ("sources", (E.list encodeImage) x.sources)
    , ("name", E.string x.name)
    , ("directory", E.string x.directory)
    , ("createdAt", E.int x.createdAt)
    , ("updatedAt", E.int x.updatedAt)
    , ("glyphCollections", (E.list encodeGlyphCollection) x.glyphCollections)
    , ("id", encodeProjectId x.id)
    ]
