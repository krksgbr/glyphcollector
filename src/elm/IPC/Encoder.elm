module IPC.Encoder exposing (encodeAvg, encodeCreateProjectInput, encodeGlyphCollection, encodeImPModel, encodeImage, encodeImageStore, encodeMatchedGlyph, encodeModel, encodePendingImage, encodeProject, encodeProjectId, encodeProjectModel, encodeProjectReq, encodeProjectView, encodeRepoModel, encodeRepoReq, encodeReq, encodeRes, encodeTMInput, encodeTMProcess, encodeTMReq, encodeTMStatus)

import IPC.ElmStreet exposing (..)
import IPC.Types exposing (..)
import Iso8601 as Iso
import Json.Encode as E exposing (..)


encodeModel : Model -> Value
encodeModel x =
    E.object
        [ ( "project", elmStreetEncodeMaybe encodeProjectModel x.project )
        , ( "repo", encodeRepoModel x.repo )
        ]


encodeReq : Req -> Value
encodeReq x =
    E.object <|
        case x of
            ProjectReq x1 ->
                [ ( "tag", E.string "ProjectReq" ), ( "contents", encodeProjectReq x1 ) ]

            RepoReq x1 ->
                [ ( "tag", E.string "RepoReq" ), ( "contents", encodeRepoReq x1 ) ]

            LoadProject x1 ->
                [ ( "tag", E.string "LoadProject" ), ( "contents", encodeProject x1 ) ]

            UnloadProject ->
                [ ( "tag", E.string "UnloadProject" ), ( "contents", E.list identity [] ) ]


encodeRes : Res -> Value
encodeRes x =
    E.object <|
        case x of
            Error x1 ->
                [ ( "tag", E.string "Error" ), ( "contents", E.string x1 ) ]

            Ready x1 ->
                [ ( "tag", E.string "Ready" ), ( "contents", encodeModel x1 ) ]

            ModelUpdated x1 ->
                [ ( "tag", E.string "ModelUpdated" ), ( "contents", encodeModel x1 ) ]

            BadReq ->
                [ ( "tag", E.string "BadReq" ), ( "contents", E.list identity [] ) ]


encodeProjectModel : ProjectModel -> Value
encodeProjectModel x =
    E.object
        [ ( "templates", encodeImageStore x.templates )
        , ( "sources", encodeImageStore x.sources )
        , ( "name", E.string x.name )
        , ( "createdAt", E.int x.createdAt )
        , ( "updatedAt", E.int x.updatedAt )
        , ( "imP", encodeImPModel x.imP )
        , ( "directory", E.string x.directory )
        , ( "id", encodeProjectId x.id )
        , ( "view", encodeProjectView x.view )
        ]


encodeProjectReq : ProjectReq -> Value
encodeProjectReq x =
    E.object <|
        case x of
            ImPReq x1 ->
                [ ( "tag", E.string "ImPReq" ), ( "contents", encodeTMReq x1 ) ]

            ImportTemplates x1 ->
                [ ( "tag", E.string "ImportTemplates" ), ( "contents", E.list E.string x1 ) ]

            CancelImportTemplates x1 ->
                [ ( "tag", E.string "CancelImportTemplates" ), ( "contents", E.list encodePendingImage x1 ) ]

            ImportSources x1 ->
                [ ( "tag", E.string "ImportSources" ), ( "contents", E.list E.string x1 ) ]

            CancelImportSources x1 ->
                [ ( "tag", E.string "CancelImportSources" ), ( "contents", E.list encodePendingImage x1 ) ]

            DeleteTemplates x1 ->
                [ ( "tag", E.string "DeleteTemplates" ), ( "contents", E.list encodeImage x1 ) ]

            DeleteSources x1 ->
                [ ( "tag", E.string "DeleteSources" ), ( "contents", E.list encodeImage x1 ) ]

            ViewReq x1 ->
                [ ( "tag", E.string "ViewReq" ), ( "contents", encodeProjectView x1 ) ]

            OpenCollectionsDirectory x1 ->
                [ ( "tag", E.string "OpenCollectionsDirectory" ), ( "contents", E.string x1 ) ]

            OpenAvgsDirectory x1 ->
                [ ( "tag", E.string "OpenAvgsDirectory" ), ( "contents", E.string x1 ) ]


encodeImage : Image -> Value
encodeImage x =
    E.object
        [ ( "thumbnail", E.string x.thumbnail )
        , ( "original", E.string x.original )
        , ( "name", E.string x.name )
        , ( "id", E.string x.id )
        ]


encodePendingImage : PendingImage -> Value
encodePendingImage x =
    E.object
        [ ( "filePath", E.string x.filePath )
        , ( "fileName", E.string x.fileName )
        ]


encodeImageStore : ImageStore -> Value
encodeImageStore x =
    E.object
        [ ( "pending", (elmStreetEncodeMaybe <| E.list encodePendingImage) x.pending )
        , ( "imported", E.list encodeImage x.imported )
        ]


encodeGlyphCollection : GlyphCollection -> Value
encodeGlyphCollection x =
    E.object
        [ ( "glyphName", E.string x.glyphName )
        , ( "matches", E.list encodeMatchedGlyph x.matches )
        , ( "averages", E.list encodeAvg x.averages )
        ]


encodeMatchedGlyph : MatchedGlyph -> Value
encodeMatchedGlyph x =
    E.object
        [ ( "image", encodeImage x.image )
        , ( "score", E.float x.score )
        , ( "sourceImage", encodeImage x.sourceImage )
        , ( "templateImage", encodeImage x.templateImage )
        , ( "glyphName", E.string x.glyphName )
        ]


encodeAvg : Avg -> Value
encodeAvg x =
    E.object
        [ ( "image", encodeImage x.image )
        , ( "glyphName", E.string x.glyphName )
        ]


encodeProjectView : ProjectView -> Value
encodeProjectView x =
    E.object <|
        case x of
            Sources ->
                [ ( "tag", E.string "Sources" ), ( "contents", E.list identity [] ) ]

            Collections x1 ->
                [ ( "tag", E.string "Collections" ), ( "contents", E.string x1 ) ]


encodeTMInput : TMInput -> Value
encodeTMInput x =
    E.object
        [ ( "templates", E.list encodeImage x.templates )
        , ( "sources", E.list encodeImage x.sources )
        ]


encodeTMStatus : TMStatus -> Value
encodeTMStatus x =
    E.object
        [ ( "source", encodeImage x.source )
        , ( "template", encodeImage x.template )
        , ( "pct", E.float x.pct )
        ]


encodeTMProcess : TMProcess -> Value
encodeTMProcess x =
    E.object
        [ ( "threadId", E.string x.threadId )
        , ( "status", elmStreetEncodeMaybe encodeTMStatus x.status )
        ]


encodeImPModel : ImPModel -> Value
encodeImPModel x =
    E.object
        [ ( "collections", E.list encodeGlyphCollection x.collections )
        , ( "process", elmStreetEncodeMaybe encodeTMProcess x.process )
        , ( "genAvgProcess", elmStreetEncodeMaybe (elmStreetEncodePair E.string E.string) x.genAvgProcess )
        ]


encodeTMReq : TMReq -> Value
encodeTMReq x =
    E.object <|
        case x of
            RunTemplateMatching x1 ->
                [ ( "tag", E.string "RunTemplateMatching" ), ( "contents", encodeTMInput x1 ) ]

            CancelTemplateMatching ->
                [ ( "tag", E.string "CancelTemplateMatching" ), ( "contents", E.list identity [] ) ]

            DeleteMatchedGlyph x1 ->
                [ ( "tag", E.string "DeleteMatchedGlyph" ), ( "contents", encodeMatchedGlyph x1 ) ]

            GenAvg x1 ->
                [ ( "tag", E.string "GenAvg" ), ( "contents", E.list encodeMatchedGlyph x1 ) ]

            DeleteAvg x1 ->
                [ ( "tag", E.string "DeleteAvg" ), ( "contents", encodeAvg x1 ) ]

            CancelGenAvg ->
                [ ( "tag", E.string "CancelGenAvg" ), ( "contents", E.list identity [] ) ]


encodeRepoModel : RepoModel -> Value
encodeRepoModel x =
    E.object
        [ ( "projectRepo", E.list encodeProject x.projectRepo )
        , ( "version", E.string x.version )
        ]


encodeRepoReq : RepoReq -> Value
encodeRepoReq x =
    E.object <|
        case x of
            CreateProject x1 ->
                [ ( "tag", E.string "CreateProject" ), ( "contents", encodeCreateProjectInput x1 ) ]

            RenameProject x1 x2 ->
                [ ( "tag", E.string "RenameProject" ), ( "contents", E.list identity [ encodeProjectId x1, E.string x2 ] ) ]

            DeleteProject x1 ->
                [ ( "tag", E.string "DeleteProject" ), ( "contents", encodeProjectId x1 ) ]


encodeProjectId : ProjectId -> Value
encodeProjectId =
    E.string << unProjectId


encodeCreateProjectInput : CreateProjectInput -> Value
encodeCreateProjectInput x =
    E.object
        [ ( "directory", E.string x.directory )
        , ( "name", E.string x.name )
        ]


encodeProject : Project -> Value
encodeProject x =
    E.object
        [ ( "templates", E.list encodeImage x.templates )
        , ( "sources", E.list encodeImage x.sources )
        , ( "name", E.string x.name )
        , ( "directory", E.string x.directory )
        , ( "createdAt", E.int x.createdAt )
        , ( "updatedAt", E.int x.updatedAt )
        , ( "glyphCollections", E.list encodeGlyphCollection x.glyphCollections )
        , ( "id", encodeProjectId x.id )
        ]
