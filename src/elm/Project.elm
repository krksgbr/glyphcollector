module Project exposing
    ( cancelTemplateMatching
    , deleteAvgs
    , deleteMatchedGlyphs
    , deleteSources
    , deleteTemplates
    , genAvg
    , importSources
    , importTemplates
    , matchTemplate
    , rename
    , setView
    , showAvgsDirectory
    , showCollectionsDirectory
    , thumbnail
    )

import Avg
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import File exposing (File)
import IPC
import IPC.Types exposing (..)
import Image
import UI.Background exposing (PositionX(..), PositionY(..), Size(..))
import UI.Button as Button
import UI.Color as Color


thumbnail :
    List (Attribute msg)
    ->
        { image : String
        , name : String
        , onClick : msg
        , onRemove : msg
        }
    -> Element msg
thumbnail attrs ({ image, name } as config) =
    let
        backgroundImage =
            UI.Background.image (File.pathToUrl image) ( CenterX, CenterY ) Contain
    in
    column
        ([ spacing 10
         ]
            ++ attrs
        )
        [ el
            [ Background.color Color.white
            , Border.rounded 10
            , onClick <| config.onClick
            ]
          <|
            el
                [ backgroundImage
                , width <| px 150
                , height <| px 150
                , Border.width 2
                , Border.rounded 10
                ]
            <|
                none
        , row [ spaceEvenly, width fill ]
            [ text name
            , el
                [ onClick <| config.onRemove
                , mouseOver
                    [ Font.color Color.red
                    ]
                ]
              <|
                text "x"
            ]
        ]



--- PUBLIC


mkReq =
    IPC.sendReq << ProjectReq


rename : String -> Cmd msg
rename newName =
    Cmd.none



-- Debug.todo "implement Project.updateName"


importTemplates : List File -> Cmd msg
importTemplates =
    importImages ImportTemplates


importSources : List File -> Cmd msg
importSources =
    importImages ImportSources


importImages : (List String -> ProjectReq) -> List File -> Cmd msg
importImages req files =
    let
        filePaths =
            files
                |> List.map .path
    in
    mkReq <| req <| filePaths


deleteTemplates : List Image.Promise -> Cmd msg
deleteTemplates templates =
    templates
        |> List.map
            (\template ->
                case template of
                    Image.Resolved image ->
                        mkReq <| DeleteTemplates <| [ image ]

                    Image.Pending p ->
                        mkReq <| CancelImportTemplates <| [ p ]
            )
        |> Cmd.batch


deleteSources : List Image.Promise -> Cmd msg
deleteSources sources =
    sources
        |> List.map
            (\source ->
                case source of
                    Image.Resolved image ->
                        mkReq <| DeleteSources <| [ image ]

                    Image.Pending p ->
                        mkReq <| CancelImportSources <| [ p ]
            )
        |> Cmd.batch


deleteMatchedGlyphs : List MatchedGlyph -> Cmd msg
deleteMatchedGlyphs mgs =
    mgs
        |> List.map (mkReq << ImPReq << DeleteMatchedGlyph)
        |> Cmd.batch


genAvg : List MatchedGlyph -> Cmd msg
genAvg =
    mkReq << ImPReq << GenAvg


deleteAvgs : List Avg.Promise -> Cmd msg
deleteAvgs avgps =
    avgps
        |> List.map
            (\avgp ->
                case avgp of
                    Avg.Resolved avg ->
                        mkReq <| ImPReq <| DeleteAvg avg

                    Avg.Pending _ ->
                        mkReq <| ImPReq CancelGenAvg
            )
        |> Cmd.batch


matchTemplate : TMInput -> Cmd msg
matchTemplate =
    mkReq << ImPReq << RunTemplateMatching


cancelTemplateMatching : Cmd msg
cancelTemplateMatching =
    mkReq <| ImPReq CancelTemplateMatching


setView : ProjectView -> Cmd msg
setView =
    mkReq << ViewReq


showAvgsDirectory : String -> Cmd msg
showAvgsDirectory =
    mkReq << OpenAvgsDirectory


showCollectionsDirectory : String -> Cmd msg
showCollectionsDirectory =
    mkReq << OpenCollectionsDirectory
