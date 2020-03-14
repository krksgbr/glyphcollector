module Release exposing (..)

import AppSettings
import Electron
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Http
import Json.Decode as JD
import Return exposing (Return)
import UI.Button as Button
import UI.Color as Color
import UI.Layout as Layout


type alias Model =
    { newReleaseTag : Maybe String
    }


type alias Manifest =
    { os : String
    , version : String
    }


type Msg
    = CheckedForUpdate (Maybe String)
    | DownloadRequested
    | NewReleaseIgnored String


init : Manifest -> Return Msg Model
init manifest =
    { newReleaseTag = Nothing }
        |> Return.singleton
        |> Return.command (checkForUpdates manifest)


type alias Release =
    { tag : String
    , assets : List String
    }


addTagToIgnoredTags : AppSettings.Settings -> String -> Cmd msg
addTagToIgnoredTags settings tag =
    if List.member tag settings.ignoredReleaseTags then
        Cmd.none

    else
        AppSettings.updateAppSettings
            { settings
                | ignoredReleaseTags = tag :: settings.ignoredReleaseTags
            }


update : Msg -> Model -> AppSettings.Settings -> Return Msg Model
update msg model settings =
    case msg of
        CheckedForUpdate maybeNewVersion ->
            { model
                | newReleaseTag =
                    maybeNewVersion
                        |> Maybe.andThen
                            (\releaseTag ->
                                if List.member releaseTag settings.ignoredReleaseTags then
                                    Nothing

                                else
                                    Just releaseTag
                            )
            }
                |> Return.singleton

        DownloadRequested ->
            { model | newReleaseTag = Nothing }
                |> Return.singleton
                |> Return.command
                    (Cmd.batch
                        [ Electron.openExternalUrl
                            "https://glyphcollector.app"
                        ]
                    )

        NewReleaseIgnored tag ->
            { model | newReleaseTag = Nothing }
                |> Return.singleton
                |> Return.command (addTagToIgnoredTags settings tag)


decodeRelease : JD.Decoder Release
decodeRelease =
    JD.map2 Release
        (JD.field "tag_name" JD.string)
        (JD.field "assets" <| JD.list <| JD.field "browser_download_url" JD.string)


checkForUpdates : Manifest -> Cmd Msg
checkForUpdates manifest =
    let
        stringContains =
            String.toLower >> String.contains

        searchString =
            if stringContains manifest.os "linux" then
                "deb"

            else if stringContains manifest.os "darwin" then
                "mac"

            else
                "exe"

        isNewerRelease release =
            compare (String.replace "v" "" release.tag) manifest.version == GT

        assetForOS release =
            release.assets
                |> List.filter (stringContains searchString)
                |> List.head
                |> Maybe.map (always True)
                |> Maybe.withDefault False

        maybeNewVersion : Release -> Maybe String
        maybeNewVersion release =
            if isNewerRelease release && assetForOS release then
                Just release.tag

            else
                Nothing

        toMsg =
            Result.withDefault Nothing
                >> CheckedForUpdate
    in
    Http.get
        { url = "https://api.github.com/repos/krksgbr/glyphcollector/releases/latest"
        , expect =
            Http.expectJson toMsg
                (decodeRelease
                    |> JD.map maybeNewVersion
                )
        }


view : Model -> Maybe (Element Msg)
view model =
    model.newReleaseTag
        |> Maybe.map
            (\releaseTag ->
                row
                    [ width fill
                    , Background.color Color.accent
                    , Font.color Color.white
                    , paddingXY Layout.global.paddingX 0
                    , height <| px Layout.workspace.headerHeight
                    ]
                    [ text "A new version of GlyphCollector is available."
                    , row [ alignRight, spacing 20 ]
                        [ Button.text "Download"
                            [ mouseOver [ Font.color Color.black ]
                            , onClick DownloadRequested
                            ]
                        , Button.text "Ignore"
                            [ mouseOver [ Font.color Color.black ]
                            , onClick (NewReleaseIgnored releaseTag)
                            ]
                        ]
                    ]
            )
