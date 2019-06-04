module Home exposing
    ( Model(..)
    , Msg(..)
    , init
    , update
    , view
    )

import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import File exposing (File)
import IPC.Types exposing (Project, Req(..), Res(..))
import Project
import Repo
import Return exposing (Return)
import Return.Extra as Return
import Task
import UI.Attributes exposing (height_, vh, vw)
import UI.Border as Border
import UI.Button as Button
import UI.Color as Color
import UI.Input as Input
import UI.Typography as UI
import Utils


type alias ProjectForm =
    { name : String
    , directory : Maybe File
    }


type alias ValidProjectForm =
    { name : String
    , directory : File
    }


type Model
    = ViewProjects
    | ViewProjectForm ProjectForm


type Msg
    = ProjectThumbnailClicked Project
    | CreateProjectClicked
    | NewProjectNameChanged String
    | NewProjectDirectorySelected File
    | ProjectFormSubmitted ValidProjectForm
    | NewProjectCanceled
    | ProjectRemoved Project
    | Reinit


init : Model
init =
    ViewProjects


update : Msg -> Model -> Return Msg Model
update msg model =
    case ( model, msg ) of
        ( ViewProjectForm projectForm, NewProjectNameChanged newName ) ->
            ViewProjectForm { projectForm | name = newName }
                |> Return.singleton

        ( ViewProjects, ProjectThumbnailClicked project ) ->
            model
                |> Return.singleton
                |> Return.sendReq (LoadProject project)

        ( ViewProjects, CreateProjectClicked ) ->
            ViewProjectForm (ProjectForm "" Nothing)
                |> Return.singleton

        ( ViewProjects, ProjectRemoved project ) ->
            model
                |> Return.singleton
                |> Return.command (Repo.deleteProject project.id)

        ( ViewProjectForm projectForm, NewProjectDirectorySelected newDirectory ) ->
            ViewProjectForm { projectForm | directory = Just newDirectory }
                |> Return.singleton

        ( ViewProjectForm _, ProjectFormSubmitted { directory, name } ) ->
            model
                |> Return.singleton
                |> Return.command
                    (Cmd.batch
                        [ Repo.createProject
                            { directory = directory.path
                            , name = name
                            }

                        -- TODO remove this hack
                        -- createProject implicitly opens the newly created project,
                        -- but there's a small delay till the change propagates to the frontend,
                        -- causing a flash if the model is reinited here immediately,
                        -- hence this delayed update
                        , Utils.timeoutMsg 200 Reinit
                        ]
                    )

        ( ViewProjectForm _, NewProjectCanceled ) ->
            ViewProjects
                |> Return.singleton

        ( _, Reinit ) ->
            init
                |> Return.singleton

        ( _, _ ) ->
            model
                |> Return.singleton


viewProjects : List Project -> Element Msg
viewProjects projects =
    column [ width fill, height fill ]
        [ column
            [ width fill
            , height_ <| vh 50
            , Border.bottom 1
            , padding 20
            ]
            [ el [ centerX, centerY ] <| newProjectButton ]
        , column
            [ width fill
            , height_ <| vh 50
            , paddingXY 0 20
            , spacing 20
            ]
            [ el [ paddingXY 20 0 ] <| UI.h2 "Recent projects"
            , projects
                |> List.map
                    (\project ->
                        Project.thumbnail
                            [ pointer
                            , alignTop
                            ]
                            { image =
                                case project.templates of
                                    t :: _ ->
                                        t.thumbnail

                                    [] ->
                                        ""
                            , name = project.name
                            , onClick = ProjectThumbnailClicked project
                            , onRemove = ProjectRemoved project
                            }
                    )
                |> wrappedRow
                    [ spacing 20
                    , width fill
                    , height fill
                    , paddingXY 20 0
                    ]
            ]
        ]


newProjectButton : Element Msg
newProjectButton =
    Button.custom
        { size = Button.Normal
        , color = Button.Default
        }
        { onPress = Just CreateProjectClicked
        , label = text "+ New Project"
        }


viewEmpty : Element Msg
viewEmpty =
    column [ centerX, centerY ]
        [ newProjectButton
        ]


validateProjectForm : ProjectForm -> Maybe ValidProjectForm
validateProjectForm projectForm =
    if projectForm.name == "" then
        Nothing

    else
        case projectForm.directory of
            Nothing ->
                Nothing

            Just file ->
                Just <| ValidProjectForm projectForm.name file


view : Model -> List Project -> Element Msg
view model projects =
    el [ width fill, height fill ] <|
        case model of
            ViewProjects ->
                case projects of
                    [] ->
                        viewEmpty

                    _ ->
                        viewProjects projects

            ViewProjectForm ({ name, directory } as projectForm) ->
                column [ centerX, centerY, width <| px 500 ]
                    [ column
                        [ Background.color Color.black
                        , height <| px 30
                        , width fill
                        , Font.color Color.white
                        , paddingXY 10 0
                        ]
                        [ el
                            [ centerY
                            , onClick NewProjectCanceled
                            , pointer
                            ]
                          <|
                            text "x"
                        ]
                    , column
                        [ width fill
                        , Background.color Color.white
                        , paddingXY 40 30
                        , spacing 30
                        ]
                        [ Input.textField
                            { onChange = NewProjectNameChanged
                            , onBlur = Nothing
                            , disabled = False
                            , value = name
                            , error = Nothing
                            , showError = False
                            , attributes =
                                { label = "Project Name"
                                , placeholder = ""
                                }
                            }
                        , column [ spacing 20, width fill ]
                            [ Input.directory
                                { label = Button.showAsButton <| text "Select project directory"
                                , id = "fileInput"
                                , onChange = NewProjectDirectorySelected
                                }
                            , directory
                                |> Maybe.map .path
                                |> Maybe.map text
                                |> Maybe.withDefault none
                            ]
                        , Button.custom
                            { size = Button.Normal
                            , color = Button.Accent
                            }
                            { label = text "Create"
                            , onPress =
                                validateProjectForm projectForm
                                    |> Maybe.map ProjectFormSubmitted
                            }
                        ]
                    ]
