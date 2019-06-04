module Repo exposing (createProject, deleteProject)

import IPC
import IPC.Types exposing (ProjectId, RepoReq(..), Req(..))


type alias CreateProjectInput =
    { name : String
    , directory : String
    }


createProject : CreateProjectInput -> Cmd msg
createProject createProjectInput =
    IPC.sendReq
        (RepoReq <| CreateProject createProjectInput)


deleteProject : ProjectId -> Cmd msg
deleteProject i =
    IPC.sendReq
        (RepoReq <| DeleteProject i)
