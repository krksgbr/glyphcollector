{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE RecordWildCards  #-}

module Repo where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           GHC.Generics
import           Elm
import qualified AppData
import qualified Data.Aeson                    as Aeson
import qualified Data.Map                      as Map
import           Data.Map                       ( Map
                                                , (!?)
                                                )
import qualified Data.List                     as List

import           Data.Function                  ( (&) )
import           Control.Monad                  ( guard )
import           Control.Exception              ( tryJust
                                                , throwIO
                                                )
import           System.IO.Error                ( isDoesNotExistError )
import System.FilePath ((</>))
import qualified System.Directory as Directory
import           Exception                      ( Failure(..) )
import           Utils
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID

import Project.Image (Image)
import           Project.GlyphCollection                  ( GlyphCollection
                                                )

newtype ProjectId = ProjectId String
  deriving (Generic, Show, Eq)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet ProjectId

data Project = Project { pTemplates ::  [Image]
                         , pSources :: [Image]
                         , pName :: String
                         , pDirectory :: FilePath
                         , pCreatedAt :: Int
                         , pUpdatedAt :: Int
                         , pGlyphCollections :: [GlyphCollection]
                         , pId :: ProjectId
                       }
  deriving (Generic, Show)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet Project


data CreateProjectInput = CreateProjectInput { cpDirectory :: FilePath
                                             , cpName :: String
                                             }
  deriving (Generic, Show)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet CreateProjectInput



data RepoModel = RepoModel { mProjectRepo :: [Project]
                   , mVersion :: String
                   }
    deriving (Generic, Show)
    deriving (Elm, ToJSON, FromJSON) via ElmStreet RepoModel

type Model = RepoModel


data RepoReq =
  CreateProject CreateProjectInput
  | RenameProject ProjectId String
  | DeleteProject ProjectId
  deriving (Generic, Show)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet RepoReq

type Req = RepoReq

data Msg =
  HandleReq Req
  | UpdateProject Project


data Ctx = Ctx { onProjectCreated :: Project -> IO ()
               }


init :: IO Model
init = do
    repoPath <- path
    r <- tryJust (guard . isDoesNotExistError) (Aeson.decodeFileStrict repoPath)
    case r of
        Left  _           -> return initialModel
        Right Nothing     -> throwIO $ Failure "Could not decode repo."
        Right (Just repo) -> return repo
  where
    initialModel :: Model
    initialModel = RepoModel { mProjectRepo = [], mVersion = "1.0" }


mkNewProject :: CreateProjectInput -> IO Project
mkNewProject input = do
    now  <- Utils.getTimestamp
    uuid <- UUID.nextRandom
    pDirectory <- Utils.mkdirp $ (cpDirectory input) </> (cpName input)
    return $ Project { pTemplates    = []
                     , pSources      = []
                     , pGlyphCollections = []
                     , pName         = (cpName input)
                     , pDirectory    = pDirectory
                     , pCreatedAt    = now
                     , pUpdatedAt    = now
                     , pId           = ProjectId $ UUID.toString uuid
                     }


updateProject :: ProjectId -> [Project] -> (Project -> Project) -> [Project]
updateProject projectId projects updateFn = projects & List.map
    (\project -> if pId project == projectId then updateFn project else project)



update :: Ctx -> Msg -> Model -> IO Model
update Ctx {..} msg model = do
    newModel <- applyUpdate
    save newModel
    return newModel
  where
    applyUpdate = case msg of
        HandleReq (CreateProject input) -> do
            newProject <- mkNewProject input
            onProjectCreated newProject
            return $ model { mProjectRepo = mProjectRepo model ++ [newProject] }

        HandleReq (DeleteProject projectId) ->
            let maybeProject = List.find (\p -> (pId p == projectId)) (mProjectRepo model)
            in
            case maybeProject of
              Just project -> do
                  dataPath <- AppData.path [pName project] -- TODO this is defined in src/Project as well (getDataPath)
                                                           -- Also if people can rename their projects, this will break
                  Directory.removeDirectoryRecursive dataPath
                  return $ model { mProjectRepo =
                                     List.filter (\p -> (pId p /= projectId))
                                       (mProjectRepo model)
                         }
              Nothing ->
                return model


        HandleReq (RenameProject projectId newName) ->
            let newProjects = updateProject projectId (mProjectRepo model)
                    $ \project -> project { pName = newName }
            in  return $ model { mProjectRepo = newProjects }

        UpdateProject newProject ->
            let
                newProjects = updateProject (pId newProject) (mProjectRepo model)
                    $ const newProject
            in  return $ model { mProjectRepo = newProjects }




path :: IO FilePath
path = AppData.path ["repo.json"]


save :: Model -> IO ()
save repo = do
    repoPath <- path
    Aeson.encodeFile repoPath repo
