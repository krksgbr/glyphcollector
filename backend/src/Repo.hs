{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE BangPatterns  #-}

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
import qualified Data.Text as T

import Project.Image (Image)
import           Project.GlyphCollection                  ( GlyphCollection
                                                )
import Debug

newtype ProjectId = ProjectId T.Text
  deriving (Generic, Show, Eq)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet ProjectId

data Project = Project { pTemplates ::  [Image]
                         , pSources :: [Image]
                         , pName :: T.Text
                         , pDirectory :: T.Text
                         , pCreatedAt :: Int
                         , pUpdatedAt :: Int
                         , pGlyphCollections :: [GlyphCollection]
                         , pId :: ProjectId
                       }
  deriving (Generic, Show)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet Project


data CreateProjectInput = CreateProjectInput { cpDirectory :: T.Text
                                             , cpName :: T.Text
                                             }
  deriving (Generic, Show)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet CreateProjectInput



data RepoModel = RepoModel { mProjectRepo :: [Project]
                   , mVersion :: T.Text
                   }
    deriving (Generic, Show)
    deriving (Elm, ToJSON, FromJSON) via ElmStreet RepoModel

type Model = RepoModel


data RepoReq =
  CreateProject CreateProjectInput
  | RenameProject ProjectId T.Text
  | DeleteProject ProjectId
  deriving (Generic, Show)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet RepoReq

type Req = RepoReq

data Msg =
  HandleReq Req
  | UpdateProject Project


data Ctx = Ctx { onProjectCreated :: Project -> IO ()
               }

loadRepo = do
  repoPath <- path
  result <-  Aeson.eitherDecodeFileStrict $ T.unpack repoPath
  return $ case result of
    Left err ->
      let
        !x = Debug.log "err" err
      in
        Nothing
    Right r ->
        Just r


init :: IO Model
init = do
    repoPath <- path
    r <- tryJust (guard . isDoesNotExistError) loadRepo
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
    pDirectory <- Utils.mkdirp $ T.pack $ (T.unpack $ cpDirectory input) </> (T.unpack $ cpName input)
    return $ Project { pTemplates    = []
                     , pSources      = []
                     , pGlyphCollections = []
                     , pName         = (cpName input)
                     , pDirectory    = pDirectory
                     , pCreatedAt    = now
                     , pUpdatedAt    = now
                     , pId           = ProjectId $ T.pack $ UUID.toString uuid
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

                  _ <- Directory.removeDirectoryRecursive (T.unpack dataPath)
                       -- the project directory might not yet exist
                       -- it is only created when the first asset is added
                       & tryJust (guard . isDoesNotExistError)

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




path :: IO T.Text
path = AppData.path ["repo.json"]


save :: Model -> IO ()
save repo = do
    repoPath <- path
    Aeson.encodeFile (T.unpack repoPath) repo
