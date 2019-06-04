{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase  #-}

module App where

import           Control.Concurrent
import qualified Control.Exception
import           Control.Exception              ( SomeException )
import           Control.Monad                  ( foldM
                                                , forever
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Aeson                    as J
import           Data.Function                  ( (&) )
import           Debug
import qualified Network.WebSockets            as WS
import qualified Project
import qualified Repo
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           GHC.Generics
import           Elm
import qualified Repo
import Exception (catchAny)

data Model = Model { mProject :: Maybe Project.Model
                     , mRepo :: Repo.Model
                   }
    deriving (Generic, Show)
    deriving (Elm, ToJSON) via ElmStreet Model

mkInitialModel :: IO Model
mkInitialModel = do
    repo <- Repo.init
    return $ Model { mProject = Nothing, mRepo = repo }

data Req =
    ProjectReq Project.Req
    | RepoReq Repo.Req
    | LoadProject Repo.Project
    | UnloadProject
    deriving (Generic)
    deriving (Elm, ToJSON, FromJSON) via ElmStreet Req

instance Show Req where
  show = \case
    ProjectReq _ -> "ProjectReq"
    RepoReq _ -> "RepoReq"
    LoadProject _ -> "LoadProject"
    UnloadProject  -> "UnloadProject"


data Res =
   Error String
  | Ready Model
  | ModelUpdated Model
  | BadReq
    deriving (Generic, Show)
    deriving (Elm, ToJSON) via ElmStreet Res

data Msg
  = HandleReq Req
  | HandleInvalidReq
  | ProjectMsg Project.Msg
  | RepoMsg Repo.Msg
  | OpenProject Repo.Project
  | CloseProject

data Ctx = Ctx
      { respond :: Res -> IO ()
        , trigger :: Msg -> IO ()
      }

reqToMsg :: Req -> Msg
reqToMsg req = case req of
    ProjectReq  subReq -> ProjectMsg $ Project.HandleReq subReq
    RepoReq     subReq -> RepoMsg $ Repo.HandleReq subReq
    LoadProject pid    -> OpenProject pid
    UnloadProject      -> CloseProject

update :: Ctx -> Msg -> Model -> IO Model
update ctx@Ctx {..} msg model = do
    newModel <- applyUpdate `catchAny` \e -> do
        respond (Error $ Control.Exception.displayException e)
        return model
    -- Project.Repo.save (repo newModel)
    respond $ ModelUpdated newModel
    return newModel
  where
    projectCtx = Project.Ctx (trigger . ProjectMsg)
                             (trigger . RepoMsg . Repo.UpdateProject)
    applyUpdate = case msg of
        HandleReq req ->
            update ctx (reqToMsg req) model

        ProjectMsg subMsg -> case mProject model of
            Just subModel -> do
                newProjectModel <- Project.update projectCtx subMsg subModel
                return $ model { mProject = Just newProjectModel }

            Nothing -> return model

        RepoMsg subMsg -> do
            let repoCtx = Repo.Ctx { onProjectCreated = trigger . OpenProject }

            newRepo <- Repo.update repoCtx subMsg (mRepo model)
            return $ model { mRepo = newRepo }

        OpenProject project -> do
            let projectState = Just $ Project.initModel project
            return $ model { mProject = projectState }

        CloseProject -> case mProject model of
            Just subModel -> do
                _ <- Project.update projectCtx Project.ShutDown subModel
                return $ model { mProject = Nothing }
            Nothing -> return model

        HandleInvalidReq -> do
            respond BadReq
            return model


loop :: Ctx -> MVar Model -> IO Msg -> IO ()
loop ctx stateMVar getMsg = do
    action   <- getMsg
    state    <- takeMVar stateMVar
    newModel <- update ctx action state
    putMVar stateMVar newModel
    loop ctx stateMVar getMsg
    return ()

app :: WS.ServerApp
app pending = do
    conn         <- WS.acceptRequest pending
    actionsChan  <- newChan
    initialModel <- mkInitialModel
    stateMVar    <- newMVar initialModel
    let respond = WS.sendTextData conn . J.encode
        ctx     = Ctx { respond = respond, trigger = writeChan actionsChan }
    _ <- forkIO $ loop ctx stateMVar (readChan actionsChan)
    respond $ Ready initialModel
    loop ctx stateMVar $ do
        msg <- WS.receiveData conn
        return $ case J.decode msg of
            Just req -> HandleReq req
            Nothing  -> HandleInvalidReq

main :: IO ()
main = do
    putStrLn "WS server running on 9160"
    WS.runServer "0.0.0.0" 9160 app
-- handleRequest respond req pendingJob
