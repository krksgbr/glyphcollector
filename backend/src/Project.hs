{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds  #-}

module Project
    ( Image
    , PendingImage
    , Req
    , Model
    , initModel
    , update
    , Ctx(..)
    , Msg(..)
    , ImageStore
    , View
    )
where

import           Control.Monad                  ( forM
                                                , forM_
                                                , foldM
                                                , (>=>)
                                                , filterM
                                                )
import qualified AppData
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           GHC.Generics

import qualified Data.List                     as List
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                , catMaybes
                                                )
import           Data.Function                  ( (&) )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import qualified Data.UUID.V4                  as UUID
import qualified Data.UUID                     as UUID
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.List                     as List
import qualified Data.Text as T

import qualified Utils
import           System.IO.Error                ( isDoesNotExistError )

import           Elm

import qualified System.FilePath               as Path
import           System.FilePath                ( (</>)
                                                , (<.>)
                                                )
import qualified System.Directory              as Directory
import qualified Debug
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                , ThreadId
                                                )

import           Project.GlyphCollection (MatchedGlyph)

import qualified Project.ImageProcessing      as ImP
import qualified Project.Image                 as Image
import           Project.Image                  ( Image(..) )
import qualified ImageProcessing               as Image

import qualified Repo
import           Exception                      ( catchAny )


data ProjectView =
  Sources
  | Collections T.Text
 deriving (Eq, Ord, Generic, Show)
 deriving (Elm, ToJSON, FromJSON) via ElmStreet ProjectView

type View = ProjectView

data ProjectReq =
  ImPReq ImP.Req
  | ImportTemplates [T.Text]
  | CancelImportTemplates [PendingImage]
  | ImportSources [T.Text]
  | CancelImportSources [PendingImage]
  | DeleteTemplates [Image]
  | DeleteSources [Image]
  | ViewReq View
  | OpenCollectionsDirectory T.Text
  | OpenAvgsDirectory T.Text
  deriving (Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet ProjectReq

type Req = ProjectReq

instance Show ProjectReq where
    show = \case
        ImPReq                 subReq -> show (ImPReq subReq)
        ImportTemplates       _      -> "ImportTemplates"
        ImportSources         _      -> "ImportSources"
        DeleteTemplates       _      -> "DeleteTemplates"
        DeleteSources         _      -> "DeleteSources"
        CancelImportTemplates _      -> "CancelImportTemplates"
        CancelImportSources   _      -> "CancelImportSources"
        ViewReq               v      -> "ViewReq " ++ show v
        OpenCollectionsDirectory g                   -> "OpenCollectionsDirectory " ++ (T.unpack g)
        OpenAvgsDirectory g -> "OpenAvgsDirectory " ++ (T.unpack g)


data Msg =
  HandleReq Req
  | TemplateImported Image
  | SourceImported Image
  | ShutDown
  | ImPMsg ImP.Msg
  | SetView View
  deriving (Show)


data PendingImage = PendingImage { piFilePath :: T.Text
                                 , piFileName :: T.Text
                                 }
                  deriving (Eq, Ord, Generic, Show)
                  deriving (Elm, ToJSON, FromJSON) via ElmStreet PendingImage

data ImageImports = ImageImports { queued :: [PendingImage]
                                 , importing :: (ThreadId, PendingImage)
                                 }
                  deriving (Show)

instance ToJSON ImageImports where
    toJSON ImageImports {..} =
        let (_, active) = importing in Aeson.toJSON $ active : queued

instance Elm ImageImports where
    toElmDefinition _ =
        DefPrim
            $ ElmList
            $ definitionToRef
            $ toElmDefinition
            $ Proxy @PendingImage


data ImageStore = ImageStore { pending :: Maybe ImageImports
                             , imported :: [Image]
                             }
  deriving (Generic, Show)
  deriving (Elm, ToJSON) via ElmStreet ImageStore


data ProjectModel = ProjectModel
  { mTemplates ::  ImageStore
  , mSources :: ImageStore
  , mName :: T.Text
  , mCreatedAt :: Int
  , mUpdatedAt :: Int
  , mImP :: ImP.Model
  , mDirectory :: T.Text
  , mId :: Repo.ProjectId
  , mView :: View
  }
  deriving (Generic, Show)
  deriving (Elm, ToJSON) via ElmStreet Model

type Model = ProjectModel

data Ctx = Ctx { trigger :: Msg -> IO ()
               , persist :: Repo.Project -> IO ()
               }


newtype DataPath = DataPath T.Text

data ImageImportConfig = ImageImportConfig { projectName :: T.Text
                                            , ctx :: Ctx
                                            , onImported :: Image -> Msg
                                           }

initImageStore :: [Image] -> ImageStore
initImageStore images = ImageStore { pending = Nothing, imported = images }


initModel :: Repo.Project -> Model
initModel rp = ProjectModel
    { mTemplates        = (initImageStore . Repo.pTemplates) rp
    , mSources          = (initImageStore . Repo.pSources) rp
    , mName             = Repo.pName rp
    , mDirectory        = Repo.pDirectory rp
    , mCreatedAt        = Repo.pCreatedAt rp
    , mUpdatedAt        = Repo.pUpdatedAt rp
    , mImP = (ImP.initModel . Repo.pGlyphCollections) rp
    , mId               = Repo.pId rp
    , mView             = Sources
    }


mkPendingImage :: T.Text -> PendingImage
mkPendingImage filePath = PendingImage
    { piFileName = T.pack $ Path.takeFileName $ T.unpack filePath
    , piFilePath = filePath
    }


importImage :: ImageImportConfig -> PendingImage -> IO (ThreadId, PendingImage)
importImage ImageImportConfig {..} pendingImage = do
    let Ctx {..} = ctx
    dataPath <- getDataPath projectName
    tid      <-
        forkIO
            $ let io = do
                      image <- createImage dataPath (piFilePath pendingImage)
                      trigger $ onImported image
              in
                  io
                      `catchAny` \e ->
                                     putStrLn
                                         ("Caught error while importing image: "
                                         ++ show e
                                         )
    return (tid, pendingImage)



deleteImages :: ImageStore -> [Image] -> IO ImageStore
deleteImages imageStore toDelete = do
    newImported <- filterM
        (\importedImage -> if importedImage `elem` toDelete
            then do
                Image.delete importedImage
                return False
            else return True
        )
        (imported imageStore)

    return $ imageStore { imported = newImported }

cancelImports
    :: ImageImportConfig
    -> [PendingImage]
    -> Maybe ImageImports
    -> IO (Maybe ImageImports)
cancelImports _            _        Nothing        = return Nothing
cancelImports importConfig toCancel (Just imports) = foldM updateImports
                                                           (Just imports)
                                                           toCancel
  where

    oldQueued = queued imports
    newQueued = List.filter (`notElem` toCancel) oldQueued

    updateImports
        :: Maybe ImageImports -> PendingImage -> IO (Maybe ImageImports)
    updateImports Nothing _ = return Nothing
    updateImports (Just imports') imageToCancel =
        let (tid, activeImport) = importing imports'
        in  case (newQueued, activeImport == imageToCancel) of
                ([], True) -> do
                    killThread tid
                    return Nothing


                (pendingImage : pendingImages, True) -> do
                    killThread tid
                    newImporting <- importImage importConfig pendingImage
                    return $ Just $ ImageImports { queued    = pendingImages
                                                 , importing = newImporting
                                                 }
                (_, _) -> return $ Just $ imports' { queued = newQueued }



modelToRepoProject :: Model -> Repo.Project
modelToRepoProject model = Repo.Project
    { Repo.pId               = mId model
    , Repo.pCreatedAt        = mCreatedAt model
    , Repo.pUpdatedAt        = mUpdatedAt model
    , Repo.pDirectory        = mDirectory model
    , Repo.pName             = mName model
    , Repo.pTemplates        = imported . mTemplates $ model
    , Repo.pSources          = imported . mSources $ model
    , Repo.pGlyphCollections = ImP.tmCollections . mImP $ model
    }



queueImageImports
    :: [T.Text] -> ImageImportConfig -> ImageStore -> IO ImageStore

queueImageImports [] _ imageStore = return imageStore
queueImageImports (filePath : filePaths) importConfig imageStore =
    case pending imageStore of
        Just oldPending -> return $ imageStore
            { pending = Just $ oldPending
                            { queued = queued oldPending ++ List.map
                                           mkPendingImage
                                           (filePath : filePaths)
                            }
            }
        Nothing -> do
            importing <- importImage importConfig $ mkPendingImage filePath
            return $ imageStore
                { pending = Just $ ImageImports
                                { queued    = List.map mkPendingImage filePaths
                                , importing = importing
                                }
                }

handleImageImported :: Image -> ImageImportConfig -> ImageStore -> IO ImageStore
handleImageImported image importConfig imageStore =
    let newImported   = imported imageStore ++ [image]
        newImageStore = imageStore { imported = newImported }
    in  case pending imageStore of
            Nothing         -> return newImageStore
            Just oldPending -> case queued oldPending of
                [] -> return $ newImageStore { pending = Nothing }
                (pendingImage : pendingImages) -> do
                    importing <- importImage importConfig pendingImage
                    return $ newImageStore
                        { pending = Just $ ImageImports { queued = pendingImages
                                                        , importing = importing
                                                        }
                        }

update :: Ctx -> Msg -> Model -> IO Model
update ctx@Ctx {..} msg model = do
    newModel <- applyUpdate
    persist $ modelToRepoProject newModel
    return newModel
  where
    mkImageImportConfig :: (Image -> Msg) -> ImageImportConfig
    mkImageImportConfig  = ImageImportConfig (mName model) ctx

    templateImportConfig = mkImageImportConfig TemplateImported
    sourceImportConfig   = mkImageImportConfig SourceImported

    tmCtx = ImP.Ctx { ImP.trigger       = trigger . ImPMsg
                   , projectDirectory = mDirectory model
                   , onMatchCompleted = trigger . SetView . Collections
                   }

    applyUpdate = case msg of
        HandleReq (ImportTemplates []       ) -> return model
        HandleReq (ImportTemplates filePaths) -> do
            newTemplates <- queueImageImports filePaths
                                              templateImportConfig
                                              (mTemplates model)
            return $ model { mTemplates = newTemplates }

        TemplateImported image -> do
            newTemplates <- handleImageImported image
                                                templateImportConfig
                                                (mTemplates model)
            return $ model { mTemplates = newTemplates }

        HandleReq (ImportSources []       ) -> return model
        HandleReq (ImportSources filePaths) -> do
            newSources <- queueImageImports filePaths
                                            sourceImportConfig
                                            (mSources model)
            return $ model { mSources = newSources }

        SourceImported image -> do
            newSources <- handleImageImported image
                                              sourceImportConfig
                                              (mSources model)
            return $ model { mSources = newSources }


        HandleReq (CancelImportTemplates images) ->
            let oldTemplates = mTemplates model
            in  do
                    newTemplateImports <- cancelImports
                        templateImportConfig
                        images
                        (pending oldTemplates)
                    return $ model
                        { mTemplates = oldTemplates
                                           { pending = newTemplateImports
                                           }
                        }

        HandleReq (CancelImportSources images) ->
            let oldSources = mSources model
            in
                do
                    newSourceImports <- cancelImports sourceImportConfig
                                                      images
                                                      (pending oldSources)
                    return $ model
                        { mSources = oldSources { pending = newSourceImports }
                        }

        HandleReq (DeleteTemplates images) -> do
            newTemplates <- deleteImages (mTemplates model) images
            return $ model { mTemplates = newTemplates }

        HandleReq (DeleteSources images) -> do
            newSources <- deleteImages (mSources model) images
            return $ model { mSources = newSources }

        HandleReq (ImPReq subReq) ->
            update ctx (ImPMsg $ ImP.HandleReq subReq) model

        ImPMsg subMsg -> do
            newImP <- ImP.update tmCtx subMsg (mImP model)
            return $ model { mImP = newImP }

        HandleReq (ViewReq view) -> update ctx (SetView view) model

        SetView   view           -> return $ model { mView = view }

        HandleReq (OpenCollectionsDirectory glyphName) -> do
            Utils.openFile $ ImP.collectionsDirectory (mDirectory model) glyphName
            return model

        HandleReq (OpenAvgsDirectory glyphName) -> do
            Utils.openFile $ ImP.avgsDirectory (mDirectory model) glyphName
            return model

        ShutDown ->
            (cancelPendingImports >=> update
                    ctx
                    (ImPMsg $ ImP.HandleReq ImP.CancelTemplateMatching)
                )
                model


cancelPendingImport :: ImageStore -> IO ImageStore
cancelPendingImport imageStore = case importing <$> pending imageStore of
    Just (pid, _) -> do
        killThread pid
        return $ imageStore { pending = Nothing }
    Nothing -> return imageStore

cancelPendingImports :: Model -> IO Model
cancelPendingImports model = do
    newTemplates <- cancelPendingImport (mTemplates model)
    newSources   <- cancelPendingImport (mSources model)
    return $ model { mTemplates = newTemplates, mSources = newSources }

getDataPath :: T.Text -> IO DataPath
getDataPath projectName = do
    p <- AppData.path [projectName]
    Directory.createDirectoryIfMissing True (T.unpack p)
    return (DataPath p)


createImage :: DataPath -> T.Text -> IO Image
createImage (DataPath projectDataPath) filePath = do
    imageId <- UUID.toString <$> UUID.nextRandom
    let extension = Path.takeExtension $ T.unpack filePath
        thumbnailPath =
            (T.unpack projectDataPath) </> imageId <.> extension

    _       <-
        Image.read
        >=> (pure . Image.scaleToWidth 500)
        >=> (Image.write $ T.pack thumbnailPath)
        $   filePath

    return $ Image { iThumbnail = T.pack thumbnailPath
                   , iOriginal  = filePath
                   , iName      = T.pack $ Path.takeFileName $ T.unpack filePath
                   , iId        =  T.pack imageId
                   }
