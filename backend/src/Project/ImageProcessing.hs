{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Project.ImageProcessing where

import           Elm
import qualified Project.GlyphCollection       as GlyphCollection
import           Project.GlyphCollection        ( GlyphCollection(..)
                                                , MatchedGlyph(..)
                                                , Avg(..)
                                                )
import           GHC.Generics
import           Control.Monad                  ( foldM
                                                , forM_
                                                , forM
                                                )
import           Control.Exception
import           Control.Concurrent             ( ThreadId
                                                , forkIO
                                                , killThread
                                                )
import           Exception
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import qualified Data.List                     as List
import qualified Data.Char                     as Char
import qualified Data.Map                      as Map
import           Data.Map                       ( Map
                                                , (!?)
                                                , (!)
                                                )

import qualified Data.Maybe                    as Maybe

import           Data.Function                  ( (&) )
import qualified Data.Text                     as T
import qualified Text.Read                     as T
import           Text.Printf                    ( printf )
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID
import qualified Utils
import qualified System.FilePath               as Path
import           System.FilePath                ( (</>)
                                                , (<.>)
                                                )
import qualified System.Directory              as Directory
import qualified Project.Image                 as Image
import           Project.Image                  ( Image(..) )
import qualified ImageProcessing               as Image
import qualified ImageProcessing.TemplateMatching
                                               as IPT

import qualified Debug


data TMInput = TMInput
  { tmiTemplates :: [Image]
  , tmiSources :: [Image]
  }
  deriving (Generic, Show)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet TMInput

data TMStatus = TMStatus { tmpSource :: Image
                             , tmpTemplate :: Image
                             , tmpPct :: Float
                             }
  deriving (Generic, Show)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet TMStatus


instance Elm ThreadId where
    toElmDefinition _ = DefPrim ElmString

instance ToJSON ThreadId where
    toJSON tid = Aeson.String $ T.pack $ show tid

data TMProcess = TMProcess { tmpThreadId :: ThreadId
                             , tmpStatus :: Maybe TMStatus
                           }
  deriving (Generic, Show)
  deriving (Elm, ToJSON) via ElmStreet TMProcess

data ImPModel = ImPModel { tmCollections :: [GlyphCollection]
                          , tmProcess ::  Maybe TMProcess
                          , tmGenAvgProcess :: Maybe (ThreadId, T.Text)
                         }
  deriving (Generic, Show)
  deriving (Elm, ToJSON) via ElmStreet ImPModel

type Model = ImPModel

data TMReq =
  RunTemplateMatching TMInput
  | CancelTemplateMatching
  | DeleteMatchedGlyph MatchedGlyph
  | GenAvg [MatchedGlyph]
  | DeleteAvg Avg
  | CancelGenAvg
  deriving (Generic, Show)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet TMReq

type Req = TMReq


data Msg =
  HandleReq TMReq
  | UpdateStatus TMStatus
  | StoreMatchedGlyphs [MatchedGlyph]
  | AvgDone Avg
  | TMProcessFinished [T.Text]
  deriving (Show)


data Ctx = Ctx { trigger :: Msg -> IO ()
               , projectDirectory :: T.Text
               , onMatchCompleted :: T.Text -> IO ()
               }

initModel :: [GlyphCollection] -> ImPModel
initModel glyphCollections = ImPModel { tmCollections   = glyphCollections
                                      , tmProcess       = Nothing
                                      , tmGenAvgProcess = Nothing
                                      }


collectionsDirectory projectDirectory glyphName = T.pack $
    (T.unpack projectDirectory) </> "results" </> "glyphs" </> (T.unpack glyphName)

avgsDirectory projectDirectory glyphName = T.pack $
    (T.unpack projectDirectory) </> "results" </> "averages" </> (T.unpack glyphName)



update :: Ctx -> Msg -> Model -> IO Model
update ctx@Ctx {..} msg model = case msg of
    HandleReq (RunTemplateMatching input) -> case tmProcess model of
        Nothing -> do
            tid <- forkIO $ do
                runTemplateMatching ctx input (tmCollections model)

            return $ model
                { tmProcess = Just $ TMProcess { tmpThreadId = tid
                                               , tmpStatus   = Nothing
                                               }
                }
        Just _ -> return model


    HandleReq CancelTemplateMatching -> case tmProcess model of
        Nothing      -> return model
        Just process -> do
            _ <- killThread (tmpThreadId process)
            return $ model { tmProcess = Nothing }

    HandleReq (DeleteMatchedGlyph match) -> do
        let newCollections =
                GlyphCollection.deleteMatchedGlyph match (tmCollections model)
        Image.delete (mgImage match)
        return $ model { tmCollections = newCollections }

    HandleReq (GenAvg []        ) -> return model
    HandleReq (GenAvg (mg : mgs)) -> case tmGenAvgProcess model of
        Just _  -> return model
        Nothing -> do
            tid <- forkIO $ genAvg ctx (mg : mgs)
            return $ model { tmGenAvgProcess = Just (tid, (mgGlyphName mg)) }

    HandleReq (DeleteAvg avg) -> do
        let newCollections =
                GlyphCollection.deleteAvg avg (tmCollections model)
        Image.delete (avgImage avg)
        return $ model { tmCollections = newCollections }


    HandleReq CancelGenAvg -> case tmGenAvgProcess model of
        Just (tid, _) -> do
            killThread tid
            return $ model { tmGenAvgProcess = Nothing }
        Nothing -> return model


    UpdateStatus newStatus ->
        tmProcess model
            & fmap
                  (\process -> model
                      { tmProcess = Just
                                        $ process { tmpStatus = Just newStatus }
                      }
                  )
            & Maybe.fromMaybe model
            & return

    StoreMatchedGlyphs matchedGlyphs ->
        let newCollections = GlyphCollection.appendMatchedGlyphs
                matchedGlyphs
                (tmCollections model)
        in  return $ model { tmCollections = newCollections }

    AvgDone avg ->
        let newCollections =
                    GlyphCollection.appendAvgs [avg] (tmCollections model)
        in  return $ model { tmCollections   = newCollections
                           , tmGenAvgProcess = Nothing
                           }

    TMProcessFinished matchedGlyphNames -> do
        case matchedGlyphNames of
            (g : _) -> onMatchCompleted g
            []      -> return ()
        return $ model { tmProcess = Nothing }


genAvg :: Ctx -> [MatchedGlyph] -> IO ()
genAvg _        []         = return ()
genAvg Ctx {..} (mg : mgs) = do
    let glyphName = mgGlyphName mg
    imgs       <- (mg : mgs) & fmap (iThumbnail . mgImage) & mapM Image.read
    outDir     <- Utils.mkdirp $ avgsDirectory projectDirectory glyphName
    lastSeqNum <- inferLastSeqNum outDir
    imgId      <- UUID.nextRandom
    let avg         = Image.mkAverage imgs
        outFileName = formatFileName (T.unpack glyphName) "a" (lastSeqNum + 1) <.> "jpg"
        outFilePath = (T.unpack outDir) </> outFileName
    Image.write (T.pack outFilePath) avg
    trigger $ AvgDone $ Avg
        { avgImage     = Image { iThumbnail = T.pack outFilePath
                               , iOriginal  = ""
                               , iName      = T.pack outFileName
                               , iId        = T.pack $ UUID.toString imgId
                               }
        , avgGlyphName = glyphName
        }
    return ()


runTemplateMatching :: Ctx -> TMInput -> [GlyphCollection] -> IO ()
runTemplateMatching Ctx {..} input prevCollections = do
    let combinations =
            [ (source, template)
                | source   <- tmiSources input
                , template <- tmiTemplates input
                ]
                & List.filter (not . hasMatchedBefore prevCollections)
                & Prelude.zip ([0 ..] :: [Integer])

    _ <- forM_ combinations $ \(ix, (source, template)) -> do
        trigger $ UpdateStatus
            (TMStatus
                { tmpSource = source
                , tmpTemplate = template
                , tmpPct = fromIntegral ix / fromIntegral (length combinations)
                }
            )
        matchedGlyphs <- matchImages
            source
            template
            projectDirectory
            (T.pack $ Path.takeBaseName $ T.unpack $ iOriginal template)
        trigger $ StoreMatchedGlyphs matchedGlyphs
        return matchedGlyphs

    let matchedGlyphNames =
            (tmiTemplates input)
            & List.map (Path.takeBaseName . T.unpack . iOriginal)
            & List.map T.pack


    trigger $ Debug.log "Finished" $ TMProcessFinished matchedGlyphNames
  where
    hasMatchedBefore collection (source, template) =
        let prevMatched = collection & List.concatMap gcMatches & List.map
                (\g -> (mgSourceImage g, mgTemplateImage g))
        in  (source, template) `elem` prevMatched


formatFileName :: String -> String -> Int -> String
formatFileName = printf "%s-%s-%d"

matchImages :: Image -> Image -> T.Text -> T.Text -> IO [MatchedGlyph]
matchImages source template projectDir glyphName = do
    sourceImage   <- Image.read (iOriginal source)
    templateImage <- Image.read (iOriginal template)
    outDir        <- Utils.mkdirp $ collectionsDirectory projectDir glyphName
    lastSeqNum    <- inferLastSeqNum outDir

    let matches = IPT.matchImages sourceImage templateImage
        foldMatch :: [MatchedGlyph] -> (Int, IPT.Result) -> IO [MatchedGlyph]
        foldMatch acc (i, (score, image)) = do
            imgId <- UUID.nextRandom
            let outName = formatFileName (T.unpack glyphName) "m" (i + lastSeqNum + 1) <.> "jpg"
                saveTo = (T.unpack outDir) </> outName
                matchedGlyph = MatchedGlyph
                    { mgGlyphName     = glyphName
                    , mgScore         = score
                    , mgSourceImage   = source
                    , mgTemplateImage = template
                    , mgImage         = Image { iThumbnail = T.pack saveTo
                                              , iOriginal  = "" -- TODO fix this
                                              , iName      = T.pack outName
                                              , iId        = T.pack $ UUID.toString imgId
                                              }
                    }
            Image.write (T.pack saveTo) image
            return $ acc ++ [matchedGlyph]

    foldM foldMatch [] $ Utils.indexed matches


inferLastSeqNum :: T.Text -> IO Int
inferLastSeqNum dir =
    let digits = List.reverse
            . List.takeWhile (Char.isDigit)
            . List.dropWhile (not . Char.isDigit)
            . List.reverse
    in  do
            files <- Directory.listDirectory (T.unpack dir)
            let nums :: [Int] =
                    Maybe.catMaybes $ T.readMaybe . digits <$> files
            case nums of
                (n : ns) -> return $ List.maximum (n : ns)
                []       -> return 0
