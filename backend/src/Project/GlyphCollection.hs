{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE  DeriveGeneric #-}
{-# LANGUAGE  LambdaCase #-}

module Project.GlyphCollection where

-- Project related types that need to be shared between various modules

import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           GHC.Generics
import           Elm
import           Data.Map
import           Project.Image                  ( Image(..) )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map
                                                , (!?)
                                                , (!)
                                                )

import           Data.Function                  ( (&) )
import qualified Data.List                     as List


data MatchedGlyph = MatchedGlyph { mgImage :: Image
                                  , mgScore :: Double
                                  , mgSourceImage :: Image
                                  , mgTemplateImage :: Image
                                  , mgGlyphName :: String
                                 }
  deriving (Generic, Show, Eq)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet MatchedGlyph

data Avg = Avg { avgImage :: Image
               , avgGlyphName :: String
               }
  deriving (Generic, Show, Eq)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet Avg


data GlyphCollection = GlyphCollection { gcGlyphName :: String
                                       , gcMatches :: [MatchedGlyph]
                                       , gcAverages :: [Avg]
                                       }

  deriving (Generic, Show)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet GlyphCollection


ensureCollectionForKey :: String -> [GlyphCollection] -> [GlyphCollection]
ensureCollectionForKey key gcs =
    gcs
        & collectionsToMap
        & Map.alter
              (\case
                  Just c  -> Just c
                  Nothing -> Just $ GlyphCollection { gcGlyphName = key
                                                    , gcMatches   = []
                                                    , gcAverages  = []
                                                    }
              )
              key
        & collectionsFromMap

appendMatchedGlyphs :: [MatchedGlyph] -> [GlyphCollection] -> [GlyphCollection]
appendMatchedGlyphs [] gcs = gcs
appendMatchedGlyphs (mg : mgs) gcs =
    let append gc = gc { gcMatches = gcMatches gc ++ (mg : mgs) }
    in  adjustCollection append
                         (mgGlyphName mg)
                         (ensureCollectionForKey (mgGlyphName mg) gcs)

deleteMatchedGlyph :: MatchedGlyph -> [GlyphCollection] -> [GlyphCollection]
deleteMatchedGlyph match =
    let removeMatch collection = collection
            { gcMatches = List.filter (/= match) (gcMatches collection)
            }
    in  adjustCollection removeMatch (mgGlyphName match)


deleteAvg :: Avg -> [GlyphCollection] -> [GlyphCollection]
deleteAvg avg =
    let removeAvg collection = collection
            { gcAverages = List.filter (/= avg) (gcAverages collection)
            }
    in  adjustCollection removeAvg (avgGlyphName avg)

appendAvgs :: [Avg] -> [GlyphCollection] -> [GlyphCollection]
appendAvgs [] collections = collections
appendAvgs (avg : avgs) gcs =
    let append gc = gc { gcAverages = gcAverages gc ++ (avg : avgs) }
    in  adjustCollection append (avgGlyphName avg) gcs



adjustCollection
    :: (GlyphCollection -> GlyphCollection)
    -> String
    -> [GlyphCollection]
    -> [GlyphCollection]
adjustCollection fn key gcs =
    gcs & collectionsToMap & Map.adjust fn key & collectionsFromMap




-- Helpers

collectionsToMap :: [GlyphCollection] -> Map String GlyphCollection
collectionsToMap collection =
    collection & List.map (\c -> (gcGlyphName c, c)) & Map.fromList

collectionsFromMap :: Map String GlyphCollection -> [GlyphCollection]
collectionsFromMap = Map.elems
