{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE  DeriveGeneric #-}

module Project.Image where


import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           GHC.Generics
import           Elm
import qualified Utils

data Image = Image { iThumbnail :: FilePath
                   , iOriginal :: FilePath
                   , iName :: String
                   , iId :: String
                   }
           deriving (Generic, Show, Eq, Ord)
           deriving (Elm, ToJSON, FromJSON) via ElmStreet Image



delete :: Image -> IO ()
delete image = do
    if iThumbnail image == iOriginal image
        then return ()
        else Utils.removeFile (iThumbnail image)
    -- Image.Cache.remove (iId image)
