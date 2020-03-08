module AppData
    ( directory
    , path
    )
where

import           System.Directory              as Directory
import           System.FilePath.Posix         as Path
import qualified Data.Text as T

directory :: IO FilePath
directory = do
    dir <- Directory.getXdgDirectory Directory.XdgData "glyphcollector"
    Directory.createDirectoryIfMissing True dir
    return dir

path :: [T.Text] -> IO T.Text
path paths = do
    root <- directory
    return $ T.pack $ Path.joinPath (root : (T.unpack <$> paths))
