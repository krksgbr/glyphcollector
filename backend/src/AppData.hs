module AppData
    ( directory
    , path
    )
where

import           System.Directory              as Directory
import           System.FilePath.Posix         as Path

directory :: IO FilePath
directory = do
    dir <- Directory.getXdgDirectory Directory.XdgData "glyphcollector"
    Directory.createDirectoryIfMissing True dir
    return dir

path :: [FilePath] -> IO FilePath
path paths = do
    root <- directory
    return $ Path.joinPath (root : paths)
