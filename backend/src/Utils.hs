{-# LANGUAGE BangPatterns #-}

module Utils where

import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import qualified System.Directory              as Directory
import           System.IO.Error                ( isDoesNotExistError )
import qualified System.Info                   as System
import qualified System.Process                as Process
import qualified Control.Exception             as Exception
import           Control.Exception              ( catch )
import           Debug
import           Exception

getTimestamp :: IO Int
getTimestamp = (round . (* 1000)) <$> getPOSIXTime


removeFile :: FilePath -> IO ()
removeFile fileName = Directory.removeFile fileName `catch` handleExists
  where
    handleExists e | isDoesNotExistError e = return ()
                   | otherwise             = Exception.throwIO e


mkdirp :: FilePath -> IO FilePath
mkdirp dirPath = do
    Directory.createDirectoryIfMissing True dirPath
    return dirPath


time :: IO a -> IO a
time io = do
    start  <- getTimestamp
    result <- io
    end    <- getTimestamp
    putStrLn $ "done in: " ++ (show (end - start)) ++ "ms"
    return result



indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]



openFile :: FilePath -> IO ()
openFile dirpath =
    let maybeCmd = case System.os of
            "mingw32" -> Just ("cmd", ["/c start " ++ dirpath])
            "darwin"  -> Just ("open", [dirpath])
            "linux"   -> Just ("xdg-open", [dirpath])

            _         -> Nothing
    in  case (maybeCmd) of
            Just (cmd, args) -> do
                _ <- Process.readProcess cmd args "" `catchAny` \_ -> return ""
                return ()


                -- Process.withCreateProcess (Process.proc cmd args)
                --     $ \_ _ _ _ -> return ()
            Nothing -> return ()
