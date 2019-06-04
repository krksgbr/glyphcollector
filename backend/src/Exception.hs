module Exception where

import           Control.Exception

data Failure =
  Failure String
  deriving (Show)

instance Exception Failure

-- from https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell
isSyncException :: Exception e => e -> Bool
isSyncException e = case fromException (toException e) of
    Just (SomeAsyncException _) -> False
    Nothing                     -> True


catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch
