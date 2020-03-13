module Debug where

import qualified Debug.Trace


log :: (Show a) => String -> a -> a
log msg thing = Debug.Trace.trace ((msg ++ ": ") ++ show thing) thing

trace = Debug.Trace.trace
