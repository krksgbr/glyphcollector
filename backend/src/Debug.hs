module Debug where

import           Debug.Trace                    ( trace )


log :: (Show a) => String -> a -> a
log msg thing = trace ((msg ++ ": ") ++ show thing) thing
