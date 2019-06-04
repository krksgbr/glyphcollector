module ElmGenSettings where

import qualified Elm


settings :: [String] -> Elm.Settings
settings m =
  Elm.defaultSettings "../frontend/src/elm" (["IPC"] ++ m)
