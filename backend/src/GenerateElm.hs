{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Elm
import qualified Project
import qualified Project.GlyphCollection as Project
import qualified Project.ImageProcessing
import qualified App
import qualified Repo

settings m =
  Elm.defaultSettings "../frontend/src/elm" (["IPC"] ++ m)


type Types  =
  '[ App.Model
   , App.Req
   , App.Res
   , Project.Model
   , Project.Req
   , Project.Image
   , Project.PendingImage
   , Project.ImageStore
   , Project.GlyphCollection
   , Project.MatchedGlyph
   , Project.Avg
   , Project.View
   , Project.ImageProcessing.TMInput
   , Project.ImageProcessing.TMStatus
   , Project.ImageProcessing.TMProcess
   , Project.ImageProcessing.Model
   , Project.ImageProcessing.TMReq
   , Repo.Model
   , Repo.Req
   , Repo.ProjectId
   , Repo.CreateProjectInput
   , Repo.Project
   ]


main :: IO ()
main = Elm.generateElm @Types $ settings []
