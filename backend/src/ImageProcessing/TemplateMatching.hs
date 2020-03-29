module ImageProcessing.TemplateMatching where

import Data.Function ((&))
import qualified Data.List as List
import Data.Massiv.Array as Massiv
import qualified Data.Set as Set
import qualified ImageProcessing as Image

matchMatrices ::
  (Storable e, Default e, Floating e, Num e) =>
  Image.Matrix e ->
  Image.Matrix e ->
  Image.Matrix e
matchMatrices haystack needle =
  -- calculate the normalized cross correlation of two matrices
  -- https://www.youtube.com/watch?v=ngEC3sXeUb4
  -- https://stackoverflow.com/questions/53436231/normalized-cross-correlation-in-python
  let stencilSize = (size needle)
      stencilCenter = (0 :. 0)
      templateIndices = rangeSize Seq (0 :. 0) stencilSize
      stencil = makeStencil stencilSize stencilCenter $ \get ->
        let -- multiply matrix a with matrix b and sum the results
            ab =
              Massiv.sum $
                Massiv.map
                  ( \ix ->
                      let s = get ix
                          t = needle ! ix
                       in (* t) <$> s
                  )
                  templateIndices
            -- square matrix a and sum the result
            as = Massiv.sum $ Massiv.map (\ix -> (^ 2) <$> get ix) templateIndices
            -- square matrix b and sum the result
            bs = Massiv.sum $ Massiv.map (\ix -> (needle ! ix) ^ 2) templateIndices
            -- denominator of the final equation
            denom = (\a b -> sqrt (a * b)) <$> as <*> pure bs
         in (/) <$> ab <*> denom
      correlations = computeAs S (mapStencil (Fill 0) stencil haystack)
   in correlations

findInSet :: (a -> Bool) -> Set.Set a -> Maybe a
findInSet f =
  let foldFn result a = case result of
        Just r -> Just r
        Nothing -> if f a then Just a else Nothing
   in Set.foldl foldFn Nothing

filterMatches :: Sz Ix2 -> [(Double, Ix2)] -> [(Double, Ix2)]
filterMatches bounds (match : matches) =
  let boundsIx = unSz bounds
      initialAcc = Set.fromList [match]
      areNeighbors (_, ix) (_, ix') = abs (ix' - ix) < boundsIx
      resultSet =
        List.foldl
          ( \acc (score, ix) ->
              let neighbor = findInSet (areNeighbors (score, ix)) acc
               in case neighbor of
                    Nothing -> Set.insert (score, ix) acc
                    Just (score', ix') ->
                      if score > score'
                        then Set.delete (score', ix') acc & Set.insert (score, ix)
                        else acc
          )
          initialAcc
          matches
   in Set.toList resultSet
filterMatches _ [] = []

type Result = (Double, Image.Image)

matchImages ::
  Image.Image -> Image.Image -> [(Double, Image.Image)]
matchImages sourceImage templateImage =
  let -- the template size of 15 was determined experimentally
      -- seemed to work well with the image material I used for testing, but might not be optimal for other material
      -- might need to give users the option to customize this
      getScaleFactor (w, _) = 15 / (fromIntegral w)
      scaleFactor = getScaleFactor (Image.size templateImage)
      scaledTemplate = Image.scale scaleFactor templateImage
      scaledSource = Image.scale scaleFactor sourceImage
      scaleIx :: RealFrac n => n -> Ix2 -> Ix2
      scaleIx n (Ix2 i j) =
        round (fromIntegral i * n) :. round (fromIntegral j * n)
      -- not defining this will result in non-exhausting pattern-match error, why?
      scaleIx _ _ = error "Something went wrong while scaleIx"
      -- undo scaling of ix on match results,
      -- so that the ix can be used to extract image from on the original source image
      unscaleIx = (scaleIx (1 / scaleFactor))
      -- extract result from sourceImage
      extractResult ix = computeAs S $ extract' ix (size templateImage) sourceImage
      go :: Image.Image -> Image.Image -> [(Double, Ix2)]
      go s t =
        let tMatrix = (Image.toMatrix . Image.toDouble) t
            sMatrix = (Image.toMatrix . Image.toDouble) s
            result = matchMatrices sMatrix tMatrix
            -- the score threshold of 0.95 was determined experimentally
            -- seemed to work well with the image material I used for testing, but might not be optimal for other material
            -- might need to give users the option to customize this
            ixs = ifoldMono (\ix e -> [(e, ix) | e > 0.95]) result
         in ixs
   in go scaledSource scaledTemplate
        & (fmap . fmap) unscaleIx
        & filterMatches (size templateImage)
        & (fmap . fmap) extractResult
