{-# LANGUAGE BangPatterns #-}

module ImageProcessing where


import qualified Data.Massiv.Array             as Massiv
import           Data.Massiv.Array              ( (!) )
import qualified Data.Massiv.Array.IO          as MassivIO
import           Data.Function                  ( (&) )
import qualified Graphics.ColorSpace           as ColorSpace
import qualified Data.List                     as List
import qualified Debug
import qualified Codec.Picture.Extra           as Juicy
import qualified Codec.Picture                 as Juicy

type Image_ a = MassivIO.Image Massiv.S ColorSpace.Y a
type Image = Image_ ColorSpace.Word8
type ImageD = Image_ Double


type Matrix e = Massiv.Array Massiv.S Massiv.Ix2 e




mkAverage :: [Image] -> Image
mkAverage imgsWord8 =
    List.foldl addImages img imgs
        & Massiv.map (fmap divPx)
        & Massiv.computeAs Massiv.S
        & toWord8
  where
    (img : imgs) = List.map toDouble imgsWord8 -- TODO unsafe pattern match
    addImages :: ImageD -> ImageD -> ImageD
    addImages a b =
        Massiv.computeAs Massiv.S $ Massiv.imap (\ix px -> px + (b ! ix)) a
    numImages = List.length (img : imgs)
    divPx pxValue = pxValue / fromIntegral numImages

read :: FilePath -> IO Image
read = MassivIO.readImageAuto

size :: Image -> (Int, Int)
size = Massiv.fromIx2 . Massiv.unSz . Massiv.size

scale :: RealFrac f => f -> Image -> Image
scale factor image =
    let (h, w)        = size image
        newW          = fromIntegral w * factor
        newH          = fromIntegral h * factor
        jp            = MassivIO.toJPImageY8 image
        scaledJp      = Juicy.scaleBilinear (round newW) (round newH) jp
        (Just result) = MassivIO.fromDynamicImage (Juicy.ImageY8 scaledJp)
    in  result


scaleToWidth :: Int -> Image -> Image
scaleToWidth targetWidth image
    | currentWidth' <= targetWidth' = image
    | otherwise                     = scale (targetWidth' / currentWidth') image
  where
    (_, currentWidth) = size image
    currentWidth'     = fromIntegral currentWidth
    targetWidth'      = fromIntegral targetWidth


write :: FilePath -> Image -> IO ()
write = MassivIO.writeImageAuto


toDouble :: Image -> ImageD
toDouble image =
    Massiv.computeAs Massiv.S $ Massiv.map ColorSpace.toDouble image

toWord8 :: ImageD -> Image
toWord8 image = Massiv.computeAs Massiv.S $ Massiv.map ColorSpace.toWord8 image

toMatrix :: Massiv.Storable a => Image_ a -> Matrix a
toMatrix image =
    let unpackPx (ColorSpace.PixelY v) = v
    in  Massiv.computeAs Massiv.S $ Massiv.map unpackPx image

fromMatrix :: Massiv.Storable a => Matrix a -> Image_ a
fromMatrix matrix =
    Massiv.computeAs Massiv.S $ Massiv.map ColorSpace.PixelY matrix
