module Graphics.IO.JuicyPixels (
    readImage
) where

import qualified Codec.Picture as JP
import Graphics.Image
import Data.Massiv.Array
import Data.Massiv.Array.Manifest.Vector
import Data.Word (Word8)
import qualified Data.Vector.Storable as V

readImage :: FilePath -> IO (Either String (Image Word8))
readImage path = do eitherImg <- JP.readImage path
                    return $ case eitherImg of
                        Left s -> Left s
                        Right dynImg -> case dynamicImageToImage dynImg of
                            Just img -> Right img
                            Nothing -> let JP.Image{
                                                JP.imageWidth = w,
                                                JP.imageHeight = h,
                                                JP.imageData = imgData
                                        } = JP.convertRGBA8 dynImg
                                        in Left ("Error reading image: "
                                                ++ show w ++ "x" ++ show h
                                                ++ " - "
                                                ++ show (V.length imgData))

dynamicImageToImage :: JP.DynamicImage -> Maybe (Image Word8)
dynamicImageToImage dynImg = do img <- castFromVector Par (Sz2 h (w*4)) imgData
                                return $ BaseImage (delay img)
    where
        JP.Image{
            JP.imageWidth = w,
            JP.imageHeight = h,
            JP.imageData = imgData
        } = JP.convertRGBA8 dynImg
