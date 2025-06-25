{-# LANGUAGE DataKinds #-}
-- | Functions for writing images to files.
module Graphics.Phoskell.IO.Output (
    writeImageBinary,
    writeImageGrey,
    writeImageRGB,
    writeImageRGBA,
    writeImageHSV,
    writeImageHSL,
) where

import qualified Codec.Picture as JP

import Graphics.Phoskell.Core
import Data.Char (toLower)
import GHC.Base (failIO)
import Data.Massiv.Array (toUnboxedVector, toStorableVector)
import Graphics.Phoskell.Analysis
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

writeDynamicImageAuto :: FilePath -> JP.DynamicImage -> IO ()
writeDynamicImageAuto fp = case extension of
        ".bmp" -> JP.saveBmpImage fp
        ".jpg" -> JP.saveJpgImage 100 fp
        ".jpeg" -> JP.saveJpgImage 100 fp
        ".gif" -> either fail id . JP.saveGifImage fp
        ".png" -> JP.savePngImage fp
        ".tiff" -> JP.saveTiffImage fp
        ".hdr" -> JP.saveRadianceImage fp
        _ -> const (failIO "Invalid image format")
    where
        extension = '.' : (fmap toLower . reverse . takeWhile (/= '.') . reverse) fp

greyToDynamicImage :: Image Grey -> JP.DynamicImage
greyToDynamicImage img =
        let (w,h) = imageSize img
            imgData = toStorableVector $ toArrayStorable (img :> unwrap)
        in JP.ImageY8 $ JP.Image {
                JP.imageWidth = w,
                JP.imageHeight = h,
                JP.imageData = imgData
            }
    where
        unwrap :: Grey -> Word8
        unwrap (Pixel1 y) = y
        {-# INLINE unwrap #-}

rgbToDynamicImage :: Image RGB -> JP.DynamicImage
rgbToDynamicImage img =
        let (w,h) = imageSize img
            imgVec = toUnboxedVector $ toArrayUnboxed (img :> splitPixel)
            channels = tuple3ToList $ VU.unzip3 imgVec
            imgData = VS.generate (w*h*3) (\i -> (channels !! (i `mod` 3)) VU.! (i `div` 3))
        in JP.ImageRGB8 $ JP.Image {
                JP.imageWidth = w,
                JP.imageHeight = h,
                JP.imageData = imgData
            }
    where
        splitPixel :: RGB -> (Word8, Word8, Word8)
        splitPixel = VU.toURepr
        {-# INLINE splitPixel #-}
        tuple3ToList :: (a,a,a) -> [a]
        tuple3ToList (r,g,b) = [r,g,b]
        {-# INLINE tuple3ToList #-}

rgbaToDynamicImage :: Image RGBA -> JP.DynamicImage
rgbaToDynamicImage img =
        let (w,h) = imageSize img
            imgVec = toUnboxedVector $ toArrayUnboxed (img :> splitPixel)
            channels = tuple4ToList $ VU.unzip4 imgVec
            imgData = VS.generate (w*h*4) (\i -> (channels !! (i `mod` 4)) VU.! (i `div` 4))
        in JP.ImageRGBA8 $ JP.Image {
                JP.imageWidth = w,
                JP.imageHeight = h,
                JP.imageData = imgData
            }
    where
        splitPixel :: RGBA -> (Word8, Word8, Word8, Word8)
        splitPixel = VU.toURepr
        {-# INLINE splitPixel #-}
        tuple4ToList :: (a,a,a,a) -> [a]
        tuple4ToList (r,g,b,a) = [r,g,b,a]
        {-# INLINE tuple4ToList #-}

-- | Read a binary image to the path given.
writeImageBinary :: FilePath -> Image Binary -> IO ()
writeImageBinary fp = writeImageGrey fp . fmap toGrey
    where
        toGrey (Pixel1 False) = 0
        toGrey (Pixel1 True) = 255
        {-# INLINE toGrey #-}

-- | Read a greyscale image to the path given.
writeImageGrey :: FilePath -> Image Grey -> IO ()
writeImageGrey fp = writeDynamicImageAuto fp . greyToDynamicImage

-- | Read an RGB image to the path given.
writeImageRGB :: FilePath -> Image RGB -> IO ()
writeImageRGB fp = writeDynamicImageAuto fp . rgbToDynamicImage

-- | Read an RGBA image to the path given.
writeImageRGBA :: FilePath -> Image RGBA -> IO ()
writeImageRGBA fp = writeDynamicImageAuto fp . rgbaToDynamicImage

-- | Read an HSV image to the path given.
writeImageHSV :: FilePath -> Image HSV -> IO ()
writeImageHSV fp = writeImageRGB fp . fmap hsvToRGB

-- | Read an HSL image to the path given.
writeImageHSL :: FilePath -> Image HSL -> IO ()
writeImageHSL fp = writeImageRGB fp . fmap hslToRGB
