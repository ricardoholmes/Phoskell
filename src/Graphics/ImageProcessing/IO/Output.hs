{-# LANGUAGE DataKinds #-}
module Graphics.ImageProcessing.IO.Output (
    writeImageBinary,
    writeImageGrey,
    writeImageRGB,
    writeImageRGBA,
    writeImageHSV,
    writeImageHSL,
) where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.IO as MIO
import Graphics.ImageProcessing.Core

writeImageBinary :: FilePath -> Image Binary -> IO ()
writeImageBinary fp = writeImageRGBA fp . fmap binToRGBA
        where
            binToRGBA (Pixel1 False) = 0
            binToRGBA (Pixel1 True) = 255

writeImageGrey :: FilePath -> Image Grey -> IO ()
writeImageGrey fp = writeImageRGBA fp . fmap greyToRGBA

writeImageRGB :: FilePath -> Image RGB -> IO ()
writeImageRGB fp = writeImageRGBA fp . fmap rgbToRGBA

writeImageRGBA :: FilePath -> Image RGBA -> IO ()
writeImageRGBA fp = MIO.writeImageAuto fp
                  . M.map toSRGBA
                  . toArray
    where
        toSRGBA :: RGBA -> MIO.Pixel (MIO.Alpha (MIO.SRGB MIO.NonLinear)) Word8
        toSRGBA (Pixel4 r g b a) = MIO.PixelSRGBA r g b a

writeImageHSV :: FilePath -> Image HSV -> IO ()
writeImageHSV fp = writeImageRGBA fp . fmap hsvToRGBA

writeImageHSL :: FilePath -> Image HSL -> IO ()
writeImageHSL fp = writeImageRGBA fp . fmap hslToRGBA
