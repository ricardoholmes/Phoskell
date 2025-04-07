{-# LANGUAGE DataKinds #-}
module Graphics.Phoskell.IO.Output (
    writeImageBinary,
    writeImageGrey,
    writeImageRGB,
    writeImageRGBA,
    writeImageHSV,
    writeImageHSL,
) where

import qualified Data.Massiv.Array.IO as MIO

import Graphics.Phoskell.Core

writeImageBinary :: FilePath -> Image Binary -> IO ()
writeImageBinary fp = writeImageGrey fp . fmap toGrey
    where
        toGrey (Pixel1 False) = 0
        toGrey (Pixel1 True) = 255
        {-# INLINE toGrey #-}

writeImageGrey :: FilePath -> Image Grey -> IO ()
writeImageGrey fp = MIO.writeImageAuto fp
                  . toArray
                  . fmap toGrey
    where
        toGrey :: Grey -> MIO.Pixel (MIO.Y' MIO.SRGB) Word8
        toGrey (Pixel1 y) = MIO.PixelY' y
        {-# INLINE toGrey #-}

writeImageRGB :: FilePath -> Image RGB -> IO ()
writeImageRGB fp = MIO.writeImageAuto fp
                 . toArray
                 . fmap toSRGB
    where
        toSRGB :: RGB -> MIO.Pixel (MIO.SRGB MIO.NonLinear) Word8
        toSRGB (Pixel3 r g b) = MIO.PixelRGB r g b
        {-# INLINE toSRGB #-}

writeImageRGBA :: FilePath -> Image RGBA -> IO ()
writeImageRGBA fp = MIO.writeImageAuto fp
                  . toArray
                  . fmap toSRGBA
    where
        toSRGBA :: RGBA -> MIO.Pixel (MIO.Alpha (MIO.SRGB MIO.NonLinear)) Word8
        toSRGBA (Pixel4 r g b a) = MIO.PixelSRGBA r g b a
        {-# INLINE toSRGBA #-}

writeImageHSV :: FilePath -> Image HSV -> IO ()
writeImageHSV fp = writeImageRGB fp . fmap hsvToRGB

writeImageHSL :: FilePath -> Image HSL -> IO ()
writeImageHSL fp = writeImageRGB fp . fmap hslToRGB
