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

-- | Read a binary image to the path given.
writeImageBinary :: FilePath -> Image Binary -> IO ()
writeImageBinary fp = writeImageGrey fp . fmap toGrey
    where
        toGrey (Pixel1 False) = 0
        toGrey (Pixel1 True) = 255
        {-# INLINE toGrey #-}

-- | Read a greyscale image to the path given.
writeImageGrey :: FilePath -> Image Grey -> IO ()
writeImageGrey fp = MIO.writeImageAuto fp
                  . toArray
                  . fmap toGrey
    where
        toGrey :: Grey -> MIO.Pixel (MIO.Y' MIO.SRGB) Word8
        toGrey (Pixel1 y) = MIO.PixelY' y
        {-# INLINE toGrey #-}

-- | Read an RGB image to the path given.
writeImageRGB :: FilePath -> Image RGB -> IO ()
writeImageRGB fp = MIO.writeImageAuto fp
                 . toArray
                 . fmap toSRGB
    where
        toSRGB :: RGB -> MIO.Pixel (MIO.SRGB MIO.NonLinear) Word8
        toSRGB (Pixel3 r g b) = MIO.PixelRGB r g b
        {-# INLINE toSRGB #-}

-- | Read an RGBA image to the path given.
writeImageRGBA :: FilePath -> Image RGBA -> IO ()
writeImageRGBA fp = MIO.writeImageAuto fp
                  . toArray
                  . fmap toSRGBA
    where
        toSRGBA :: RGBA -> MIO.Pixel (MIO.Alpha (MIO.SRGB MIO.NonLinear)) Word8
        toSRGBA (Pixel4 r g b a) = MIO.PixelSRGBA r g b a
        {-# INLINE toSRGBA #-}

-- | Read an HSV image to the path given.
writeImageHSV :: FilePath -> Image HSV -> IO ()
writeImageHSV fp = writeImageRGB fp . fmap hsvToRGB

-- | Read an HSL image to the path given.
writeImageHSL :: FilePath -> Image HSL -> IO ()
writeImageHSL fp = writeImageRGB fp . fmap hslToRGB
