{-# LANGUAGE DataKinds #-}
module Graphics.Phoskell.IO.Input (
    readImageBinary,
    readImageGrey,
    readImageRGB,
    readImageRGBA,
    readImageHSV,
    readImageHSL,
) where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.IO as MIO

import Graphics.Phoskell.Core
import Graphics.Phoskell.Processes.Threshold (threshold)

-- | Read an image as binary, given its path.
readImageBinary :: FilePath -> IO (Image Binary)
readImageBinary fp = (:> threshold 127) <$> readImageGrey fp

-- | Read an image as greyscale, given its path.
readImageGrey :: FilePath -> IO (Image Grey)
readImageGrey fp = do img <- readImageGrey' fp
                      return $ BaseImage (M.map toPixel img)
    where
        toPixel (MIO.PixelY' y) = Pixel1 y
        {-# INLINE toPixel #-}
        readImageGrey' :: FilePath -> IO (MIO.Image M.S (MIO.Y' MIO.SRGB) Word8)
        readImageGrey' = MIO.readImageAuto
        {-# INLINE readImageGrey' #-}

-- | Read an image as RGB, given its path.
readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB fp = do img <- readImageRGB' fp
                     return $ BaseImage (M.map toPixel img)
    where
        toPixel (MIO.PixelSRGB r g b) = Pixel3 r g b
        {-# INLINE toPixel #-}
        readImageRGB' :: FilePath -> IO (MIO.Image M.S (MIO.SRGB MIO.NonLinear) Word8)
        readImageRGB' = MIO.readImageAuto
        {-# INLINE readImageRGB' #-}

-- | Read an image as RGBA, given its path.
readImageRGBA :: FilePath -> IO (Image RGBA)
readImageRGBA fp = do img <- readImageRGBA' fp
                      return $ BaseImage (M.map toPixel img)
    where
        toPixel (MIO.PixelSRGBA r g b a) = Pixel4 r g b a
        {-# INLINE toPixel #-}
        readImageRGBA' :: FilePath -> IO (MIO.Image M.S (MIO.Alpha (MIO.SRGB MIO.NonLinear)) Word8)
        readImageRGBA' = MIO.readImageAuto
        {-# INLINE readImageRGBA' #-}

-- | Read an image as HSV, given its path.
readImageHSV :: FilePath -> IO (Image HSV)
readImageHSV fp = fmap rgbaToHSV <$> readImageRGBA fp

-- | Read an image as HSL, given its path.
readImageHSL :: FilePath -> IO (Image HSL)
readImageHSL fp = fmap rgbaToHSL <$> readImageRGBA fp
