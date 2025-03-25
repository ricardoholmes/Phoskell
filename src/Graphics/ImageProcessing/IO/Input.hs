{-# LANGUAGE DataKinds #-}
module Graphics.ImageProcessing.IO.Input (
    readImageBinary,
    readImageGrey,
    readImageRGB,
    readImageRGBA,
    readImageHSV,
    readImageHSL,
) where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.IO as MIO
import Graphics.ImageProcessing.Core.Colour
import Graphics.ImageProcessing.Core.Pixel
import Graphics.ImageProcessing.Core.Image
import Graphics.ImageProcessing.Processes.Threshold (threshold)

readImageBinary :: FilePath -> IO (Image Binary)
readImageBinary fp = (:> threshold 127) <$> readImageGrey fp

readImageGrey :: FilePath -> IO (Image Grey)
readImageGrey fp = fmap rgbaToGrey <$> readImageRGBA fp

readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB fp = fmap rgbaToRGB <$> readImageRGBA fp

readImageRGBA :: FilePath -> IO (Image RGBA)
readImageRGBA fp = do img <- readImageRGBA' fp
                      let img' = M.map (\(MIO.PixelSRGBA r g b a) -> Pixel4 r g b a) img
                      return $ BaseImage img'
        where
            readImageRGBA' :: FilePath -> IO (MIO.Image M.S (MIO.Alpha (MIO.SRGB MIO.NonLinear)) Word8)
            readImageRGBA' = MIO.readImageAuto

readImageHSV :: FilePath -> IO (Image HSV)
readImageHSV fp = fmap rgbaToHSV <$> readImageRGBA fp

readImageHSL :: FilePath -> IO (Image HSL)
readImageHSL fp = fmap rgbaToHSL <$> readImageRGBA fp
