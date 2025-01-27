{-# LANGUAGE DataKinds #-}
module Graphics.ImageProcessing.IO.Input (
    readImageBinary,
    readImageGray,
    readImageRGB,
    readImageRGBA,
    readImageHSV,
    readImageHSL,
) where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.IO as MIO
import Graphics.ImageProcessing.Core.Color
import Graphics.ImageProcessing.Core.Pixel
import Graphics.ImageProcessing.Core.Image
import GHC.Word ( Word8 )

readImageBinary :: FilePath -> IO (Image Binary)
readImageBinary fp = do img <- readImageBin' fp
                        let img' = M.map (\(MIO.PixelY' p) -> Pixel1 (p == MIO.one)) img
                        return $ BaseImage img'
        where
            readImageBin' :: FilePath -> IO (MIO.Image M.S (MIO.Y' MIO.SRGB) MIO.Bit)
            readImageBin' = MIO.readImageAuto

readImageGray :: FilePath -> IO (Image Gray)
readImageGray fp = do img <- readImageRGB' fp
                      let img' = M.map (\(MIO.PixelY' y) -> Pixel1 y) img
                      return $ BaseImage img'
        where
            readImageRGB' :: FilePath -> IO (MIO.Image M.S (MIO.Y' MIO.SRGB) Word8)
            readImageRGB' = MIO.readImageAuto

readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB fp = do img <- readImageRGB' fp
                     let img' = M.map (\(MIO.PixelSRGB r g b) -> Pixel3 r g b) img
                     return $ BaseImage img'
        where
            readImageRGB' :: FilePath -> IO (MIO.Image M.S (MIO.SRGB MIO.NonLinear) Word8)
            readImageRGB' = MIO.readImageAuto

readImageRGBA :: FilePath -> IO (Image RGBA)
readImageRGBA fp = do img <- readImageRGBA' fp
                      let img' = M.map (\(MIO.PixelSRGBA r g b a) -> Pixel4 r g b a) img
                      return $ BaseImage img'
        where
            readImageRGBA' :: FilePath -> IO (MIO.Image M.S (MIO.Alpha (MIO.SRGB MIO.NonLinear)) Word8)
            readImageRGBA' = MIO.readImageAuto

readImageHSV :: FilePath -> IO (Image HSV)
readImageHSV fp = do img <- readImageHSV' fp
                     let img' = M.map (\(MIO.PixelHSV h s v) -> Pixel3 h s v) img
                     return $ BaseImage img'
        where
            readImageHSV' :: FilePath -> IO (MIO.Image M.S (MIO.HSV (MIO.SRGB MIO.NonLinear)) Word8)
            readImageHSV' = MIO.readImageAuto

readImageHSL :: FilePath -> IO (Image HSL)
readImageHSL fp = do img <- readImageHSL' fp
                     let img' = M.map (\(MIO.PixelHSL h s l) -> Pixel3 h s l) img
                     return $ BaseImage img'
        where
            readImageHSL' :: FilePath -> IO (MIO.Image M.S (MIO.HSL (MIO.SRGB MIO.NonLinear)) Word8)
            readImageHSL' = MIO.readImageAuto
