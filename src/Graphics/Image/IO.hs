{-# LANGUAGE DataKinds #-}
module Graphics.Image.IO (
    readImageGray,
    readImageRGB,
    readImageRGBA,
    readImageHSV,

    writeImageGray,
    writeImageRGB,
    writeImageRGBA,
    writeImageHSV,
) where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.IO as MIO

import Graphics.Image.Color
import Graphics.Image.Pixel
import Graphics.Image.Internal
import Graphics.Image.ImageProcess (PointProcess(PointProcess))
import Data.Ord (clamp)

-- Read Images --

readImageGray :: FilePath -> IO (Image Gray)
readImageGray fp = do img <- readImageRGB' fp
                      let img' = M.map (\(MIO.PixelY' y) -> Pixel1 y) img
                      return $ BaseImage img'
        where
            readImageRGB' :: FilePath -> IO (MIO.Image M.S (MIO.Y' MIO.SRGB) Double)
            readImageRGB' = MIO.readImageAuto

readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB fp = do img <- readImageRGB' fp
                     let img' = M.map (\(MIO.PixelSRGB r g b) -> Pixel3 r g b) img
                     return $ BaseImage img'
        where
            readImageRGB' :: FilePath -> IO (MIO.Image M.S (MIO.SRGB MIO.NonLinear) Double)
            readImageRGB' = MIO.readImageAuto

readImageRGBA :: FilePath -> IO (Image RGBA)
readImageRGBA fp = do img <- readImageRGBA' fp
                      let img' = M.map (\(MIO.PixelSRGBA r g b a) -> Pixel4 r g b a) img
                      return $ BaseImage img'
        where
            readImageRGBA' :: FilePath -> IO (MIO.Image M.S (MIO.Alpha (MIO.SRGB MIO.NonLinear)) Double)
            readImageRGBA' = MIO.readImageAuto

readImageHSV :: FilePath -> IO (Image HSV)
readImageHSV fp = do img <- readImageHSV' fp
                     let img' = M.map (\(MIO.PixelHSV h s v) -> Pixel3 h s v) img
                     return $ BaseImage img'
        where
            readImageHSV' :: FilePath -> IO (MIO.Image M.S (MIO.HSV (MIO.SRGB MIO.NonLinear)) Double)
            readImageHSV' = MIO.readImageAuto

-- Write Images --

writeImageGray :: FilePath -> Image Gray -> IO ()
writeImageGray fp img = MIO.writeImageAuto fp
                     $ M.map grayToY'
                     $ toArray
                     $ img :> PointProcess (fmap (clamp (0,1)))
        where
            grayToY' :: Gray -> MIO.Pixel (MIO.Y' MIO.SRGB) Double
            grayToY' (Pixel1 y) = MIO.PixelY' y

writeImageRGB :: FilePath -> Image RGB -> IO ()
writeImageRGB fp img = MIO.writeImageAuto fp
                     $ M.map (\(Pixel3 r g b) -> MIO.PixelSRGB r g b)
                     $ toArray
                     $ img :> PointProcess (fmap (clamp (0,1)))

writeImageRGBA :: FilePath -> Image RGBA -> IO ()
writeImageRGBA fp img = MIO.writeImageAuto fp
                     $ M.map (\(Pixel4 r g b a) -> MIO.PixelSRGBA r g b a)
                     $ toArray
                     $ img :> PointProcess (fmap (clamp (0,1)))

writeImageHSV :: FilePath -> Image HSV -> IO ()
writeImageHSV fp img = MIO.writeImageAuto fp
                     $ M.map convertPixelType
                     $ toArray
                     $ img :> PointProcess (fmap (clamp (0,1)))
        where
            convertPixelType :: Pixel3 a -> MIO.Pixel (MIO.HSV (MIO.SRGB MIO.NonLinear)) a
            convertPixelType (Pixel3 h s v) = MIO.PixelHSV h s v
