{-# LANGUAGE DataKinds #-}
module Graphics.Image.IO (
    readImageRGB,
    writeImageRGB,
) where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.IO as MIO

import Graphics.Image.Color
import Graphics.Image.Pixel
import Graphics.Image.Internal
import Graphics.Image.ImageProcess (PointProcess(PointProcess))
import Data.Ord (clamp)

readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB fp = do img <- readImageRGB' fp
                     let img' = M.map (\(MIO.PixelSRGB r g b) -> Pixel3 r g b) img
                     return $ BaseImage img'
        where
            readImageRGB' :: FilePath -> IO (MIO.Image M.S (MIO.SRGB MIO.NonLinear) Double)
            readImageRGB' = MIO.readImageAuto

writeImageRGB :: FilePath -> Image RGB -> IO ()
writeImageRGB fp img = MIO.writeImageAuto fp
                     $ M.map (\(Pixel3 r g b) -> MIO.PixelSRGB r g b)
                     $ toArray
                     $ img :> PointProcess (fmap (clamp (0,1)))
