{-# LANGUAGE DataKinds #-}
module Graphics.ImageProcessing.IO.Output (
    writeImageBinary,
    writeImageGray,
    writeImageRGB,
    writeImageRGBA,
    writeImageHSV,
    writeImageHSL,
) where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.IO as MIO
import Graphics.ImageProcessing.Core.Color
import Graphics.ImageProcessing.Core.Pixel
import Graphics.ImageProcessing.Core.Image
import Graphics.ImageProcessing.Processes (PointProcess(PointProcess))
import Data.Ord (clamp)
import Data.Bool (bool)

writeImageBinary :: FilePath -> Image Binary -> IO ()
writeImageBinary fp img = MIO.writeImageAuto fp
                        $ toArray
                        $ img :> PointProcess binToBit
        where
            binToBit :: Binary -> MIO.Pixel (MIO.Y' MIO.SRGB) MIO.Bit
            binToBit (Pixel1 x) = MIO.PixelY' (bool MIO.zero MIO.one x)

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
            convertPixelType :: HSV -> MIO.Pixel (MIO.HSV (MIO.SRGB MIO.NonLinear)) Double
            convertPixelType (Pixel3 h s v) = MIO.PixelHSV h s v

writeImageHSL :: FilePath -> Image HSL -> IO ()
writeImageHSL fp img = MIO.writeImageAuto fp
                     $ M.map convertPixelType
                     $ toArray
                     $ img :> PointProcess (fmap (clamp (0,1)))
        where
            convertPixelType :: HSL -> MIO.Pixel (MIO.HSL (MIO.SRGB MIO.NonLinear)) Double
            convertPixelType (Pixel3 h s l) = MIO.PixelHSL h s l
