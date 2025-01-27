module Graphics.ImageProcessing.Transformations.Rotation (
    rotate90,
    rotate180,
    rotate270,
    rotateDeg,
    rotate,
) where

import Graphics.ImageProcessing.Processes (MiscProcess (..))
import qualified Data.Massiv.Array as M
import Data.Massiv.Array (Ix2((:.)))
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe)

-- | Rotate the image 90 degrees clockwise.
--
-- Implemented with transposition and mirroring on the x axis.
rotate90 :: MiscProcess a a
rotate90 = MiscProcess (M.reverse M.Dim1 . M.transpose)

-- | Rotate the image 180 degrees clockwise.
--
-- Implemented with mirroring on both axes.
rotate180 :: MiscProcess a a
rotate180 = MiscProcess (M.reverse M.Dim1 . M.reverse M.Dim2)

-- | Rotate the image 270 degrees clockwise.
--
-- Implemented with transposition and mirroring on the y axis.
rotate270 :: MiscProcess a a
rotate270 = MiscProcess (M.reverse M.Dim2 . M.transpose)

-- | Apply a rotation given an angle in radians.
--
-- Uses nearest neighbour with no interpolation.
--
-- Takes rotation and value to use for background.
rotate :: NFData a => Double -> a -> MiscProcess a a
rotate t v = MiscProcess (\img ->
        let img' = M.computeAs M.BN img
            (M.Sz2 h w) = M.size img'
            centreY = fromIntegral (h - 1) / 2 -- -1 accounts for 0 indexing
            centreX = fromIntegral (w - 1) / 2
            calcX x y = centreX + ((x-centreX)*cos t - (y-centreY)*sin t)
            calcY x y = centreY + ((x-centreX)*sin t + (y-centreY)*cos t)
        in M.imap (\(y:.x) _ ->
            let x' = fromIntegral x
                y' = fromIntegral y
                newX = round $ calcX x' y'
                newY = round $ calcY x' y'
            in fromMaybe v $ M.index img' (newY:.newX)
        ) img'
    )

-- | Apply a rotation given an angle in degrees.
--
-- Equivalent to `rotate` but is given degrees rather than radians.
--
-- Takes angle of rotation and value to use for background
rotateDeg :: NFData a => Double -> a -> MiscProcess a a
rotateDeg d = rotate (d * pi / 180)
