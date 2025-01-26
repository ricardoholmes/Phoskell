module Graphics.ImageProcessing.Transformations.Rotation (
    RotationDirection(..),
    rotate90,
    rotate180,
) where

import Graphics.ImageProcessing.Processes (MiscProcess (..))
import qualified Data.Massiv.Array as M

data RotationDirection = Clockwise | Anticlockwise

-- | Rotate the image 90 degrees clockwise or anticlockwise
--
-- Clockwise or anticlockwise determined by value given
--
-- Implemented with transposition and mirroring (axis depends on direction).
rotate90 :: RotationDirection -> MiscProcess a a
rotate90 Clockwise = MiscProcess (M.reverse M.Dim1 . M.transpose)
rotate90 Anticlockwise = MiscProcess (M.reverse M.Dim2 . M.transpose)

-- | Rotate the image 180 degrees
--
-- Implemented with mirroring on both axes.
rotate180 :: MiscProcess a a
rotate180 = MiscProcess (M.reverse M.Dim1 . M.reverse M.Dim2)
