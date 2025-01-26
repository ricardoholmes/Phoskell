module Graphics.ImageProcessing.Transformations.Rotation (
    rotate90,
    rotate180,
    rotate270,
) where

import Graphics.ImageProcessing.Processes (MiscProcess (..))
import qualified Data.Massiv.Array as M

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
