module Graphics.ImageProcessing.Analysis (
    histogram1,
    imageSize
) where

import Graphics.ImageProcessing.Analysis.Histogram ( histogram1 )
import Graphics.ImageProcessing.Core.Image ( Image, toArray )
import Data.Massiv.Array ( Size (size), Sz (Sz2) )

-- | Gets the dimensions of the image given.
imageSize :: Image a -> (Int,Int)
imageSize img = (w,h)
    where Sz2 h w = size (toArray img)
