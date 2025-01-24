module Graphics.ImageProcessing.Analysis (
    histogramGray,
    imageSize
) where

import Graphics.ImageProcessing.Analysis.Histogram ( histogramGray )
import Graphics.ImageProcessing.Core.Image ( Image, toArray )
import Data.Massiv.Array ( Sz2, Size (size) )

-- | Gets the dimensions of the image given.
imageSize :: Image a -> Sz2
imageSize = size . toArray
