module Graphics.ImageProcessing.Processes.Threshold (
    threshold,
    invThreshold,
) where

import Graphics.ImageProcessing.Core.Pixel (Pixel1(..))
import Graphics.ImageProcessing.Core.Color (Binary)
import Graphics.ImageProcessing.Processes (PointProcess (..))

threshold :: (Ord a) => a -> PointProcess a Binary
threshold t = PointProcess (Pixel1 . (>t))

invThreshold :: (Ord a) => a -> PointProcess a Binary
invThreshold t = PointProcess (Pixel1 . (<t))
