module Graphics.Image.Threshold (
    threshold,
    invThreshold,
) where

import Graphics.Image.Color (Binary)
import Graphics.Image.ImageProcess (PointProcess (..))
import Graphics.Image.Pixel (Pixel1(..))

threshold :: (Ord a) => a -> PointProcess a Binary
threshold t = PointProcess (Pixel1 . (>t))

invThreshold :: (Ord a) => a -> PointProcess a Binary
invThreshold t = PointProcess (Pixel1 . (<t))
