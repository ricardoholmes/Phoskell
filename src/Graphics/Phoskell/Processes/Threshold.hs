-- | Thresholding processes.
module Graphics.Phoskell.Processes.Threshold (
    threshold,
    invThreshold,
    otsuThreshold,
    invOtsuThreshold,
) where

import Graphics.Phoskell.Core.Image
import Graphics.Phoskell.Core.Pixel (Pixel1(..))
import Graphics.Phoskell.Core.Colour (Binary, Grey)
import Graphics.Phoskell.Analysis.Histogram (histogram1)

-- | Apply threshold with threshold value given.
threshold :: Ord a => a -> PointProcess a Binary
threshold t = PointProcess (Pixel1 . (>t))

-- | Apply inverse threshold with threshold value given.
invThreshold :: Ord a => a -> PointProcess a Binary
invThreshold t = PointProcess (Pixel1 . (<=t))

-- | Apply thresholding using Otsu's method
otsuThreshold :: ArrayProcess Grey Binary
otsuThreshold = ArrayProcess (\img ->
        let hist = histogram1 (BaseImage img)
            -- intraclass variance
            var t = ω0 t * ω1 t * (μ0 t - μ1 t)^(2 :: Int)
            -- class probabilities
            ω0 t = sum (take t hist)
            ω1 t = sum (drop t hist)
            -- class means
            μ0 t = sum (take t (zipWith (*) [0..] hist)) `div` ω0 t
            μ1 t = sum (drop t (zipWith (*) [0..] hist)) `div` ω1 t
            -- thresholds
            ts = [if ω0 t > 0 && ω1 t > 0 then var t else 0 | t <- [1..255]]
            bestT = snd $ maximum (zip ts [1..])
        in applyProcess (threshold bestT) img
    )

-- | Apply thresholding using Otsu's method, with the output inverted
invOtsuThreshold :: ArrayProcess Grey Binary
invOtsuThreshold = ArrayProcess (fmap (fmap not) . applyProcess otsuThreshold)
