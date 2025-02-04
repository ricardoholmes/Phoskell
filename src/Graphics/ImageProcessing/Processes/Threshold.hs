module Graphics.ImageProcessing.Processes.Threshold (
    threshold,
    invThreshold,
    otsuThreshold,
) where

import Graphics.ImageProcessing.Core.Image
import Graphics.ImageProcessing.Core.Pixel (Pixel1(..))
import Graphics.ImageProcessing.Core.Color (Binary, Gray)
import Graphics.ImageProcessing.Analysis.Histogram (histogram1)

threshold :: Ord a => a -> PointProcess a Binary
threshold t = PointProcess (Pixel1 . (>=t))

invThreshold :: Ord a => a -> PointProcess a Binary
invThreshold t = PointProcess (Pixel1 . (<=t))

-- | Apply thresholding using Otsu's method
otsuThreshold :: MiscProcess Gray Binary
otsuThreshold = MiscProcess (\img ->
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
