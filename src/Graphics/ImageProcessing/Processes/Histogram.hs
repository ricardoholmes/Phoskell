{-# LANGUAGE ScopedTypeVariables #-}
-- | Histogram manipulation functions
module Graphics.ImageProcessing.Processes.Histogram (
    contrastStretch,
    contrastStretchToRange,
    equaliseHistogram,
) where

import Graphics.ImageProcessing.Core.Image (MiscProcess (..), Image (..), PointProcess (..), ImageProcess (applyProcess))
import Graphics.ImageProcessing.Core.Color (Gray)
import Data.Word (Word8)
import Graphics.ImageProcessing.Analysis.Histogram (histogram1)
import Graphics.ImageProcessing.Core.Pixel (Pixel1(..))
import qualified Data.Massiv.Array as M

contrastStretch :: MiscProcess Gray Gray
contrastStretch = contrastStretchToRange (0,255)

contrastStretchToRange :: (Word8,Word8) -> MiscProcess Gray Gray
contrastStretchToRange (l,u) = MiscProcess (\img ->
        let hist = histogram1 (BaseImage img)
            (indices :: [Int]) = map fst . filter (\(_,c) -> c > 0) $ zip [0..] hist
            lowest = head indices
            highest = last indices
            oldRange = highest - lowest
            newRange = fromIntegral $ u - l
            l' = fromIntegral l
            process = PointProcess (\(Pixel1 p) ->
                    let p' = fromIntegral p
                        q = (p' - lowest) * newRange
                        q' = (q `div` oldRange) + l'
                    in Pixel1 (fromIntegral q')
                )
        in applyProcess process img
    )

-- | Apply histogram equalisation algorithm to the image.
--
-- More details on [Wikipedia](https://en.wikipedia.org/wiki/Histogram_equalization).
equaliseHistogram :: MiscProcess Gray Gray
equaliseHistogram = MiscProcess (\img ->
        let hist = histogram1 (BaseImage img)
            (total :: Double) = fromIntegral $ sum hist
            cdf i = fromIntegral (sum $ take (fromIntegral i) hist)
            cdfMin = head [idx | (v,idx) <- zip hist [0..], v > 0]
            h v = round ((cdf v - cdfMin) / (total - cdfMin) * 255)
        in M.map (\(Pixel1 p) -> Pixel1 $ h p) img
    )
