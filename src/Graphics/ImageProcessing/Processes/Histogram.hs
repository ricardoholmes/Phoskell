{-# LANGUAGE ScopedTypeVariables #-}
-- | Histogram manipulation functions
module Graphics.ImageProcessing.Processes.Histogram (
    contrastStretch,
    contrastStretchToRange
) where

import Graphics.ImageProcessing.Core.Image (MiscProcess (..), Image (..), PointProcess (..), ImageProcess (applyProcess))
import Graphics.ImageProcessing.Core.Color (Gray)
import Data.Word (Word8)
import Graphics.ImageProcessing.Analysis.Histogram (histogramGray)
import Graphics.ImageProcessing.Core.Pixel (Pixel1(..))

contrastStretch :: MiscProcess Gray Gray
contrastStretch = contrastStretchToRange (0,255)

contrastStretchToRange :: (Word8,Word8) -> MiscProcess Gray Gray
contrastStretchToRange (l,u) = MiscProcess (\img ->
        let hist = histogramGray (BaseImage img)
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
