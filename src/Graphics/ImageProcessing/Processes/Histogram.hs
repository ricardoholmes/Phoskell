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
import Graphics.ImageProcessing.Analysis.Histogram (histogram1, Histogrammable (histogram))
import Graphics.ImageProcessing.Core.Pixel (Pixel1(..), Pixel)
import qualified Data.Massiv.Array as M
import Control.Monad.State.Lazy

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
equaliseHistogram :: (Pixel p, Histogrammable p) => MiscProcess (p Word8) (p Word8)
equaliseHistogram = MiscProcess (\img ->
        let hist = histogram (BaseImage img)
            (total :: Double) = fromIntegral $ M.elemsCount img
            cdf c i = fromIntegral (sum $ take (fromIntegral i) (hist !! c))
            cdfMin = [head [idx | (v,idx) <- zip hx [0..], v > 0] | hx <- hist]
            h c v = round ((cdf c v - (cdfMin !! c)) / (total - (cdfMin !! c)) * 255)
            hPixel :: Word8 -> State Int Word8
            hPixel v = do c <- get
                          put (c+1)
                          return $ h c v
        in M.map (\p -> evalState (mapM hPixel p) 0) img
    )
