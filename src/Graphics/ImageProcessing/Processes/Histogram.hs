{-# LANGUAGE ScopedTypeVariables #-}
-- | Histogram manipulation functions
module Graphics.ImageProcessing.Processes.Histogram (
    contrastStretch,
    contrastStretchToRange,
    equaliseHistogram,
) where

import Graphics.ImageProcessing.Core.Image (MiscProcess (..), Image (..))
import Data.Word (Word8)
import Graphics.ImageProcessing.Analysis.Histogram (Histogrammable (histogram))
import Graphics.ImageProcessing.Core.Pixel (Pixel)
import qualified Data.Massiv.Array as M
import qualified Data.Vector as V
import Data.List (mapAccumL)

contrastStretch :: (Pixel p, Histogrammable p) => MiscProcess (p Word8) (p Word8)
contrastStretch = contrastStretchToRange (0,255)

contrastStretchToRange :: (Pixel p, Histogrammable p) => (Word8,Word8) -> MiscProcess (p Word8) (p Word8)
contrastStretchToRange (l,u) = MiscProcess (\img ->
        let hist = histogram (BaseImage img)
            (indices :: [[Int]]) = [[idx | (v,idx) <- zip hx [0..], v > 0] | hx <- hist]
            lowest = fmap head indices
            highest = fmap last indices
            oldRange = zipWith (-) highest lowest
            newRange = fromIntegral $ u - l
            l' = fromIntegral l
            stretch :: (Int, Word8) -> Word8
            stretch (c,p) = let p' = fromIntegral p
                                q = (p' - (lowest !! c)) * newRange
                                q' = (q `div` (oldRange !! c)) + l'
                            in fromIntegral q'
            cs = length hist -- number of channels
            lookupTable = V.generate 256 (\v -> V.generate cs (\i -> stretch (i, fromIntegral v)))
            lookup' = fmap (\(i,v) -> lookupTable V.! fromIntegral v V.! i) . addChannelIndices
            stretch' = fmap stretch . addChannelIndices
        in if M.elemsCount img > 256 * cs
                then M.map lookup' img
                else M.map stretch' img -- don't use lookup table if the image is too small
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
            h (c,v) = round ((cdf c v - (cdfMin !! c)) / (total - (cdfMin !! c)) * 255)
            cs = length hist -- number of channels
            lookupTable = V.generate 256 (V.generate cs . flip (curry h))
            lookup' = fmap (\(i,v) -> lookupTable V.! fromIntegral v V.! i) . addChannelIndices
            hPixel = fmap h . addChannelIndices
        in if M.elemsCount img > 256 * cs
                then M.map lookup' img
                else M.map hPixel img -- don't use lookup table if the image is too small
    )

addChannelIndices :: Pixel p => p Word8 -> p (Int,Word8)
addChannelIndices = snd . mapAccumL (\n x -> (n+1,(n,x))) 0
