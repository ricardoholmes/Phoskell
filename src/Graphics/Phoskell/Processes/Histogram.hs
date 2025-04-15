{-# LANGUAGE ScopedTypeVariables #-}
-- | Histogram manipulation processes.
module Graphics.Phoskell.Processes.Histogram (
    contrastStretch,
    contrastStretchToRange,
    equaliseHistogram,
) where

import Data.Word (Word8)
import Graphics.Phoskell.Core.Image (ArrayProcess (..), Image (..))
import Graphics.Phoskell.Core.Pixel (Pixel)
import Graphics.Phoskell.Analysis.Histogram (Histogrammable (histogram))
import qualified Data.Massiv.Array as M
import qualified Data.Vector as V
import Data.List (mapAccumL)

-- | Contrast stretch with range [0..255].
contrastStretch :: (Pixel p, Histogrammable p) => ArrayProcess (p Word8) (p Word8)
contrastStretch = contrastStretchToRange (0,255)

-- | Given lower and upper bound, stretch values to keep them within that range.
contrastStretchToRange :: (Pixel p, Histogrammable p) => (Word8,Word8) -> ArrayProcess (p Word8) (p Word8)
contrastStretchToRange (l,u) = ArrayProcess (\img ->
        let hist = histogram (BaseImage img)
            (indices :: [[Int]]) = [[idx | (v,idx) <- zip hx [0..], v > 0] | hx <- hist]
            lowest = fmap head indices
            highest = fmap last indices
            oldRange = zipWith (-) highest lowest
            newRange = fromIntegral $ u - l
            l' = fromIntegral l
            stretch :: Int -> Word8 -> Word8
            stretch c p = let p' = fromIntegral p
                              q = (p' - (lowest !! c)) * newRange
                              q' = (q `div` (oldRange !! c)) + l'
                          in fromIntegral q'
            lookupTable = fmap (\i -> V.generate 256 (stretch i . fromIntegral)) (getChannelIndices 0)
            lookup' p = (V.!) <$> lookupTable <*> (fromIntegral <$> p)
            stretch' = fmap (uncurry stretch) . addChannelIndices
        in if M.elemsCount img > 256
                then M.map lookup' img
                else M.map stretch' img -- don't use lookup table if the image is too small
    )

-- | Apply histogram equalisation algorithm to the image.
--
-- More details on [Wikipedia](https://en.wikipedia.org/wiki/Histogram_equalization).
equaliseHistogram :: (Pixel p, Histogrammable p) => ArrayProcess (p Word8) (p Word8)
equaliseHistogram = ArrayProcess (\img ->
        let hist = histogram (BaseImage img)
            (total :: Double) = fromIntegral $ M.elemsCount img
            cdf c i = fromIntegral (sum $ take (fromIntegral i) (hist !! c))
            cdfMin = [head [idx | (v,idx) <- zip hx [0..], v > 0] | hx <- hist]
            h c v = round ((cdf c v - (cdfMin !! c)) / (total - (cdfMin !! c)) * 255)
            lookupTable = fmap (V.generate 256 . h) (getChannelIndices 0)
            lookup' p = (V.!) <$> lookupTable <*> (fromIntegral <$> p)
            hPixel = fmap (uncurry h) . addChannelIndices
        in if M.elemsCount img > 256
                then M.map lookup' img
                else M.map hPixel img -- don't use lookup table if the image is too small
    )

addChannelIndices :: Pixel p => p Word8 -> p (Int,Word8)
addChannelIndices = snd . mapAccumL (\n x -> (n+1,(n,x))) 0

getChannelIndices :: Pixel p => p Word8 -> p Int
getChannelIndices = snd . mapAccumL (\n _ -> (n+1,n)) 0
