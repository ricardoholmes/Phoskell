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
import Control.Monad.State.Lazy

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
            stretch' :: Word8 -> State Int Word8
            stretch' p = do c <- get
                            put (c+1)
                            let p' = fromIntegral p
                            let q = (p' - (lowest !! c)) * newRange
                            let q' = (q `div` (oldRange !! c)) + l'
                            return (fromIntegral q')
        in M.map (\p -> evalState (mapM stretch' p) 0) img
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
