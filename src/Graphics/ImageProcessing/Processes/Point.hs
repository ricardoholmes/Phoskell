module Graphics.ImageProcessing.Processes.Point (
    addBias,
    subtractBias,
    applyGain,
) where

import Graphics.ImageProcessing.Processes ( PointProcess(..) )
import Graphics.ImageProcessing.Core.Pixel ( Pixel )
import Data.Word ( Word8 )
import Data.Ord (clamp)

-- | Increment intensity of all pixels by adding the given value.
--
-- Prevents integer overflow.
addBias :: (Pixel p) => Word8 -> PointProcess (p Word8) (p Word8)
addBias x = PointProcess (fmap addBias')
    where
        addBias' a = let z = a + x
                         carry = z < a -- will be true if there was overflow
                      in if carry then maxBound else z

-- | Decrement intensity of all pixels by subtracting the given value.
--
-- Prevents integer underflow.
subtractBias :: (Pixel p) => Word8 -> PointProcess (p Word8) (p Word8)
subtractBias x = PointProcess (fmap subBias')
    where
        subBias' a = let z = a - x -- subtract first param from second param (to avoid flip or infix)
                         underflow = z > a -- only true if there was underflow
                      in if underflow then minBound else z

-- | Multiply all pixels by the given value.
applyGain :: (Pixel p) => Double -> PointProcess (p Word8) (p Word8)
applyGain m = PointProcess (fmap appGain')
    where
        appGain' a = let a' = fromIntegral a
                         z = a' * m
                         z' = clamp (0,255) z
                      in round z'
