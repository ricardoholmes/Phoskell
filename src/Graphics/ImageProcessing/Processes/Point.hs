module Graphics.ImageProcessing.Processes.Point (
    addBias,
    subtractBias,
    applyGain,
    gammaCorrect,
    alterRed,
    alterGreen,
    alterBlue,
) where

import Graphics.ImageProcessing.Processes ( PointProcess(..) )
import Graphics.ImageProcessing.Core.Pixel ( Pixel, Pixel3 (..) )
import Data.Word ( Word8 )
import Data.Ord (clamp)
import Graphics.ImageProcessing.Core.Color (RGB)

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
--
-- Accounts for and prevents integer underflow.
applyGain :: (Pixel p) => Double -> PointProcess (p Word8) (p Word8)
applyGain m = PointProcess (fmap appGain')
    where
        appGain' a = let a' = fromIntegral a
                         z = a' * m
                         z' = clamp (0,255) z
                      in round z'

gammaCorrect :: (Pixel p) => Double -> PointProcess (p Word8) (p Word8)
gammaCorrect g = PointProcess (fmap gammaCorr')
    where
        gammaCorr' x = let x' = fromIntegral x -- convert to double
                           y  = (x' / 255) ** g -- raise normalised value (in [0,1]) to power of gamma
                        in round (y * 255) -- convert back to word

alterRed :: (Word8 -> Word8) -> PointProcess RGB RGB
alterRed f = PointProcess (\(Pixel3 r g b) -> Pixel3 (f r) g b)

alterGreen :: (Word8 -> Word8) -> PointProcess RGB RGB
alterGreen f = PointProcess (\(Pixel3 r g b) -> Pixel3 r (f g) b)

alterBlue :: (Word8 -> Word8) -> PointProcess RGB RGB
alterBlue f = PointProcess (\(Pixel3 r g b) -> Pixel3 r g (f b))
