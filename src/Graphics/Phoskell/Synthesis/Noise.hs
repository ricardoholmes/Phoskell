module Graphics.Phoskell.Synthesis.Noise (
    -- noise generation
    uniformNoise,
    saltAndPepperNoise,
    gaussianNoise,
) where

import Data.Bifunctor (first)
import Data.List (mapAccumL)
import Data.Word (Word16)
import System.Random
import System.Random.Stateful (runStateGen)
import System.Random.MWC.Distributions (standard)

import qualified Data.Massiv.Array as M

import Graphics.Phoskell.Core
import Graphics.Phoskell.Synthesis.Internal

-- | Create an image made up of uniformly-distributed noise.
uniformNoise :: (Pixel p, Uniform a)
             => Int -- ^ Random seed.
             -> (Int,Int) -- ^ Size of the image in terms @(width,height)@.
             -> Image (p a)
uniformNoise seed (w,h) = BaseImage (M.delay $ M.computeAs M.B arr)
    where
        gen = mkStdGen seed
        swap (a,b) = (b,a)
        pixel0 = pure 0 :: Pixel p => p Word8
        getRandPixel g = mapAccumL (\g' _ -> swap (uniform g')) g pixel0
        arr = M.randomArray gen split (swap . getRandPixel) M.Par (M.Sz2 h w)

-- | Create an image made up of salt and pepper noise.
--
-- Note: if $p \le 0$ then all pixels will be black,
-- and if $p \ge 1$ then all pixels will be white.
saltAndPepperNoise :: Pixel p => Int -- ^ Random seed.
                              -> (Int,Int) -- ^ Size of the image in terms @(width,height)@.
                              -> Double -- ^ Probability, $p \in [0,1]$, of any given pixel being salt (white) rather than pepper (black).
                              -> Image (p Word8)
saltAndPepperNoise seed sz@(w,h) p
 | p <= 0 = canvas sz (pure minBound)
 | p >= 1 = canvas sz (pure maxBound)
 | otherwise = uniformNoise seed (w,h) :> PointProcess (fmap toSaltPepper)
    where
        toSaltPepper v = if v < p' then maxBound else minBound
        p' = round $ p * fromIntegral (maxBound :: Word16) :: Word16

-- | Create an image made up of Gaussian/normally-distributed noise.
--
-- Parameters:
-- - Random seed.
-- - Size of the image in terms @(width,height)@.
gaussianNoise :: Pixel p => Int -> (Int,Int) -> Image (p Word8)
gaussianNoise seed (w,h) = BaseImage (M.delay $ M.computeAs M.B arr)
    where
        gen = mkStdGen seed
        swap (a,b) = (b,a)
        pixel0 = pure 0 :: Pixel p => p Word8
        getRandom' g = runStateGen g standard
        clamp v = min (max 0 v) 255
        -- ~100% of values are within [-3,3] (since std dev = 1)
        normToWord8 x = floor $ clamp (256/6 * (x + 3)) :: Word8
        getRandom g = first normToWord8 (getRandom' g)
        getRandPixel g = mapAccumL (\g' _ -> swap (getRandom g')) g pixel0
        arr = M.randomArray gen split (swap . getRandPixel) M.Par (M.Sz2 h w)
