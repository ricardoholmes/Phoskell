module Graphics.ImageProcessing.Synthesis.Gradient (
    -- gradients
    simpleGradientH,
    simpleGradientV,
    multiColorGradientH,
    multiColorGradientV,
) where

import Data.Massiv.Array (Ix2(..))
import Data.Word (Word8)

import qualified Data.Massiv.Array as M
import qualified Data.Vector as V

import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Synthesis.Internal

-- | Generate an image made up of a 2-color linear horizontal gradient.
--
-- Parameters:
-- - Size in terms @(width,height)@.
-- - Initial color, at the leftmost part of the image.
-- - Final color, at the rightmost part of the image.
simpleGradientH :: Pixel p => (Int,Int) -> p Word8 -> p Word8 -> Image (p Word8)
simpleGradientH (w,h) l r = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(_:.x) ->
                                vals V.! x
                            )
    where
        percent :: Int -> Double
        percent val = fromIntegral val / fromIntegral (w-1)
        l' = fromIntegral <$> l
        r' = fromIntegral <$> r
        lerp p = l' + ((r' - l') `multScalar` p)
        vals = V.generate w (fmap round . lerp . percent)

-- | Generate an image made up of a 2-color linear vertical gradient.
--
-- Parameters:
-- - Size in terms @(width,height)@.
-- - Initial color, at the top of the image.
-- - Final color, at the bottom of the image.
simpleGradientV :: Pixel p => (Int,Int) -> p Word8 -> p Word8 -> Image (p Word8)
simpleGradientV (w,h) l r = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(y:._) ->
                                vals V.! y
                            )
    where
        percent :: Int -> Double
        percent val = fromIntegral val / fromIntegral (h-1)
        l' = fromIntegral <$> l
        r' = fromIntegral <$> r
        lerp p = l' + ((r' - l') `multScalar` p)
        vals = V.generate h (fmap round . lerp . percent)

-- | Generate an image made up of an n-color linear horizontal gradient.
--
-- Parameters:
-- - Size in terms @(width,height)@.
-- - Initial color, at the left of the image.
-- - Evenly-spaced colors to reach throughout the gradient.
multiColorGradientH :: Pixel p => (Int,Int) -> p Word8 -> [p Word8] -> Image (p Word8)
multiColorGradientH (w,h) c [] = canvas (w,h) c
multiColorGradientH (w,h) c cs = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(_:.x) ->
                                    vals V.! x
                                )
    where
        len = length cs
        percent :: Int -> Double
        percent val = fromIntegral (len * val) / fromIntegral w
        cs' = fmap fromIntegral <$> (c:cs)
        lerp p = if (floor p :: Int) == (ceiling p :: Int)
                    then cs' !! floor p
                    else
                        let l = cs' !! floor p
                            r = cs' !! ceiling p
                            p' = p - fromIntegral (floor p :: Int)
                        in l + ((r - l) `multScalar` p')
        vals = V.generate w (fmap round . lerp . percent)

-- | Generate an image made up of an n-color linear vertical gradient.
--
-- Parameters:
-- - Size in terms @(width,height)@.
-- - Initial color, at the top of the image.
-- - Evenly-spaced colors to reach throughout the gradient.
multiColorGradientV :: Pixel p => (Int,Int) -> p Word8 -> [p Word8] -> Image (p Word8)
multiColorGradientV (w,h) c [] = canvas (w,h) c
multiColorGradientV (w,h) c cs = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(y:._) ->
                                    vals V.! y
                                )
    where
        len = length cs
        percent :: Int -> Double
        percent val = fromIntegral (len * val) / fromIntegral h
        cs' = fmap fromIntegral <$> (c:cs)
        lerp p = if (floor p :: Int) == (ceiling p :: Int)
                    then cs' !! floor p
                    else
                        let l = cs' !! floor p
                            r = cs' !! ceiling p
                            p' = p - fromIntegral (floor p :: Int)
                        in l + ((r - l) `multScalar` p')
        vals = V.generate h (fmap round . lerp . percent)
