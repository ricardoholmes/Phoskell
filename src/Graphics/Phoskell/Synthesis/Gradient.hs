module Graphics.Phoskell.Synthesis.Gradient (
    -- gradients
    simpleGradientH,
    simpleGradientV,
    multiColourGradientH,
    multiColourGradientV,
) where

import Data.Massiv.Array (Ix2(..))

import qualified Data.Massiv.Array as M
import qualified Data.Vector as V

import Graphics.Phoskell.Core
import Graphics.Phoskell.Synthesis.Internal

-- | Generate an image made up of a 2-colour linear horizontal gradient.
simpleGradientH :: Pixel p => (Int,Int) -- ^ Size in terms @(width,height)@.
                           -> p Word8 -- ^ Initial colour, at the leftmost part of the image.
                           -> p Word8 -- ^ Final colour, at the rightmost part of the image.
                           -> Image (p Word8)
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

-- | Generate an image made up of a 2-colour linear vertical gradient.
simpleGradientV :: Pixel p => (Int,Int) -- ^ Size in terms @(width,height)@.
                           -> p Word8 -- ^ Initial colour, at the top of the image.
                           -> p Word8 -- ^ Final colour, at the bottom of the image.
                           -> Image (p Word8)
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

-- | Generate an image made up of an n-colour linear horizontal gradient.
multiColourGradientH :: Pixel p => (Int,Int) -- ^ Size in terms @(width,height)@.
                                -> p Word8 -- ^ Initial colour, at the left of the image.
                                -> [p Word8] -- ^ Evenly-spaced colours to reach throughout the gradient.
                                -> Image (p Word8)
multiColourGradientH (w,h) c [] = canvas (w,h) c
multiColourGradientH (w,h) c cs = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(_:.x) ->
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

-- | Generate an image made up of an n-colour linear vertical gradient.
multiColourGradientV :: Pixel p => (Int,Int) -- ^ Size in terms @(width,height)@.
                                -> p Word8 -- ^ Initial colour, at the top of the image.
                                -> [p Word8] -- ^ Evenly-spaced colours to reach throughout the gradient.
                                -> Image (p Word8)
multiColourGradientV (w,h) c [] = canvas (w,h) c
multiColourGradientV (w,h) c cs = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(y:._) ->
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
