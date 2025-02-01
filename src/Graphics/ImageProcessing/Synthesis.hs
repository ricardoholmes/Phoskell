module Graphics.ImageProcessing.Synthesis (
    canvas,
    generateImage,
    generateImage',
    simpleGradientH,
    simpleGradientV,
    multiColorGradientH,
    multiColorGradientV,
) where

import Graphics.ImageProcessing.Core
import qualified Data.Massiv.Array as M
import Data.Word (Word8)
import Data.Massiv.Array (Ix2(..))

-- | Generate a blank image filled with a single value.
--
-- - First parameter is the size in terms @(width,height)@.
-- - Second parameter is the value to use to fill.
canvas :: (Int,Int) -> a -> Image a
canvas (w,h) x = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (const x)

-- | Generate an image from a function.
--
-- - First parameter is the bottom-left coordinates in terms @(x,y)@.
-- - Second parameter is the top-right coordinates in terms @(x,y)@.
-- - Third parameter is the function to use, taking @(x,y)@ and returning the the pixel value.
-- 
-- Values not in the range [0..255] will be clamped to be in the range.
generateImage :: (Pixel p, Integral a, Integral b) => (Int,Int) -> (Int,Int) -> ((a,a) -> p b) -> Image (p Word8)
generateImage (xm,ym) (xM,yM) f = BaseImage $ M.makeArrayR M.D M.Par sz (\(y:.x) ->
                                    let y' = fromIntegral (y + ym)
                                        x' = fromIntegral (x + xm)
                                    in fromIntegral . clamp <$> f (x',y')
                                )
    where
        sz = M.Sz2 (yM - ym + 1) (xM - xm + 1)
        clamp v = min (max 0 v) 255

-- | Generate an image from a function.
--
-- Equivalent to @generateImage@, except the functions returns non-integer pixel values
-- between 0 and 1 (values outside of the range will be clamped).
generateImage' :: (Pixel p, RealFloat a) => (Int,Int) -> (Int,Int) -> ((a,a) -> p a) -> Image (p Word8)
generateImage' (xm,ym) (xM,yM) f = BaseImage $ M.makeArrayR M.D M.Par sz (\(y:.x) ->
                                    let y' = fromIntegral (y + ym)
                                        x' = fromIntegral (x + xm)
                                    in round . (*255) . clamp01 <$> f (x',y')
                                )
    where
        sz = M.Sz2 (yM - ym + 1) (xM - xm + 1)
        clamp01 v = min (max 0 v) 1

-- | Generate an image made up of a 2-color linear horizontal gradient.
--
-- Parameters:
-- - Size in terms `(width,height)`.
-- - Color at the left of the image.
-- - Color at the right of the image.
simpleGradientH :: Pixel p => (Int,Int) -> p Word8 -> p Word8 -> Image (p Word8)
simpleGradientH (w,h) l r = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(_:.x) ->
                                round <$> lerp (percent x w)
                            )
    where
        percent :: Int -> Int -> Double
        percent val total = fromIntegral val / fromIntegral total
        l' = fromIntegral <$> l
        r' = fromIntegral <$> r
        lerp p = l' + ((r' - l') `multScalar` p)

-- | Generate an image made up of a 2-color linear vertical gradient.
--
-- Parameters:
-- - Size in terms `(width,height)`.
-- - Color at the top of the image.
-- - Color at the right of the image.
simpleGradientV :: Pixel p => (Int,Int) -> p Word8 -> p Word8 -> Image (p Word8)
simpleGradientV (w,h) l r = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(y:._) ->
                                round <$> lerp (percent y h)
                            )
    where
        percent :: Int -> Int -> Double
        percent val total = fromIntegral val / fromIntegral total
        l' = fromIntegral <$> l
        r' = fromIntegral <$> r
        lerp p = l' + ((r' - l') `multScalar` p)

-- | Generate an image made up of an n-color linear horizontal gradient.
--
-- Parameters:
-- - Size in terms `(width,height)`.
-- - Initial color, at the left of the image.
-- - Evenly-spaced colors to reach throughout the gradient.
multiColorGradientH :: Pixel p => (Int,Int) -> p Word8 -> [p Word8] -> Image (p Word8)
multiColorGradientH (w,h) c cs = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(_:.x) ->
                                    round <$> lerp (percent x w)
                                )
    where
        len = length cs
        percent :: Int -> Int -> Double
        percent val total = fromIntegral (len * val) / fromIntegral total
        cs' = fmap fromIntegral <$> (c:cs)
        lerp p = if (floor p :: Int) == (ceiling p :: Int)
                    then cs' !! floor p
                    else
                        let l = cs' !! floor p
                            r = cs' !! ceiling p
                            p' = p - fromIntegral (floor p :: Int)
                        in l + ((r - l) `multScalar` p')

-- | Generate an image made up of an n-color linear vertical gradient.
--
-- Parameters:
-- - Size in terms `(width,height)`.
-- - Initial color, at the top of the image.
-- - Evenly-spaced colors to reach throughout the gradient.
multiColorGradientV :: Pixel p => (Int,Int) -> p Word8 -> [p Word8] -> Image (p Word8)
multiColorGradientV (w,h) c cs = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(y:._) ->
                                    round <$> lerp (percent y h)
                                )
    where
        len = length cs
        percent :: Int -> Int -> Double
        percent val total = fromIntegral (len * val) / fromIntegral total
        cs' = fmap fromIntegral <$> (c:cs)
        lerp p = if (floor p :: Int) == (ceiling p :: Int)
                    then cs' !! floor p
                    else
                        let l = cs' !! floor p
                            r = cs' !! ceiling p
                            p' = p - fromIntegral (floor p :: Int)
                        in l + ((r - l) `multScalar` p')
