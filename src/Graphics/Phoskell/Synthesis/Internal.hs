-- | implements general functions to avoid cyclic dependencies
module Graphics.Phoskell.Synthesis.Internal (
    -- general functions
    canvas,
    generateImage,
    generateImage',
) where

import Data.Massiv.Array (Ix2((:.)))

import qualified Data.Massiv.Array as M

import Graphics.Phoskell.Core

-- | Generate a blank image filled with a single value.
--
-- - First parameter is the size in terms @(width,height)@.
-- - Second parameter is the value to use to fill.
canvas :: (Int,Int) -> a -> Image a
canvas (w,h) x = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (const x)

-- | Generate an image from a function.
--
-- Values not in the range [0..255] returned by the function will be clamped to be in the range.
generateImage :: (Pixel p, Integral a, Integral b)
              => (Int,Int) -- ^ Top-left coordinates, i.e. @(min x, min y)@.
              -> (Int,Int) -- ^ Bottom-right coordinates, i.e. @(max x, max y)@.
              -> ((a,a) -> p b) -- ^ Function to use, taking @(x,y)@ and returning the pixel value.
              -> Image (p Word8)
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
-- Equivalent to 'generateImage', except the functions returns non-integer pixel values
-- between 0 and 1 (values outside of the range will be clamped).
generateImage' :: (Pixel p, RealFloat a)
               => (Int,Int) -- ^ Top-left coordinates, i.e. @(min x, min y)@.
               -> (Int,Int) -- ^ Bottom-right coordinates, i.e. @(max x, max y)@.
               -> ((a,a) -> p a) -- ^ Function to use, taking @(x,y)@ and returning the pixel value.
               -> Image (p Word8)
generateImage' (xm,ym) (xM,yM) f = BaseImage $ M.makeArrayR M.D M.Par sz (\(y:.x) ->
                                    let y' = fromIntegral (y + ym)
                                        x' = fromIntegral (x + xm)
                                    in round . (*255) . clamp01 <$> f (x',y')
                                )
    where
        sz = M.Sz2 (yM - ym + 1) (xM - xm + 1)
        clamp01 v = min (max 0 v) 1
