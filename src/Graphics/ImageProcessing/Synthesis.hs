module Graphics.ImageProcessing.Synthesis (
    canvas,
    generateImage,
    generateImage',
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
-- Note: values returned by the function should be in the range [0..255].
generateImage :: (Pixel p, Integral a) => (Int,Int) -> (Int,Int) -> ((Int,Int) -> p a) -> Image (p Word8)
generateImage (xm,ym) (xM,yM) f = BaseImage $ M.makeArrayR M.D M.Par sz (\(y:.x) ->
                                    let y' = fromIntegral (y - ym)
                                        x' = fromIntegral (x - xm)
                                    in fromIntegral <$> f (x',y')
                                )
    where
        sz = M.Sz2 (yM - ym + 1) (xM - xm + 1)

-- | Generate an image from a function.
--
-- Equivalent to @generateImage@, except the functions returns non-integer pixel values
-- between 0 and 1.
generateImage' :: (Pixel p, RealFloat a) => (Int,Int) -> (Int,Int) -> ((Int,Int) -> p a) -> Image (p Word8)
generateImage' (xm,ym) (xM,yM) f = BaseImage $ M.makeArrayR M.D M.Par sz (\(y:.x) ->
                                    let y' = fromIntegral (y - ym)
                                        x' = fromIntegral (x - xm)
                                    in round . (*255) <$> f (x',y')
                                )
    where
        sz = M.Sz2 (yM - ym + 1) (xM - xm + 1)
