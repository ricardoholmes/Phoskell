-- | Processes involving the alpha channel.
module Graphics.ImageProcessing.Processes.Alpha (
    addAlphaChannel,
    dropAlphaChannel,
    alterAlpha,
) where

import Graphics.ImageProcessing.Core.Image (PointProcess(PointProcess))
import Graphics.ImageProcessing.Core
import Data.Word

-- | Add an alpha channel, setting all values to their maximum.
addAlphaChannel :: PointProcess RGB RGBA
addAlphaChannel = PointProcess (\(Pixel3 r g b) -> Pixel4 r g b 255)

-- | Remove the alpha channel, ignoring all of its values.
dropAlphaChannel :: PointProcess RGBA RGB
dropAlphaChannel = PointProcess (\(Pixel4 r g b _) -> Pixel3 r g b)

-- | Apply a function to all values in the alpha channel.
alterAlpha :: (Word8 -> Word8) -> PointProcess RGBA RGBA
alterAlpha f = PointProcess (\(Pixel4 r g b a) -> Pixel4 r g b (f a))
