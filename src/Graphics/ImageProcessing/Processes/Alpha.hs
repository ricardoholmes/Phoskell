-- | Processes involving the alpha channel.
module Graphics.ImageProcessing.Processes.Alpha (
    addAlphaChannel,
    dropAlphaChannel,
    alterAlpha,
    overlayImage,
) where

import Graphics.ImageProcessing.Core.Image (PointProcess(PointProcess), IPointProcess (IPointProcess))
import Graphics.ImageProcessing.Core
import Data.Word
import Data.Ord (clamp)

-- | Add an alpha channel, setting all values to their maximum.
addAlphaChannel :: PointProcess RGB RGBA
addAlphaChannel = PointProcess (\(Pixel3 r g b) -> Pixel4 r g b 255)

-- | Remove the alpha channel, ignoring all of its values.
dropAlphaChannel :: PointProcess RGBA RGB
dropAlphaChannel = PointProcess (\(Pixel4 r g b _) -> Pixel3 r g b)

-- | Apply a function to all values in the alpha channel.
alterAlpha :: (Word8 -> Word8) -> PointProcess RGBA RGBA
alterAlpha f = PointProcess (\(Pixel4 r g b a) -> Pixel4 r g b (f a))

-- | Overlay the image given onto the image that the process is being applied to.
--
-- https://en.wikipedia.org/wiki/Alpha_compositing
overlayImage :: Image RGBA -> IPointProcess RGBA RGBA
overlayImage img = IPointProcess (\idx (Pixel4 r1 g1 b1 a1) ->
            let (Pixel4 r2 g2 b2 a2) = img ! idx
                c1 = toDouble01 <$> Pixel3 r1 g1 b1
                c2 = toDouble01 <$> Pixel3 r2 g2 b2
                a1' = toDouble01 a1
                a2' = toDouble01 a2
                a = a2' + a1'*(1 - a2')
                c2' = c2 `multScalar` a2'
                c1' = c1 `multScalar` (a1'*(1-a2'))
                Pixel3 r g b = fmap (/a) (c2' + c1')
            in round . clamp (0,255) . (*255) <$> Pixel4 r g b a
        )
    where
        toDouble01 :: Word8 -> Double
        toDouble01 x = fromIntegral x / 255
