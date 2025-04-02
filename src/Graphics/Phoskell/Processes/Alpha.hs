-- | Processes involving the alpha channel.
module Graphics.Phoskell.Processes.Alpha (
    addAlphaChannel,
    dropAlphaChannel,
    alterAlpha,
    overlayImage,
    chromaKeyRemove,
    chromaKeyExtract,
) where

import Graphics.Phoskell.Core.Image
import Graphics.Phoskell.Core.Pixel
import Graphics.Phoskell.Core
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
            case img !? idx of
                Nothing -> Pixel4 r1 g1 b1 a1
                Just (Pixel4 r2 g2 b2 a2) ->
                    let c1 = toDouble01 <$> Pixel3 r1 g1 b1
                        c2 = toDouble01 <$> Pixel3 r2 g2 b2
                        a1' = toDouble01 a1
                        a2' = toDouble01 a2
                        a = a2' + a1'*(1 - a2')
                        c2' = c2 `multScalar` a2'
                        c1' = c1 `multScalar` (a1'*(1-a2'))
                        Pixel3 r g b = fmap (/a) (c2' + c1')
                    in fromDouble01 <$> Pixel4 r g b a
        )
    where
        toDouble01 :: Word8 -> Double
        toDouble01 x = fromIntegral x / 255
        fromDouble01 :: Double -> Word8
        fromDouble01 x = round $ clamp (0,255) (x * 255)

-- | Remove colours similar to colour given, setting their alpha to 0.
--
-- Maximum colour-distance to be classified as similar is determined by the
-- threshold value given.
chromaKeyRemove :: RGBA -> Double -> PointProcess RGBA RGBA
chromaKeyRemove c t = PointProcess (\p ->
            let Pixel4 pr pg pb pa = p
                -- compare hsv rather than rgb as it's closer to how colour
                -- similarity is perceived by humans
                Pixel3 ph ps pv = rgbToHSV (Pixel3 pr pg pb)
                p' = toDouble01 <$> Pixel4 ph ps pv pa
                diff = p' - c'
                Pixel1 d = sqrt ((diff*diff) `dot` 1)
            in if d < t then 0
                        else p
        )
    where
        Pixel4 cr cg cb ca = c
        Pixel3 ch cs cv = rgbToHSV (Pixel3 cr cg cb)
        c' = toDouble01 <$> Pixel4 ch cs cv ca
        toDouble01 :: Word8 -> Double
        toDouble01 x = fromIntegral x / 255

-- | Remove colours dissimilar to colour given, setting their alpha to 0.
--
-- Minimum colour-distance to be classified as dissimilar is determined by the
-- threshold value given.
--
-- Almost identical to @chromaKeyRemove@, except this function retains similar
-- colours rather than removing them.
chromaKeyExtract :: RGBA -> Double -> PointProcess RGBA RGBA
chromaKeyExtract c t = PointProcess (\p ->
            let Pixel4 pr pg pb pa = p
                -- compare hsv rather than rgb as it's closer to how colour
                -- similarity is perceived by humans
                Pixel3 ph ps pv = rgbToHSV (Pixel3 pr pg pb)
                p' = toDouble01 <$> Pixel4 ph ps pv pa
                diff = p' - c'
                Pixel1 d = sqrt ((diff*diff) `dot` 1)
            in if d < t then p
                        else 0
        )
    where
        Pixel4 cr cg cb ca = c
        Pixel3 ch cs cv = rgbToHSV (Pixel3 cr cg cb)
        c' = toDouble01 <$> Pixel4 ch cs cv ca
        toDouble01 :: Word8 -> Double
        toDouble01 x = fromIntegral x / 255
