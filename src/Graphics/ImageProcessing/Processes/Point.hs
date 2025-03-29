module Graphics.ImageProcessing.Processes.Point (
    addBias,
    subtractBias,
    applyGain,
    gammaCorrect,
    alterRed,
    alterGreen,
    alterBlue,
    alterHue,
    alterSaturation,
    alterLightness,
    alterValue,
    invertColours,
    invertColoursNotAlpha,
) where

import Data.Ord (clamp)
import Data.Word ( Word8 )
import Graphics.ImageProcessing.Core.Image ( PointProcess(..) )
import Graphics.ImageProcessing.Core.Pixel ( Pixel, Pixel3 (..), Pixel4 (..) )
import Graphics.ImageProcessing.Core.Colour (RGB, RGBA, hsvToRGB, rgbToHSV, rgbToHSL, hslToRGB)

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

-- | Applies the given function to the HSL/HSV hue of each RGB pixel
alterHue :: (Word8 -> Word8) -> PointProcess RGB RGB
alterHue f = PointProcess (hslToRGB . alterHue' f . rgbToHSL)
    where
        alterHue' g (Pixel3 h s l) = Pixel3 (g h) s l

-- | Applies the given function to the HSL saturation of each RGB pixel
alterSaturation :: (Word8 -> Word8) -> PointProcess RGB RGB
alterSaturation f = PointProcess (hslToRGB . alterSaturation' f . rgbToHSL)
    where
        alterSaturation' g (Pixel3 h s l) = Pixel3 h (g s) l

-- | Applies the given function to the HSL lightness of each RGB pixel
alterLightness :: (Word8 -> Word8) -> PointProcess RGB RGB
alterLightness f = PointProcess (hslToRGB . alterLightness' f . rgbToHSL)
    where
        alterLightness' g (Pixel3 h s l) = Pixel3 h s (g l)

-- | Applies the given function to the HSV value of each RGB pixel
alterValue :: (Word8 -> Word8) -> PointProcess RGB RGB
alterValue f = PointProcess (hsvToRGB . alterValue' f . rgbToHSV)
    where
        alterValue' g (Pixel3 h s v) = Pixel3 h s (g v)

-- | Inverts all colour channel
invertColours :: (Pixel p) => PointProcess (p Word8) (p Word8)
invertColours = PointProcess (255-)

-- | Inverts all colours, ignoring the alpha channel
invertColoursNotAlpha :: PointProcess RGBA RGBA
invertColoursNotAlpha = PointProcess (\(Pixel4 r g b a) -> Pixel4 (255-r) (255-g) (255-b) a)
