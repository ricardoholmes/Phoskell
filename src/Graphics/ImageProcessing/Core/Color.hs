{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.ImageProcessing.Core.Color (
    Binary,
    Gray,
    RGB,
    RGBA,
    HSV,
    HSL,

    grayToRGB,
    grayToRGBA,
    grayToHSV,
    grayToHSL,

    rgbToGray,
    rgbToRGBA,
    rgbToHSV,
    rgbToHSL,

    rgbaToGray,
    rgbaToRGB,
    rgbaToHSV,
    rgbaToHSL,

    hsvToGray,
    hsvToRGB,
    hsvToRGBA,
    hsvToHSL,

    hslToGray,
    hslToRGB,
    hslToRGBA,
    hslToHSV,

    takeRedRGB,
    takeGreenRGB,
    takeBlueRGB,
    takeRedRGBA,
    takeGreenRGBA,
    takeBlueRGBA,
    takeAlphaRGBA,
    takeHueHSV,
    takeSaturationHSV,
    takeValueHSV,
    takeHueHSL,
    takeSaturationHSL,
    takeLightnessHSL,

    red,
    green,
    blue,
    redA,
    greenA,
    blueA,
) where

import Graphics.ImageProcessing.Core.Pixel
import Data.Fixed (mod')
import GHC.Word (Word8)

--- type aliases ---

type Binary = Pixel1 Bool
type Gray = Pixel1 Word8
type RGB = Pixel3 Word8
type RGBA = Pixel4 Word8
type HSV = Pixel3 Double
type HSL = Pixel3 Double

--- color space conversion ---

-- from gray
grayToRGB :: Gray -> RGB
grayToRGB (Pixel1 x) = Pixel3 x x x

grayToRGBA :: Gray -> RGBA
grayToRGBA (Pixel1 x) = Pixel4 x x x maxBound
-- maxBound makes the possibility of extending to another bit depth easier

grayToHSV :: Gray -> HSV
grayToHSV = rgbToHSV . grayToRGB

grayToHSL :: Gray -> HSL
grayToHSL = rgbToHSL . grayToRGB

-- from rgb
rgbToGray :: RGB -> Gray
rgbToGray = fmap floor . dot coeffs . fmap fromIntegral
    where
        coeffs :: Pixel3 Double
        coeffs = Pixel3 0.299 0.587 0.114

rgbToRGBA :: RGB -> RGBA
rgbToRGBA (Pixel3 r g b) = Pixel4 r g b 1

rgbToHSV :: RGB -> HSV
rgbToHSV rgb = Pixel3 h s v
    where
        ((Pixel3 r g b) :: Pixel3 Double) = fmap ((/255) . fromIntegral) rgb
        ma = max r (max g b)
        mi = min r (min g b)
        c = ma - mi
        h' | c == 0    = 0
           | ma == r   =  (g - b)/c
           | ma == g   = ((b - r)/c) + 2
           | otherwise = ((r - g)/c) + 4 -- ma == b
        h = h' / 6 -- to get it into interval [0,1]
        v = ma
        s = if v == 0 then 0 else c/v

rgbToHSL :: RGB -> HSL
rgbToHSL rgb = Pixel3 h s l
    where
        ((Pixel3 r g b) :: Pixel3 Double) = fmap ((/255) . fromIntegral) rgb
        ma = max r (max g b)
        mi = min r (min g b)
        c = ma - mi
        h' | c  == 0   = 0
           | ma == r   =  (g - b)/c
           | ma == g   = ((b - r)/c) + 2
           | ma == b   = ((r - g)/c) + 4
           | otherwise = 0
        h = h' / 6 -- to get it into interval [0,1]
        l = (ma - mi) / 2
        s = if l == 0 then 0 else c/(1 - abs (2*l - 1))

-- from rgba
rgbaToGray :: RGBA -> Gray
rgbaToGray = rgbToGray . rgbaToRGB

rgbaToRGB :: RGBA -> RGB
rgbaToRGB (Pixel4 r g b a) = multScalar a (Pixel3 r g b)

rgbaToHSV :: RGBA -> HSV
rgbaToHSV = rgbToHSV . rgbaToRGB

rgbaToHSL :: RGBA -> HSL
rgbaToHSL = rgbToHSL . rgbaToRGB

-- from hsv
hsvToGray :: HSV -> Gray
hsvToGray = rgbToGray . hsvToRGB

hsvToRGB :: HSV -> RGB
hsvToRGB (Pixel3 h s v) = floor . (*255) <$> Pixel3 (f 5) (f 3) (f 1)
    where f n = let k = (n + h/60) `mod'` 6
                in v - v * s * max 0 (min k (min (4-k) 1))

hsvToRGBA :: HSV -> RGBA
hsvToRGBA = rgbToRGBA . hsvToRGB

hsvToHSL :: HSV -> HSL
hsvToHSL (Pixel3 h sv v) = Pixel3 h sl l
    where
        l = v * (1 - (sv / 2))
        sl = if l == 0 || l == 1 then 0 else (v - l) / min l (1-l)

-- from hsv
hslToGray :: HSL -> Gray
hslToGray = rgbToGray . hslToRGB

hslToRGB :: HSL -> RGB
hslToRGB (Pixel3 h s l) = floor . (*255) <$> Pixel3 (f 0) (f 8) (f 4)
    where
        f n = let k = (n + h/30) `mod'` 12
              in l - a * max (-1) (min (k-3) (min (9-k) 1))
        a = s * min l (1 - l)

hslToRGBA :: HSL -> RGBA
hslToRGBA = rgbToRGBA . hslToRGB

hslToHSV :: HSL -> HSV
hslToHSV (Pixel3 h sl l) = Pixel3 h sv v
    where
        v = l + sl * min 1 (1 - l)
        sv = if v == 0 then 0 else 2 * (1 - l/v)

--- extracting channels ---

-- rgb
takeRedRGB :: RGB -> Gray
takeRedRGB (Pixel3 r _ _) = Pixel1 r

takeGreenRGB :: RGB -> Gray
takeGreenRGB (Pixel3 _ g _) = Pixel1 g

takeBlueRGB :: RGB -> Gray
takeBlueRGB (Pixel3 _ _ b) = Pixel1 b

-- rgba
takeRedRGBA :: RGBA -> Gray
takeRedRGBA (Pixel4 r _ _ _) = Pixel1 r

takeGreenRGBA :: RGBA -> Gray
takeGreenRGBA (Pixel4 _ g _ _) = Pixel1 g

takeBlueRGBA :: RGBA -> Gray
takeBlueRGBA (Pixel4 _ _ b _) = Pixel1 b

takeAlphaRGBA :: RGBA -> Gray
takeAlphaRGBA (Pixel4 _ _ _ a) = Pixel1 a

-- hsv
takeHueHSV :: HSV -> Gray
takeHueHSV (Pixel3 h _ _) = Pixel1 (floor (255 * h))

takeSaturationHSV :: HSV -> Gray
takeSaturationHSV (Pixel3 _ s _) = Pixel1 (floor (255 * s))

takeValueHSV :: HSV -> Gray
takeValueHSV (Pixel3 _ _ v) = Pixel1 (floor (255 * v))

-- hsl
takeHueHSL :: HSL -> Gray
takeHueHSL (Pixel3 h _ _) = Pixel1 (floor (255 * h))

takeSaturationHSL :: HSL -> Gray
takeSaturationHSL (Pixel3 _ s _) = Pixel1 (floor (255 * s))

takeLightnessHSL :: HSL -> Gray
takeLightnessHSL (Pixel3 _ _ l) = Pixel1 (floor (255 * l))

--- constant colours ---

red :: RGB
red = Pixel3 1 0 0

green :: RGB
green = Pixel3 0 1 0

blue :: RGB
blue = Pixel3 0 0 1

-- | Red with an alpha channel
redA :: RGBA
redA = Pixel4 1 0 0 1

-- | Green with an alpha channel
greenA :: RGBA
greenA = Pixel4 0 1 0 1

-- | Blue with an alpha channel
blueA :: RGBA
blueA = Pixel4 0 0 1 1
