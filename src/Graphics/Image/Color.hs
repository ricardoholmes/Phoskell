{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Image.Color (
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
) where

import Graphics.Image.Pixel
import Data.Fixed (mod')

--- type aliases ---

type Binary = Pixel1 Bool
type Gray = Pixel1 Double
type RGB = Pixel3 Double
type RGBA = Pixel4 Double
type HSV = Pixel3 Double
type HSL = Pixel3 Double

--- color space conversion ---

-- from gray
grayToRGB :: Gray -> RGB
grayToRGB (Pixel1 x) = Pixel3 x x x

grayToRGBA :: Gray -> RGBA
grayToRGBA (Pixel1 x) = Pixel4 x x x 1

grayToHSV :: Gray -> HSV
grayToHSV = rgbToHSV . grayToRGB

grayToHSL :: Gray -> HSL
grayToHSL = rgbToHSL . grayToRGB

-- from rgb
rgbToGray :: RGB -> Gray
rgbToGray = dot (Pixel3 0.299 0.587 0.114)

rgbToRGBA :: RGB -> RGBA
rgbToRGBA (Pixel3 r g b) = Pixel4 r g b 1

rgbToHSV :: RGB -> HSV
rgbToHSV (Pixel3 r g b) = Pixel3 h s v
    where
        ma = max r (max g b)
        mi = min r (min g b)
        c = ma - mi
        h' | c == 0    = 0
            | ma == r   = ((g - b)/c) `mod'` 6
            | ma == g   = ((b - r)/c) + 2
            | otherwise = ((r - g)/c) + 4 -- ma == b
        h = 60 * h'
        v = ma
        s = if v == 0 then 0 else c/v

rgbToHSL :: RGB -> HSL
rgbToHSL (Pixel3 r g b) = Pixel3 h s l
    where
        ma = max r (max g b)
        mi = min r (min g b)
        c = ma - mi
        h' | c == 0    = 0
            | ma == r   = ((g - b)/c) `mod'` 6
            | ma == g   = ((b - r)/c) + 2
            | otherwise = ((r - g)/c) + 4 -- ma == b
        h = 60 * h'
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
hsvToRGB (Pixel3 h s v) = Pixel3 (f 5) (f 3) (f 1)
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
hslToRGB (Pixel3 h s l) = Pixel3 (f 0) (f 8) (f 4)
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
takeHueHSV (Pixel3 h _ _) = Pixel1 h

takeSaturationHSV :: HSV -> Gray
takeSaturationHSV (Pixel3 _ s _) = Pixel1 s

takeValueHSV :: HSV -> Gray
takeValueHSV (Pixel3 _ _ v) = Pixel1 v

-- hsl
takeHueHSL :: HSL -> Gray
takeHueHSL (Pixel3 h _ _) = Pixel1 h

takeSaturationHSL :: HSL -> Gray
takeSaturationHSL (Pixel3 _ s _) = Pixel1 s

takeLightnessHSL :: HSL -> Gray
takeLightnessHSL (Pixel3 _ _ v) = Pixel1 v
