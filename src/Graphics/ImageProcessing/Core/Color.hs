{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.ImageProcessing.Core.Color (
    Word8,

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
import Data.Ord (clamp)

--- type aliases ---

type Binary = Pixel1 Bool
type Gray = Pixel1 Word8
type RGB = Pixel3 Word8
type RGBA = Pixel4 Word8
type HSV = Pixel3 Word8
type HSL = Pixel3 Word8

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
rgbToHSV rgb = floor . clamp (0,255) . (*255) <$> Pixel3 h s v
    where
        ((Pixel3 r g b) :: Pixel3 Double) = fmap ((/255) . fromIntegral) rgb
        xMax = max r (max g b)
        xMin = min r (min g b)
        c = xMax - xMin
        h' | c    == 0 = 0
           | xMax == r =  (g - b)/c
           | xMax == g = ((b - r)/c) + 2
           | otherwise = ((r - g)/c) + 4 -- xMax == b
        h = h' / 6 -- to get it into interval [0,1]
        v = xMax
        s = if v == 0 then 0 else c/v

rgbToHSL :: RGB -> HSL
rgbToHSL rgb = floor . clamp (0,255) . (*255) <$> Pixel3 h s l
    where
        ((Pixel3 r g b) :: Pixel3 Double) = fmap ((/255) . fromIntegral) rgb
        xMax = max r (max g b)
        xMin = min r (min g b)
        c = xMax - xMin
        h' | c    == 0 = 0
           | xMax == r =  (g - b)/c
           | xMax == g = ((b - r)/c) + 2
           | otherwise = ((r - g)/c) + 4 -- xMax == b
        h = h' / 6 -- to get it into interval [0,1]
        v = xMax
        l = v - c / 2
        s = if l == 0 || l == 1 then 0 else c/(1 - abs (2*v - c - 1))

-- from rgba
rgbaToGray :: RGBA -> Gray
rgbaToGray = rgbToGray . rgbaToRGB

rgbaToRGB :: RGBA -> RGB
rgbaToRGB (Pixel4 r g b a) = Pixel3 r g b `multScalar` a

rgbaToHSV :: RGBA -> HSV
rgbaToHSV = rgbToHSV . rgbaToRGB

rgbaToHSL :: RGBA -> HSL
rgbaToHSL = rgbToHSL . rgbaToRGB

-- from hsv
hsvToGray :: HSV -> Gray
hsvToGray = rgbToGray . hsvToRGB

hsvToRGB :: HSV -> RGB
hsvToRGB hsv = floor . (*255) <$> Pixel3 (f 5) (f 3) (f 1)
    where
        (Pixel3 h s v :: Pixel3 Double) = (/255) . fromIntegral <$> hsv
        f n = let k = (n + h*6) `mod'` 6 -- h*360/60 = h*6
              in v - v * s * max 0 (min k (min (4-k) 1))

hsvToRGBA :: HSV -> RGBA
hsvToRGBA = rgbToRGBA . hsvToRGB

hsvToHSL :: HSV -> HSL
hsvToHSL hsv = floor . clamp (0,255) . (*255) <$> Pixel3 h sl l
    where
        (Pixel3 h sv v :: Pixel3 Double) = (/255) . fromIntegral <$> hsv
        l = v * (1 - (sv / 2))
        sl = if l == 0 || l == 1 then 0 else (v - l) / min l (1-l)

-- from hsv
hslToGray :: HSL -> Gray
hslToGray = rgbToGray . hslToRGB

hslToRGB :: HSL -> RGB
hslToRGB hsl = floor . (*255) <$> Pixel3 (f 0) (f 8) (f 4)
    where
        (Pixel3 h s l :: Pixel3 Double) = (/255) . fromIntegral <$> hsl
        f n = let k = (n + h*12) `mod'` 12 -- h*360/30 = h*12
              in l - a * max (-1) (min (k-3) (min (9-k) 1))
        a = s * min l (1 - l)

hslToRGBA :: HSL -> RGBA
hslToRGBA = rgbToRGBA . hslToRGB

hslToHSV :: HSL -> HSV
hslToHSV hsl = floor . (*255) <$> Pixel3 h sv v
    where
        (Pixel3 h sl l :: Pixel3 Double) = (/255) . fromIntegral <$> hsl
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
takeLightnessHSL (Pixel3 _ _ l) = Pixel1 l

--- constant colours ---

red :: RGB
red = Pixel3 255 0 0

green :: RGB
green = Pixel3 0 255 0

blue :: RGB
blue = Pixel3 0 0 255

-- | Red with an alpha channel
redA :: RGBA
redA = Pixel4 255 0 0 255

-- | Green with an alpha channel
greenA :: RGBA
greenA = Pixel4 0 255 0 255

-- | Blue with an alpha channel
blueA :: RGBA
blueA = Pixel4 0 0 255 255
