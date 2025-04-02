{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Phoskell.Core.Colour (
    -- int type used for colours
    Word8,

    -- colours
    Binary,
    Grey,
    RGB,
    RGBA,
    HSV,
    HSL,

    -- grey colour conversions
    greyToRGB,
    greyToRGBA,
    greyToHSV,
    greyToHSL,

    -- RGB colour conversions
    rgbToGrey,
    rgbToRGBA,
    rgbToHSV,
    rgbToHSL,

    -- RGBA colour conversions
    rgbaToGrey,
    rgbaToRGB,
    rgbaToHSV,
    rgbaToHSL,

    -- HSV colour conversions
    hsvToGrey,
    hsvToRGB,
    hsvToRGBA,
    hsvToHSL,

    -- HSL colour conversions
    hslToGrey,
    hslToRGB,
    hslToRGBA,
    hslToHSV,

    -- channel extraction
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

    -- RGB colour constants
    red,
    green,
    blue,
    white,
    black,
    yellow,
    cyan,
    magenta,

    -- RGBA colour constants
    redA,
    greenA,
    blueA,
    whiteA,
    blackA,
    yellowA,
    cyanA,
    magentaA,
) where

import Graphics.Phoskell.Core.Pixel
import Data.Fixed (mod')
import GHC.Word (Word8)
import Data.Ord (clamp)

--- type aliases ---

type Binary = Pixel1 Bool
type Grey = Pixel1 Word8
type RGB = Pixel3 Word8
type RGBA = Pixel4 Word8
type HSV = Pixel3 Word8
type HSL = Pixel3 Word8

--- colour space conversion ---

-- from grey
greyToRGB :: Grey -> RGB
greyToRGB (Pixel1 x) = Pixel3 x x x
{-# INLINE greyToRGB #-}

greyToRGBA :: Grey -> RGBA
greyToRGBA (Pixel1 x) = Pixel4 x x x maxBound
-- maxBound makes the possibility of extending to another bit depth easier
{-# INLINE greyToRGBA #-}

greyToHSV :: Grey -> HSV
greyToHSV = rgbToHSV . greyToRGB
{-# INLINE greyToHSV #-}

greyToHSL :: Grey -> HSL
greyToHSL = rgbToHSL . greyToRGB
{-# INLINE greyToHSL #-}

-- from rgb
rgbToGrey :: RGB -> Grey
rgbToGrey = fmap round . dot coeffs . fmap fromIntegral
    where
        coeffs :: Pixel3 Double
        coeffs = Pixel3 0.299 0.587 0.114
        {-# INLINE coeffs #-}

rgbToRGBA :: RGB -> RGBA
rgbToRGBA (Pixel3 r g b) = Pixel4 r g b 255

rgbToHSV :: RGB -> HSV
rgbToHSV rgb = round . clamp (0,255) . (*255) <$> Pixel3 h s v
    where
        ((Pixel3 r g b) :: Pixel3 Double) = fmap ((/255) . fromIntegral) rgb
        xMax = max r (max g b)
        xMin = min r (min g b)
        c = xMax - xMin
        h' | c    == 0 = 0
           | xMax == r = ((g - b)/c) `mod'` 6
           | xMax == g = ((b - r)/c) + 2
           | otherwise = ((r - g)/c) + 4 -- xMax == b
        h = h' / 6 -- to get it into interval [0,1]
        v = xMax
        s = if v == 0 then 0 else c/v

rgbToHSL :: RGB -> HSL
rgbToHSL rgb = round . clamp (0,255) . (*255) <$> Pixel3 h s l
    where
        ((Pixel3 r g b) :: Pixel3 Double) = fmap ((/255) . fromIntegral) rgb
        xMax = max r (max g b)
        xMin = min r (min g b)
        c = xMax - xMin
        h' | c    == 0 = 0
           | xMax == r = ((g - b)/c) `mod'` 6
           | xMax == g = ((b - r)/c) + 2
           | otherwise = ((r - g)/c) + 4 -- xMax == b
        h = h' / 6 -- to get it into interval [0,1]
        v = xMax
        l = v - c / 2
        s = if l == 0 || l == 1 then 0 else c/(1 - abs (2*v - c - 1))

-- from rgba
rgbaToGrey :: RGBA -> Grey
rgbaToGrey = rgbToGrey . rgbaToRGB

rgbaToRGB :: RGBA -> RGB
rgbaToRGB (Pixel4 r g b a) = fmap round (rgb `multScalar` a')
    where
        rgb = fromIntegral <$> Pixel3 r g b
        a' = fromIntegral a / 255 :: Double

rgbaToHSV :: RGBA -> HSV
rgbaToHSV = rgbToHSV . rgbaToRGB

rgbaToHSL :: RGBA -> HSL
rgbaToHSL = rgbToHSL . rgbaToRGB

-- from hsv
hsvToGrey :: HSV -> Grey
hsvToGrey = rgbToGrey . hsvToRGB

hsvToRGB :: HSV -> RGB
hsvToRGB hsv = round . clamp (0,255) . (*255) <$> Pixel3 (f 5) (f 3) (f 1)
    where
        (Pixel3 h s v :: Pixel3 Double) = (/255) . fromIntegral <$> hsv
        f n = let k = (n + h*6) `mod'` 6 -- h*360/60 = h*6
              in v - v * s * max 0 (min k (min (4-k) 1))

hsvToRGBA :: HSV -> RGBA
hsvToRGBA = rgbToRGBA . hsvToRGB

hsvToHSL :: HSV -> HSL
hsvToHSL hsv = round . clamp (0,255) . (*255) <$> Pixel3 h sl l
    where
        (Pixel3 h sv v :: Pixel3 Double) = (/255) . fromIntegral <$> hsv
        l = v * (1 - (sv / 2))
        sl = if l == 0 || l == 1 then 0 else (v - l) / min l (1-l)

-- from hsv
hslToGrey :: HSL -> Grey
hslToGrey = rgbToGrey . hslToRGB

hslToRGB :: HSL -> RGB
hslToRGB hsl = round . clamp (0,255) . (*255) <$> Pixel3 (f 0) (f 8) (f 4)
    where
        (Pixel3 h s l :: Pixel3 Double) = (/255) . fromIntegral <$> hsl
        f n = let k = (n + h*12) `mod'` 12 -- h*360/30 = h*12
              in l - a * max (-1) (min (k-3) (min (9-k) 1))
        a = s * min l (1 - l)

hslToRGBA :: HSL -> RGBA
hslToRGBA = rgbToRGBA . hslToRGB

hslToHSV :: HSL -> HSV
hslToHSV hsl = round . clamp (0,255) . (*255) <$> Pixel3 h sv v
    where
        (Pixel3 h sl l :: Pixel3 Double) = (/255) . fromIntegral <$> hsl
        v = l + sl * min l (1 - l)
        sv = if v == 0 then 0 else 2 * (1 - l/v)

--- extracting channels ---

-- rgb
takeRedRGB :: RGB -> Grey
takeRedRGB (Pixel3 r _ _) = Pixel1 r
{-# INLINE takeRedRGB #-}

takeGreenRGB :: RGB -> Grey
takeGreenRGB (Pixel3 _ g _) = Pixel1 g
{-# INLINE takeGreenRGB #-}

takeBlueRGB :: RGB -> Grey
takeBlueRGB (Pixel3 _ _ b) = Pixel1 b
{-# INLINE takeBlueRGB #-}

-- rgba
takeRedRGBA :: RGBA -> Grey
takeRedRGBA (Pixel4 r _ _ _) = Pixel1 r
{-# INLINE takeRedRGBA #-}

takeGreenRGBA :: RGBA -> Grey
takeGreenRGBA (Pixel4 _ g _ _) = Pixel1 g
{-# INLINE takeGreenRGBA #-}

takeBlueRGBA :: RGBA -> Grey
takeBlueRGBA (Pixel4 _ _ b _) = Pixel1 b
{-# INLINE takeBlueRGBA #-}

takeAlphaRGBA :: RGBA -> Grey
takeAlphaRGBA (Pixel4 _ _ _ a) = Pixel1 a
{-# INLINE takeAlphaRGBA #-}

-- hsv
takeHueHSV :: HSV -> Grey
takeHueHSV (Pixel3 h _ _) = Pixel1 h
{-# INLINE takeHueHSV #-}

takeSaturationHSV :: HSV -> Grey
takeSaturationHSV (Pixel3 _ s _) = Pixel1 s
{-# INLINE takeSaturationHSV #-}

takeValueHSV :: HSV -> Grey
takeValueHSV (Pixel3 _ _ v) = Pixel1 v
{-# INLINE takeValueHSV #-}

-- hsl
takeHueHSL :: HSL -> Grey
takeHueHSL (Pixel3 h _ _) = Pixel1 h
{-# INLINE takeHueHSL #-}

takeSaturationHSL :: HSL -> Grey
takeSaturationHSL (Pixel3 _ s _) = Pixel1 s
{-# INLINE takeSaturationHSL #-}

takeLightnessHSL :: HSL -> Grey
takeLightnessHSL (Pixel3 _ _ l) = Pixel1 l
{-# INLINE takeLightnessHSL #-}

--- constant colours ---

-- RGB

red :: RGB
red = Pixel3 255 0 0
{-# INLINE red #-}

green :: RGB
green = Pixel3 0 255 0
{-# INLINE green #-}

blue :: RGB
blue = Pixel3 0 0 255
{-# INLINE blue #-}

white :: RGB
white = Pixel3 255 255 255
{-# INLINE white #-}

black :: RGB
black = Pixel3 0 0 0
{-# INLINE black #-}

yellow :: RGB
yellow = Pixel3 255 255 0
{-# INLINE yellow #-}

cyan :: RGB
cyan = Pixel3 0 255 255
{-# INLINE cyan #-}

magenta :: RGB
magenta = Pixel3 255 0 255
{-# INLINE magenta #-}

-- RGBA

-- | Red with an alpha channel
redA :: RGBA
redA = Pixel4 255 0 0 255
{-# INLINE redA #-}

-- | Green with an alpha channel
greenA :: RGBA
greenA = Pixel4 0 255 0 255
{-# INLINE greenA #-}

-- | Blue with an alpha channel
blueA :: RGBA
blueA = Pixel4 0 0 255 255
{-# INLINE blueA #-}

whiteA :: RGBA
whiteA = Pixel4 255 255 255 255
{-# INLINE whiteA #-}

blackA :: RGBA
blackA = Pixel4 0 0 0 255
{-# INLINE blackA #-}

yellowA :: RGBA
yellowA = Pixel4 255 255 0 255
{-# INLINE yellowA #-}

cyanA :: RGBA
cyanA = Pixel4 0 255 255 255
{-# INLINE cyanA #-}

magentaA :: RGBA
magentaA = Pixel4 255 0 255 255
{-# INLINE magentaA #-}
