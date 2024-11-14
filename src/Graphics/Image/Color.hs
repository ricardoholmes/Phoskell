{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.Color (
    dropAlpha,
    addAlpha,

    RGBA,
    toRGB,
    toHSI,
    toHSL,
    toHSV,
    toYCbCr,
    toCMYK,
) where

import Graphics.Color.Space (ColorSpace, convertColor)
import Graphics.Pixel (Pixel, liftPixel)
import Graphics.Color.Model hiding (dropAlpha, addAlpha)
import qualified Graphics.Color.Model as GCM

type RGBA = Alpha RGB

dropAlpha :: Pixel (Alpha cs') e' -> Pixel cs' e'
dropAlpha = liftPixel GCM.dropAlpha

addAlpha :: Pixel (Alpha cs') e' -> Pixel cs' e'
addAlpha = liftPixel GCM.dropAlpha

-- | Convert to RGB color space
toRGB :: (ColorSpace cs i e, ColorSpace RGB i e) => Pixel cs e -> Pixel RGB e
toRGB = liftPixel convertColor

-- | Convert to HSI color space
toHSI :: (ColorSpace cs i e, ColorSpace HSI i e) => Pixel cs e -> Pixel HSI e
toHSI = liftPixel convertColor

-- | Convert to HSL color space
toHSL :: (ColorSpace cs i e, ColorSpace HSL i e) => Pixel cs e -> Pixel HSL e
toHSL = liftPixel convertColor

-- | Convert to HSV color space
toHSV :: (ColorSpace cs i e, ColorSpace HSV i e) => Pixel cs e -> Pixel HSV e
toHSV = liftPixel convertColor

-- | Convert to YCbCr color space
toYCbCr :: (ColorSpace cs i e, ColorSpace YCbCr i e) => Pixel cs e -> Pixel YCbCr e
toYCbCr = liftPixel convertColor

-- | Convert to CMYK color space
toCMYK :: (ColorSpace cs i e, ColorSpace CMYK i e) => Pixel cs e -> Pixel CMYK e
toCMYK = liftPixel convertColor
