module Spec.Colour (
    prop_grey_rgb,
    prop_grey_hsv,
    prop_grey_hsl,
    prop_grey_rgba,

    prop_rgb_hsv,
    prop_rgb_hsl,
    prop_rgb_rgba,

    prop_hsv_rgb,
    prop_hsv_hsl,
    prop_hsv_rgba,

    prop_hsl_rgb,
    prop_hsl_hsv,
    prop_hsl_rgba,
) where

import Graphics.Phoskell.Core.Colour
import Graphics.Phoskell.Core.Pixel

-- | rough pixel comparison
(~=) :: (Pixel p, Integral n) => p n -> p n -> Bool
p1 ~= p2 = sum (fmap minDiff ps) <= 5
    where
        ps = (,) <$> p1 <*> p2
        minDiff (x,y) = min (x - y) (y - x)

-- Grey == Grey -> x -> Grey --

prop_grey_rgb :: Grey -> Bool
prop_grey_rgb grey = grey ~= rgbToGrey (greyToRGB grey)

prop_grey_hsv :: Grey -> Bool
prop_grey_hsv grey = grey ~= hsvToGrey (greyToHSV grey)

prop_grey_hsl :: Grey -> Bool
prop_grey_hsl grey = grey ~= hslToGrey (greyToHSL grey)

prop_grey_rgba :: Grey -> Bool
prop_grey_rgba grey = grey ~= rgbaToGrey (greyToRGBA grey)

-- RGB == RGB -> x -> RGB --

prop_rgb_hsv :: RGB -> Bool
prop_rgb_hsv rgb = rgb ~= hsvToRGB (rgbToHSV rgb)

prop_rgb_hsl :: RGB -> Bool
prop_rgb_hsl rgb = rgb ~= hslToRGB (rgbToHSL rgb)

prop_rgb_rgba :: RGB -> Bool
prop_rgb_rgba rgb = rgb ~= rgbaToRGB (rgbToRGBA rgb)

-- HSV -> RGB == HSV -> x -> HSV -> RGB --
-- note: conversion to RGB is required due to potential ambiguity between colours

prop_hsv_rgb :: HSV -> Bool
prop_hsv_rgb hsv = hsvToRGB hsv ~= hsvToRGB hsv'
    where hsv' = rgbToHSV (hsvToRGB hsv)

prop_hsv_hsl :: HSV -> Bool
prop_hsv_hsl hsv = hsvToRGB hsv ~= hsvToRGB hsv'
    where hsv' = hslToHSV (hsvToHSL hsv)

prop_hsv_rgba :: HSV -> Bool
prop_hsv_rgba hsv = hsvToRGB hsv ~= hsvToRGB hsv'
    where hsv' = rgbaToHSV (hsvToRGBA hsv)

-- HSL -> RGB == HSL -> x -> HSL -> RGB --
-- note: conversion to RGB is required due to potential ambiguity between colours

prop_hsl_rgb :: HSL -> Bool
prop_hsl_rgb hsl = hslToRGB hsl ~= hslToRGB hsl'
    where hsl' = rgbToHSL (hslToRGB hsl)

prop_hsl_hsv :: HSL -> Bool
prop_hsl_hsv hsl = hslToRGB hsl ~= hslToRGB hsl'
    where hsl' = hsvToHSL (hslToHSV hsl)

prop_hsl_rgba :: HSL -> Bool
prop_hsl_rgba hsl = hslToRGB hsl ~= hslToRGB hsl'
    where hsl' = rgbaToHSL (hslToRGBA hsl)
