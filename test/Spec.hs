{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Data.Proxy
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.Massiv.Core.Common ()
import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Core.Color
import Graphics.ImageProcessing.Transformations.Rotation
import Graphics.ImageProcessing.Transformations

instance Arbitrary a => Arbitrary (Pixel1 a) where
    arbitrary :: Arbitrary a => Gen (Pixel1 a)
    arbitrary = Pixel1 <$> arbitrary

instance Arbitrary a => Arbitrary (Pixel2 a) where
    arbitrary :: Arbitrary a => Gen (Pixel2 a)
    arbitrary = liftM2 Pixel2 arbitrary arbitrary

instance Arbitrary a => Arbitrary (Pixel3 a) where
    arbitrary :: Arbitrary a => Gen (Pixel3 a)
    arbitrary = liftM3 Pixel3 arbitrary arbitrary arbitrary

instance Arbitrary a => Arbitrary (Pixel4 a) where
    arbitrary :: Arbitrary a => Gen (Pixel4 a)
    arbitrary = liftM4 Pixel4 arbitrary arbitrary arbitrary arbitrary

instance Arbitrary a => Arbitrary (Image a) where
    arbitrary :: Arbitrary a => Gen (Image a)
    arbitrary = BaseImage <$> arbitrary

-- | roughly equal
(~=) :: (Pixel p, Integral n) => p n -> p n -> Bool
p1 ~= p2 = sum (fmap minDiff ps) <= 5
    where
        ps = (,) <$> p1 <*> p2
        minDiff (x,y) = min (x - y) (y - x)

-- Gray == Gray -> x -> Gray --

prop_gray_rgb :: Gray -> Bool
prop_gray_rgb gray = gray ~= rgbToGray (grayToRGB gray)

prop_gray_hsv :: Gray -> Bool
prop_gray_hsv gray = gray ~= hsvToGray (grayToHSV gray)

prop_gray_hsl :: Gray -> Bool
prop_gray_hsl gray = gray ~= hslToGray (grayToHSL gray)

prop_gray_rgba :: Gray -> Bool
prop_gray_rgba gray = gray ~= rgbaToGray (grayToRGBA gray)

-- RGB == RGB -> x -> RGB --

prop_rgb_hsv :: RGB -> Bool
prop_rgb_hsv rgb = rgb ~= hsvToRGB (rgbToHSV rgb)

prop_rgb_hsl :: RGB -> Bool
prop_rgb_hsl rgb = rgb ~= hslToRGB (rgbToHSL rgb)

prop_rgb_rgba :: RGB -> Bool
prop_rgb_rgba rgb = rgb ~= rgbaToRGB (rgbToRGBA rgb)

-- HSV -> RGB == HSV -> x -> HSV -> RGB --
-- note: conversion to RGB is required due to potential ambiguity between colors

prop_hsv_rgb :: HSV -> Bool
prop_hsv_rgb hsv = hsvToRGB hsv ~= hsvToRGB (rgbToHSV (hsvToRGB hsv))

prop_hsv_hsl :: HSV -> Bool
prop_hsv_hsl hsv = hsvToRGB hsv ~= hsvToRGB (hslToHSV (hsvToHSL hsv))

prop_hsv_rgba :: HSV -> Bool
prop_hsv_rgba hsv = hsvToRGB hsv ~= hsvToRGB (rgbaToHSV (hsvToRGBA hsv))

-- HSL -> RGB == HSL -> x -> HSL -> RGB --
-- note: conversion to RGB is required due to potential ambiguity between colors

prop_hsl_rgb :: HSL -> Bool
prop_hsl_rgb hsl = hslToRGB hsl ~= hslToRGB (rgbToHSL (hslToRGB hsl))

prop_hsl_hsv :: HSL -> Bool
prop_hsl_hsv hsl = hslToRGB hsl ~= hslToRGB (hsvToHSL (hslToHSV hsl))

prop_hsl_rgba :: HSL -> Bool
prop_hsl_rgba hsl = hslToRGB hsl ~= hslToRGB (rgbaToHSL (hslToRGBA hsl))

-- collections of color conversion tests --

propsConversionsGray :: IO ()
propsConversionsGray = do
        putStrLn "Gray conversions"
        quickCheck prop_gray_rgb
        quickCheck prop_gray_hsv
        quickCheck prop_gray_hsl
        quickCheck prop_gray_rgba

propsConversionsRGB :: IO ()
propsConversionsRGB = do
        putStrLn "RGB conversions"
        quickCheck prop_rgb_hsv
        quickCheck prop_rgb_hsl
        quickCheck prop_rgb_rgba

propsConversionsHSV :: IO ()
propsConversionsHSV = do
        putStrLn "HSV conversions"
        quickCheck prop_hsv_rgb
        quickCheck prop_hsv_hsl
        quickCheck prop_hsv_rgba

propsConversionsHSL :: IO ()
propsConversionsHSL = do
        putStrLn "HSL conversions"
        quickCheck prop_hsl_hsv
        quickCheck prop_hsl_rgb
        quickCheck prop_hsl_rgba

-- typeclass laws for pixels

propsLawsPixel1 :: IO ()
propsLawsPixel1 = do
        putStrLn "Pixel1 laws"
        lawsCheck (eqLaws pa)
        lawsCheck (ordLaws pa)
        lawsCheck (functorLaws p)
        lawsCheck (applicativeLaws p)
        lawsCheck (foldableLaws p)
        lawsCheck (traversableLaws p)
        lawsCheck (numLaws pa)
        lawsCheck (enumLaws pa)
    where
        pa = Proxy :: Proxy (Pixel1 Integer)
        p = Proxy :: Proxy Pixel1

propsLawsPixel2 :: IO ()
propsLawsPixel2 = do
        putStrLn "Pixel2 laws"
        lawsCheck (eqLaws pa)
        lawsCheck (functorLaws p)
        lawsCheck (applicativeLaws p)
        lawsCheck (foldableLaws p)
        lawsCheck (traversableLaws p)
        lawsCheck (numLaws pa)
    where
        pa = Proxy :: Proxy (Pixel2 Integer)
        p = Proxy :: Proxy Pixel2

propsLawsPixel3 :: IO ()
propsLawsPixel3 = do
        putStrLn "Pixel3 laws"
        lawsCheck (eqLaws pa)
        lawsCheck (functorLaws p)
        lawsCheck (applicativeLaws p)
        lawsCheck (foldableLaws p)
        lawsCheck (traversableLaws p)
        lawsCheck (numLaws pa)
    where
        pa = Proxy :: Proxy (Pixel3 Integer)
        p = Proxy :: Proxy Pixel3

propsLawsPixel4 :: IO ()
propsLawsPixel4 = do
        putStrLn "Pixel4 laws"
        lawsCheck (eqLaws pa)
        lawsCheck (functorLaws p)
        lawsCheck (applicativeLaws p)
        lawsCheck (foldableLaws p)
        lawsCheck (traversableLaws p)
        lawsCheck (numLaws pa)
    where
        pa = Proxy :: Proxy (Pixel4 Integer)
        p = Proxy :: Proxy Pixel4

-- lossless image transformations --

prop_transpose :: Image RGBA -> Bool
prop_transpose img = img == img :> transpose :> transpose

prop_mirrorX :: Image RGBA -> Bool
prop_mirrorX img = img == img :> mirrorX :> mirrorX

prop_mirrorY :: Image RGBA -> Bool
prop_mirrorY img = img == img :> mirrorY :> mirrorY

prop_rot90x4 :: Image RGBA -> Bool
prop_rot90x4 img = img == rot90x4 img
    where rot90x4 x = x :> rotate90 :> rotate90 :> rotate90 :> rotate90

prop_rot180x2 :: Image RGBA -> Bool
prop_rot180x2 img = img == rot180x2 img
    where rot180x2 x = x :> rotate180 :> rotate180

prop_rot270x4 :: Image RGBA -> Bool
prop_rot270x4 img = img == rot270x4 img
    where rot270x4 x = x :> rotate270 :> rotate270 :> rotate270 :> rotate270

prop_zoomOutIn :: Image RGBA -> Bool
prop_zoomOutIn img = img == img :> zoom 0.5 0 :> zoom 2 0

prop_zoomInOut :: Image RGBA -> Bool
prop_zoomInOut img = img == img :> zoom 2 0 :> zoom 0.5 0

propsImageTransformations :: IO ()
propsImageTransformations = do
        putStrLn "Image transformations"
        quickCheck prop_transpose
        quickCheck prop_mirrorX
        quickCheck prop_mirrorY
        quickCheck prop_rot90x4
        quickCheck prop_rot180x2
        quickCheck prop_rot270x4
        quickCheck prop_zoomOutIn
        quickCheck prop_zoomInOut

main :: IO ()
main = do propsConversionsGray
          propsConversionsRGB
          propsConversionsHSV
          propsConversionsHSL
          propsLawsPixel1
          propsLawsPixel2
          propsLawsPixel3
          propsLawsPixel4
          propsImageTransformations
