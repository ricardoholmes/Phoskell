{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Test.QuickCheck
import Control.Monad
import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Core.Color

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

-- | roughly equal
(~=) :: (Pixel p, Integral n) => p n -> p n -> Bool
p1 ~= p2 = all (\(x,y) -> x - y <= 1 || y - x <= 1) ((,) <$> p1 <*> p2)

-- Gray -> x -> Gray --

prop_gray_rgb :: Gray -> Bool
prop_gray_rgb gray = gray ~= rgbToGray (grayToRGB gray)

prop_gray_hsv :: Gray -> Bool
prop_gray_hsv gray = gray ~= hsvToGray (grayToHSV gray)

prop_gray_hsl :: Gray -> Bool
prop_gray_hsl gray = gray ~= hslToGray (grayToHSL gray)

prop_gray_rgba :: Gray -> Bool
prop_gray_rgba gray = gray ~= rgbaToGray (grayToRGBA gray)

-- RGB -> x -> RGB --

prop_rgb_hsv :: RGB -> Bool
prop_rgb_hsv rgb = rgb ~= hsvToRGB (rgbToHSV rgb)

prop_rgb_hsl :: RGB -> Bool
prop_rgb_hsl rgb = rgb ~= hslToRGB (rgbToHSL rgb)

prop_rgb_rgba :: RGB -> Bool
prop_rgb_rgba rgb = rgb ~= rgbaToRGB (rgbToRGBA rgb)

-- HSV -> x -> HSV --

prop_hsv_rgb :: HSV -> Bool
prop_hsv_rgb hsv = hsv ~= rgbToHSV (hsvToRGB hsv)

prop_hsv_hsl :: HSV -> Bool
prop_hsv_hsl hsv = hsv ~= hslToHSV (hsvToHSL hsv)

prop_hsv_rgba :: HSV -> Bool
prop_hsv_rgba hsv = hsv ~= rgbaToHSV (hsvToRGBA hsv)

-- HSL -> x -> HSL --

prop_hsl_hsv :: HSL -> Bool
prop_hsl_hsv hsl = hsl ~= hsvToHSL (hslToHSV hsl)

prop_hsl_rgb :: HSL -> Bool
prop_hsl_rgb hsl = hsl ~= rgbToHSL (hslToRGB hsl)

prop_hsl_rgba :: HSL -> Bool
prop_hsl_rgba hsl = hsl ~= rgbaToHSL (hslToRGBA hsl)

-- collections of tests --

propsConversionsGray :: IO ()
propsConversionsGray = do
    quickCheck prop_gray_rgb
    quickCheck prop_gray_hsv
    quickCheck prop_gray_hsl
    quickCheck prop_gray_rgba

propsConversionsRGB :: IO ()
propsConversionsRGB = do
    quickCheck prop_rgb_hsv
    quickCheck prop_rgb_hsl
    quickCheck prop_rgb_rgba

propsConversionsHSV :: IO ()
propsConversionsHSV = do
    quickCheck prop_hsv_rgb
    quickCheck prop_hsv_hsl
    quickCheck prop_hsv_rgba

propsConversionsHSL :: IO ()
propsConversionsHSL = do
    quickCheck prop_hsl_hsv
    quickCheck prop_hsl_rgb
    quickCheck prop_hsl_rgba

main :: IO ()
main = do propsConversionsGray
          propsConversionsRGB
          propsConversionsHSV
          propsConversionsHSL
