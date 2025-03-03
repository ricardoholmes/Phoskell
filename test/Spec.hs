{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Test.QuickCheck

import Graphics.ImageProcessing.Core.Color
import Graphics.ImageProcessing.Core
import Control.Monad

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

prop_hsv_hsl :: HSV -> Bool
prop_hsv_hsl hsv = hsv == hslToHSV (hsvToHSL hsv)

propsColorConversion :: IO ()
propsColorConversion = do quickCheck prop_hsv_hsl

main :: IO ()
main = do propsColorConversion
