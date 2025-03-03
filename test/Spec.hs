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

prop_hsv_hsl :: HSV -> Bool
prop_hsv_hsl hsv = hsv ~= hslToHSV (hsvToHSL hsv)

propsColorConversion :: IO ()
propsColorConversion = do quickCheck prop_hsv_hsl

main :: IO ()
main = do propsColorConversion
