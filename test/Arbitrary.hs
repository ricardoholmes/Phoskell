{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Arbitrary where

import Test.QuickCheck ( Gen, Arbitrary(arbitrary) )
import Test.Massiv.Core.Common ()
import Graphics.ImageProcessing.Core
import Control.Monad

-- implement Arbitrary typeclass for custom types --

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
