{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Graphics.Image (
    Image(..),
) where

import Graphics.Image.ImageProcess
import Graphics.Image.Internal
import Data.Massiv.Array (singleton)

instance Functor Image where
    fmap :: (a -> b) -> Image a -> Image b
    fmap f = flip (:>) (PointProcess f)

instance Applicative Image where
    pure :: a -> Image a
    pure = BaseImage . singleton

    (<*>) :: Image (a -> b) -> Image a -> Image b
    (<*>) f img = img :> IPointProcess (f !)

instance Num cs => Num (Image cs) where
    (+) :: Num cs => Image cs -> Image cs -> Image cs
    (+) x y = (+) <$> x <*> y
    (*) :: Num cs => Image cs -> Image cs -> Image cs
    (*) x y = (*) <$> x <*> y
    abs :: Num cs => Image cs -> Image cs
    abs = fmap abs
    signum :: Num cs => Image cs -> Image cs
    signum = fmap signum
    fromInteger :: Num cs => Integer -> Image cs
    fromInteger = pure . fromInteger
    negate :: Num cs => Image cs -> Image cs
    negate = fmap negate
