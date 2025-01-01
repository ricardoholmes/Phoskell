{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Graphics.ImageProcessing (
    Ix2(..),
    Sz(..),
    Image(..),
    (!),
    generateImage,
) where

import Graphics.ImageProcessing.Processes
import Graphics.ImageProcessing.Core.Image
import qualified Data.Massiv.Array as MA
import Data.Massiv.Array (Ix2, D (D), Comp (Par), Sz2, Sz)

generateImage :: Sz2 -> (Ix2 -> a) -> Image a
generateImage sz f = BaseImage (MA.makeArrayR D Par sz f)

instance Functor Image where
    fmap :: (a -> b) -> Image a -> Image b
    fmap f = flip (:>) (PointProcess f)

instance Applicative Image where
    pure :: a -> Image a
    pure = BaseImage . MA.singleton

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
