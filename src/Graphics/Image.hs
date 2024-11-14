{-# LANGUAGE InstanceSigs #-}

module Graphics.Image (
    Image(..),
    toArray,
) where

import qualified Data.Massiv.Array as M
import Graphics.Pixel ( ColorModel, Pixel )

-- | Base array type for storing images
type ImageArray cs e = M.Array M.S M.Ix2 (Pixel cs e)

-- | Core image type for usage across most operations in this library
-- 
-- Requires color space and precision of elements.
newtype Image cs e = Image (ImageArray cs e)

-- | Extract data from an image as an array
toArray :: Image cs e -> ImageArray cs e
toArray (Image img) = img

instance (ColorModel cs e) => Num (Image cs e) where
    (+) :: Image cs e -> Image cs e -> Image cs e
    Image x + Image y = Image $ M.computeAs M.S (M.zipWith (+) x y)

    (*) :: Image cs e -> Image cs e -> Image cs e
    Image x * Image y = Image $ M.computeAs M.S (M.zipWith (*) x y)

    abs :: Image cs e -> Image cs e
    abs (Image x) = Image $ M.computeAs M.S (M.map abs x)

    signum :: Image cs e -> Image cs e
    signum (Image x) = Image $ M.computeAs M.S (M.map signum x)

    fromInteger :: Integer -> Image cs e
    fromInteger = Image . M.singleton . fromInteger

    negate :: Image cs e -> Image cs e
    negate (Image x) = Image $ M.computeAs M.S (M.map negate x)
