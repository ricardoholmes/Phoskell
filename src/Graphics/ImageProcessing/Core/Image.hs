{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.ImageProcessing.Core.Image (
    ImageArray,
    ImageProcess(..),
    Image(..),

    toArray,
    (!),

    PointProcess(..),
    IPointProcess(..),
    MiscProcess(..),
) where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array as MA

-- | Base array type for storing images
type ImageArray a = M.Array M.D M.Ix2 a

{- IMAGE PROCESSES -}

-- | Type class for all image processes
class ImageProcess ip where
    applyProcess :: ip a b -> ImageArray a -> ImageArray b

-- Core image process types
newtype PointProcess a b = PointProcess (a -> b)
newtype IPointProcess a b = IPointProcess (M.Ix2 -> a -> b)
newtype MiscProcess a b = MiscProcess (ImageArray a -> ImageArray b)

instance ImageProcess PointProcess where
    applyProcess :: PointProcess a b -> ImageArray a -> ImageArray b
    applyProcess (PointProcess f) = M.map f

instance ImageProcess IPointProcess where
    applyProcess :: IPointProcess a b -> ImageArray a -> ImageArray b
    applyProcess (IPointProcess f) = M.imap f

instance ImageProcess MiscProcess where
    applyProcess :: MiscProcess a b -> ImageArray a -> ImageArray b
    applyProcess (MiscProcess f) = f

{- IMAGE -}

data Image cs where
    -- | Base constructor; wrapper of the underlying array type
    BaseImage :: ImageArray cs -> Image cs
    -- | Pipeline constructor for linking together processes applied to an image
    (:>) :: (ImageProcess ip) => Image a -> ip a cs -> Image cs

toArray :: Image cs -> ImageArray cs
toArray (BaseImage img) = img
toArray (img :> f) = applyProcess f (toArray img)
{-# INLINE toArray #-}

(!) :: Image cs -> M.Ix2 -> cs
(!) = M.evaluate' . toArray

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
