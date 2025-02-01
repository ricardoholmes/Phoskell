{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Graphics.ImageProcessing.Core.Pixel (
    Pixel1(..),
    Pixel2(..),
    Pixel3(..),
    Pixel4(..),
    Pixel(..),
) where

import Control.DeepSeq ( NFData (..) )

--- pixel types ---

newtype Pixel1 a = Pixel1 a deriving (Show,Eq,Ord)
data Pixel2 a = Pixel2 !a !a deriving (Show,Eq)
data Pixel3 a = Pixel3 !a !a !a deriving (Show,Eq)
data Pixel4 a = Pixel4 !a !a !a !a deriving (Show,Eq)

--- functor ---

instance Functor Pixel1 where
    fmap :: (a -> b) -> Pixel1 a -> Pixel1 b
    fmap g (Pixel1 x) = Pixel1 (g x)

instance Functor Pixel2 where
    fmap :: (a -> b) -> Pixel2 a -> Pixel2 b
    fmap g (Pixel2 x y) = Pixel2 (g x) (g y)

instance Functor Pixel3 where
    fmap :: (a -> b) -> Pixel3 a -> Pixel3 b
    fmap g (Pixel3 x y z) = Pixel3 (g x) (g y) (g z)

instance Functor Pixel4 where
    fmap :: (a -> b) -> Pixel4 a -> Pixel4 b
    fmap g (Pixel4 x y z w) = Pixel4 (g x) (g y) (g z) (g w)

--- applicative ---
instance Applicative Pixel1 where
    pure :: a -> Pixel1 a
    pure = Pixel1

    (<*>) :: Pixel1 (a -> b) -> Pixel1 a -> Pixel1 b
    (<*>) (Pixel1 g) (Pixel1 x) = Pixel1 (g x)

instance Applicative Pixel2 where
    pure :: a -> Pixel2 a
    pure x = Pixel2 x x

    (<*>) :: Pixel2 (a -> b) -> Pixel2 a -> Pixel2 b
    (<*>) (Pixel2 g1 g2) (Pixel2 x1 x2) = Pixel2 (g1 x1) (g2 x2)

instance Applicative Pixel3 where
    pure :: a -> Pixel3 a
    pure x = Pixel3 x x x

    (<*>) :: Pixel3 (a -> b) -> Pixel3 a -> Pixel3 b
    (<*>) (Pixel3 g1 g2 g3) (Pixel3 x1 x2 x3) = Pixel3 (g1 x1) (g2 x2) (g3 x3)

instance Applicative Pixel4 where
    pure :: a -> Pixel4 a
    pure x = Pixel4 x x x x

    (<*>) :: Pixel4 (a -> b) -> Pixel4 a -> Pixel4 b
    (<*>) (Pixel4 g1 g2 g3 g4) (Pixel4 x1 x2 x3 x4) = Pixel4 (g1 x1) (g2 x2) (g3 x3) (g4 x4)

-- foldable --

instance Foldable Pixel1 where
    foldMap :: Monoid m => (a -> m) -> Pixel1 a -> m
    foldMap f (Pixel1 x1) = f x1

instance Foldable Pixel2 where
    foldMap :: Monoid m => (a -> m) -> Pixel2 a -> m
    foldMap f (Pixel2 x1 x2) = f x1 <> f x2

instance Foldable Pixel3 where
    foldMap :: Monoid m => (a -> m) -> Pixel3 a -> m
    foldMap f (Pixel3 x1 x2 x3) = f x1 <> f x2 <> f x3

instance Foldable Pixel4 where
    foldMap :: Monoid m => (a -> m) -> Pixel4 a -> m
    foldMap f (Pixel4 x1 x2 x3 x4) = f x1 <> f x2 <> f x3 <> f x4

-- traversable --

instance Traversable Pixel1 where
    traverse :: Applicative f => (a -> f b) -> Pixel1 a -> f (Pixel1 b)
    traverse f (Pixel1 x) = Pixel1 <$> f x

instance Traversable Pixel2 where
    traverse :: Applicative f => (a -> f b) -> Pixel2 a -> f (Pixel2 b)
    traverse f (Pixel2 x y) = Pixel2 <$> f x <*> f y

instance Traversable Pixel3 where
    traverse :: Applicative f => (a -> f b) -> Pixel3 a -> f (Pixel3 b)
    traverse f (Pixel3 x y z) = Pixel3 <$> f x <*> f y <*> f z

instance Traversable Pixel4 where
    traverse :: Applicative f => (a -> f b) -> Pixel4 a -> f (Pixel4 b)
    traverse f (Pixel4 x y z w) = Pixel4 <$> f x <*> f y <*> f z <*> f w

--- Pixel class ---

class (Traversable p, Applicative p) => Pixel p where
    dot :: Num a => p a -> p a -> Pixel1 a
    dot p1 p2 = pure $ sum ((*) <$> p1 <*> p2)

    multScalar :: Num a => p a -> a -> p a
    multScalar p m = fmap (m*) p

instance Pixel Pixel1
instance Pixel Pixel2
instance Pixel Pixel3
instance Pixel Pixel4

--- instances inherited from contained elements ---

instance {-# INCOHERENT #-} (Pixel p, Num a) => Num (p a) where
    (+) :: (Pixel p, Num a) => p a -> p a -> p a
    (+) p1 p2 = (+) <$> p1 <*> p2
    (*) :: (Pixel p, Num a) => p a -> p a -> p a
    (*) p1 p2 = (*) <$> p1 <*> p2
    abs :: (Pixel p, Num a) => p a -> p a
    abs = fmap abs
    signum :: (Pixel p, Num a) => p a -> p a
    signum = fmap signum
    fromInteger :: (Pixel p, Num a) => Integer -> p a
    fromInteger = pure . fromInteger
    negate :: (Pixel p, Num a) => p a -> p a
    negate = fmap negate

instance {-# INCOHERENT #-} (Pixel p, Fractional a) => Fractional (p a) where
    fromRational :: (Pixel p, Fractional a) => Rational -> p a
    fromRational = pure . fromRational
    recip :: (Pixel p, Fractional a) => p a -> p a
    recip = fmap recip

instance {-# INCOHERENT #-} (Pixel p, Floating a) => Floating (p a) where
    pi :: (Pixel p, Floating a) => p a
    pi = pure pi
    exp :: (Pixel p, Floating a) => p a -> p a
    exp = fmap exp
    log :: (Pixel p, Floating a) => p a -> p a
    log = fmap log
    sin :: (Pixel p, Floating a) => p a -> p a
    sin = fmap sin
    cos :: (Pixel p, Floating a) => p a -> p a
    cos = fmap cos
    asin :: (Pixel p, Floating a) => p a -> p a
    asin = fmap asin
    acos :: (Pixel p, Floating a) => p a -> p a
    acos = fmap acos
    atan :: (Pixel p, Floating a) => p a -> p a
    atan = fmap atan
    sinh :: (Pixel p, Floating a) => p a -> p a
    sinh = fmap sinh
    cosh :: (Pixel p, Floating a) => p a -> p a
    cosh = fmap cosh
    asinh :: (Pixel p, Floating a) => p a -> p a
    asinh = fmap asinh
    acosh :: (Pixel p, Floating a) => p a -> p a
    acosh = fmap acosh
    atanh :: (Pixel p, Floating a) => p a -> p a
    atanh = fmap atanh

instance {-# INCOHERENT #-} (Pixel p, NFData a) => NFData (p a) where
    rnf :: (Pixel p, NFData a) => p a -> ()
    rnf = foldMap rnf
