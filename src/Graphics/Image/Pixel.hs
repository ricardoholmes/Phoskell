module Graphics.Image.Pixel (
    Pixel1(..),
    Pixel2(..),
    Pixel3(..),
    Pixel4(..),

    dot2,
    dot3,
    dot4,

    multScalar1,
    multScalar2,
    multScalar3,
    multScalar4,
) where

--- pixel types ---

newtype Pixel1 a = Pixel1 a
data Pixel2 a = Pixel2 a a
data Pixel3 a = Pixel3 a a a
data Pixel4 a = Pixel4 a a a a

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


--- num ---

instance Num a => Num (Pixel1 a) where
    (+) :: Num a => Pixel1 a -> Pixel1 a -> Pixel1 a
    (+) p1 p2 = (+) <$> p1 <*> p2

    (*) :: Num a => Pixel1 a -> Pixel1 a -> Pixel1 a
    (*) p1 p2 = (*) <$> p1 <*> p2

    abs :: Num a => Pixel1 a -> Pixel1 a
    abs = fmap abs

    signum :: Num a => Pixel1 a -> Pixel1 a
    signum = fmap signum

    fromInteger :: Num a => Integer -> Pixel1 a
    fromInteger = pure . fromInteger

    negate :: Num a => Pixel1 a -> Pixel1 a
    negate = fmap negate


instance Num a => Num (Pixel2 a) where
    (+) :: Num a => Pixel2 a -> Pixel2 a -> Pixel2 a
    (+) p1 p2 = (+) <$> p1 <*> p2

    (*) :: Num a => Pixel2 a -> Pixel2 a -> Pixel2 a
    (*) p1 p2 = (*) <$> p1 <*> p2

    abs :: Num a => Pixel2 a -> Pixel2 a
    abs = fmap abs

    signum :: Num a => Pixel2 a -> Pixel2 a
    signum = fmap signum

    fromInteger :: Num a => Integer -> Pixel2 a
    fromInteger = pure . fromInteger

    negate :: Num a => Pixel2 a -> Pixel2 a
    negate = fmap negate


instance Num a => Num (Pixel3 a) where
    (+) :: Num a => Pixel3 a -> Pixel3 a -> Pixel3 a
    (+) p1 p2 = (+) <$> p1 <*> p2

    (*) :: Num a => Pixel3 a -> Pixel3 a -> Pixel3 a
    (*) p1 p2 = (*) <$> p1 <*> p2

    abs :: Num a => Pixel3 a -> Pixel3 a
    abs = fmap abs

    signum :: Num a => Pixel3 a -> Pixel3 a
    signum = fmap signum

    fromInteger :: Num a => Integer -> Pixel3 a
    fromInteger = pure . fromInteger

    negate :: Num a => Pixel3 a -> Pixel3 a
    negate = fmap negate


instance Num a => Num (Pixel4 a) where
    (+) :: Num a => Pixel4 a -> Pixel4 a -> Pixel4 a
    (+) p1 p2 = (+) <$> p1 <*> p2

    (*) :: Num a => Pixel4 a -> Pixel4 a -> Pixel4 a
    (*) p1 p2 = (*) <$> p1 <*> p2

    abs :: Num a => Pixel4 a -> Pixel4 a
    abs = fmap abs

    signum :: Num a => Pixel4 a -> Pixel4 a
    signum = fmap signum

    fromInteger :: Num a => Integer -> Pixel4 a
    fromInteger = pure . fromInteger

    negate :: Num a => Pixel4 a -> Pixel4 a
    negate = fmap negate


--- important functions ---

dot2 :: Num a => Pixel2 a -> Pixel2 a -> Pixel1 a
dot2 (Pixel2 x1 x2) (Pixel2 y1 y2) = pure $ (x1 * y1) + (x2 * y2)

dot3 :: Num a => Pixel3 a -> Pixel3 a -> Pixel1 a
dot3 (Pixel3 x1 x2 x3) (Pixel3 y1 y2 y3) = pure $ (x1 * y1) + (x2 * y2) + (x3 * y3)

dot4 :: Num a => Pixel4 a -> Pixel4 a -> Pixel1 a
dot4 (Pixel4 x1 x2 x3 x4) (Pixel4 y1 y2 y3 y4) = pure $ (x1 * y1) + (x2 * y2) + (x3 * y3) + (x4 * y4)

multScalar1 :: Num a => a -> Pixel1 a -> Pixel1 a
multScalar1 m = fmap (m*)

multScalar2 :: Num a => a -> Pixel2 a -> Pixel2 a
multScalar2 m = fmap (m*)

multScalar3 :: Num a => a -> Pixel3 a -> Pixel3 a
multScalar3 m = fmap (m*)

multScalar4 :: Num a => a -> Pixel4 a -> Pixel4 a
multScalar4 m = fmap (m*)
