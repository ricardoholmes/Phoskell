{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
-- | Core image and image process types.
module Graphics.Phoskell.Core.Image (
    ImageArray,
    ImageProcess(..),
    Image(..),

    toArray,
    toArrayUnboxed,
    toArrayStorable,
    (!),
    (!?),

    PointProcess(..),
    IPointProcess(..),
    ArrayProcess(..),
) where

import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as M
import Control.DeepSeq (NFData(..))

-- | Base array type for storing images
type ImageArray a = M.Array M.D M.Ix2 a

{- IMAGE PROCESSES -}

-- | Type class for all image processes
class ImageProcess ip where
    applyProcess :: ip a b -> ImageArray a -> ImageArray b

-- Core image process types
-- | Image process type to use for point processes.
newtype PointProcess a b = PointProcess (a -> b)
-- | Image process type to use for point processes given an index.
newtype IPointProcess a b = IPointProcess ((Int,Int) -> a -> b)
-- | Image process type to use for array processes.
newtype ArrayProcess a b = ArrayProcess (ImageArray a -> ImageArray b)

instance ImageProcess PointProcess where
    applyProcess :: PointProcess a b -> ImageArray a -> ImageArray b
    applyProcess (PointProcess f) = M.map f
    {-# INLINE applyProcess #-}

instance ImageProcess IPointProcess where
    applyProcess :: IPointProcess a b -> ImageArray a -> ImageArray b
    applyProcess (IPointProcess f) = M.imap (\(y:.x) -> f (x,y))
    {-# INLINE applyProcess #-}

instance ImageProcess ArrayProcess where
    applyProcess :: ArrayProcess a b -> ImageArray a -> ImageArray b
    applyProcess (ArrayProcess f) = f
    {-# INLINE applyProcess #-}

instance ImageProcess (->) where
    applyProcess :: (a -> b) -> ImageArray a -> ImageArray b
    applyProcess = M.map
    {-# INLINE applyProcess #-}

{- IMAGE -}

-- | Main image type used for images throughout the library.
data Image cs where
    -- | Base constructor; wrapper of the underlying array type
    BaseImage :: ImageArray cs -> Image cs
    -- | Pipeline constructor for linking together processes applied to an image
    (:>) :: (ImageProcess ip) => Image a -> ip a cs -> Image cs

-- | Compute an image into a delayed-pull array.
toArray :: Image cs -> ImageArray cs
toArray (BaseImage img) = img
toArray (img :> f) = applyProcess f (toArray img)
{-# INLINE toArray #-}

-- | Compute an image into an unboxed array.
toArrayUnboxed :: M.Unbox cs => Image cs -> M.Array M.U M.Ix2 cs
toArrayUnboxed = M.computeAs M.U . toArray

-- | Compute an image into a stored array.
toArrayStorable :: M.Storable cs => Image cs -> M.Array M.S M.Ix2 cs
toArrayStorable = M.computeAs M.S . toArray

-- | Unsafe image indexing.
(!) :: Image cs -> (Int,Int) -> cs
img ! (x,y) = M.evaluate' (toArray img) (y:.x)

-- | Safe image indexing.
(!?) :: Image cs -> (Int,Int) -> Maybe cs
img !? (x,y) = M.evaluateM (toArray img) (y:.x)

instance Functor Image where
    fmap :: (a -> b) -> Image a -> Image b
    fmap f img = img :> f
    {-# INLINE fmap #-}

instance Applicative Image where
    pure :: a -> Image a
    pure = BaseImage . pure
    {-# INLINE pure #-}

    (<*>) :: Image (a -> b) -> Image a -> Image b
    (<*>) f img = BaseImage $ toArray f <*> toArray img
    {-# INLINE (<*>) #-}

instance Num cs => Num (Image cs) where
    (+) :: Num cs => Image cs -> Image cs -> Image cs
    (+) x y = (+) <$> x <*> y
    {-# INLINE (+) #-}
    (*) :: Num cs => Image cs -> Image cs -> Image cs
    (*) x y = (*) <$> x <*> y
    {-# INLINE (*) #-}
    abs :: Num cs => Image cs -> Image cs
    abs = fmap abs
    {-# INLINE abs #-}
    signum :: Num cs => Image cs -> Image cs
    signum = fmap signum
    {-# INLINE signum #-}
    fromInteger :: Num cs => Integer -> Image cs
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}
    negate :: Num cs => Image cs -> Image cs
    negate = fmap negate
    {-# INLINE negate #-}

instance Eq a => Eq (Image a) where
    (==) :: Eq a => Image a -> Image a -> Bool
    imgX == imgY = toArray imgX == toArray imgY
    {-# INLINE (==) #-}

instance Show a => Show (Image a) where
    show :: Show a => Image a -> String
    show img = show $ toArray img
    {-# INLINE show #-}

instance NFData a => NFData (Image a) where
    rnf :: NFData a => Image a -> ()
    rnf = rnf . M.computeAs M.BN . toArray
    {-# INLINE rnf #-}
