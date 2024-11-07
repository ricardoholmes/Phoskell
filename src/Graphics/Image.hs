{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Image (
    Image(..),
    ImageProcess(..),
    generateImage,
) where

import qualified Data.Massiv.Array as M

-- | Base array type for storing images
type ImageArray a = M.Array M.D M.Ix2 a

-- | Wrapper type for any and all types of image processes
data ImageProcess a b = -- | Point process
                        PointProcess (a -> b) |
                         -- | Index-aware point process
                        IPointProcess (M.Ix2 -> a -> b) |
                        -- | Anything else
                        MiscProcess (ImageArray a -> ImageArray b)

-- | Core image type for usage across most operations in this library
data Image a where
    -- | Base image constructor
    BaseImage :: ImageArray a -> Image a
    -- | Pipeline constructor for linking together processes applied to an image
    (:>) :: Image x -> ImageProcess x a -> Image a

infixl 5 :>

-- | Generates the whole image, converting it into an array
generateImage :: Image a -> ImageArray a
generateImage (BaseImage img) = img
generateImage (img :> f) = applyProcess f (generateImage img)

-- | Applies a single process to an image array
applyProcess :: ImageProcess a b -> ImageArray a -> ImageArray b
applyProcess (PointProcess f) = M.map f
applyProcess (IPointProcess f) = M.imap f
applyProcess (MiscProcess f) = f

instance Functor Image where
    fmap :: (a -> b) -> Image a -> Image b
    fmap = flip (:>) . PointProcess

instance Applicative Image where
    pure :: a -> Image a
    pure = BaseImage . pure

    (<*>) :: Image (a -> b) -> Image a -> Image b
    fImg <*> img = BaseImage $ generateImage fImg <*> generateImage img
