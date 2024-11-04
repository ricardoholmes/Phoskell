{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Image (
    Image(..),
    ImageProcess(..),
    generateImage,
) where

import qualified Data.Massiv.Array as M

type ImageArray a = M.Array M.D M.Ix3 a

data ImageProcess a b = PointProcess (a -> b) -- point process
                      | IPointProcess (M.Ix3 -> a -> b) -- index-aware point process
                      | MiscProcess (ImageArray a -> ImageArray b) -- anything else

data Image a where
    BaseImage :: ImageArray a -> Image a
    (:>) :: Image x -> ImageProcess x a -> Image a

infixl 5 :>

generateImage :: Image a -> ImageArray a
generateImage (BaseImage img) = img
generateImage (img :> f) = applyProcess f (generateImage img)

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
