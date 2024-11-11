{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Graphics.Image (
    Image(..),
    ImageProcess(..),
    Pipeline(..),

    toArray,
    (-:>),
    applyPipeline,
    applyProcess,
) where

import qualified Data.Massiv.Array as M
import Data.Massiv.Array.IO (Pixel, ColorModel)

-- | Base array type for storing images
type ImageArray cs e = M.Array M.S M.Ix2 (Pixel cs e)

-- | Wrapper type for any and all types of image processes
data ImageProcess a b = -- | Point process
                        PointProcess (a -> b) |
                         -- | Index-aware point process
                        IPointProcess (M.Ix2 -> a -> b) |
                        -- | Any processes not covered by another constructor
                        MiscProcess (M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b)

-- | Core image type for usage across most operations in this library
-- 
-- Requires color space and precision of elements.
data Image cs e = Image !(ImageArray cs e)

-- | Main pipeline type, for use defining sequences of processes that can be
-- applied to images.
data Pipeline a b where
    -- | Identity pipeline
    Id :: Pipeline a a
    -- | Pipeline constructor for linking together processes applied to an image
    (:>) :: ImageProcess a x -> Pipeline x b -> Pipeline a b

infixr 5 :>

-- | Extract data from an image as an array
toArray :: Image cs e -> ImageArray cs e
toArray (Image img) = img

-- | Alias of `applyPipeline`, applies a pipeline to an image
(-:>) :: (ColorModel cs e, ColorModel cs' e') =>
        Pipeline (Pixel cs e) (Pixel cs' e') -> Image cs e -> Image cs' e'
(-:>) = applyPipeline

infixl 5 -:>

-- | Applies a pipeline to an image
applyPipeline :: (ColorModel cs e, ColorModel cs' e') =>
        Pipeline (Pixel cs e) (Pixel cs' e') -> Image cs e -> Image cs' e'
applyPipeline p = Image . M.computeAs M.S . applyPipeline' p . M.delay . toArray

applyPipeline' :: Pipeline a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
applyPipeline' Id = id
applyPipeline' (f :> p) = applyPipeline' p . applyProcess f

-- | Applies a single process to an image array
applyProcess :: ImageProcess a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
applyProcess (PointProcess f) = M.map f
applyProcess (IPointProcess f) = M.imap f
applyProcess (MiscProcess f) = f
