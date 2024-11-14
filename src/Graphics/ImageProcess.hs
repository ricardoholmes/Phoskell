{-# LANGUAGE GADTs #-}

module Graphics.ImageProcess (
    ImageProcess(..),
    Pipeline(..),

    (-:>),
    applyPipeline,
    applyProcess,
) where

import Graphics.Image
import qualified Data.Massiv.Array as M
import Graphics.Pixel (ColorModel, Pixel)

-- | Wrapper type for any and all types of image processes
data ImageProcess a b = -- | Point process
                        PointProcess (a -> b) |
                         -- | Index-aware point process
                        IPointProcess (M.Ix2 -> a -> b) |
                        -- | Any processes not covered by another constructor
                        MiscProcess (M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b)

-- | Main pipeline type, for use defining sequences of processes that can be
-- applied to images.
data Pipeline a b where
    -- | Identity pipeline
    Id :: Pipeline a a
    -- | Pipeline constructor for linking together processes applied to an image
    (:>) :: ImageProcess a x -> Pipeline x b -> Pipeline a b

infixr 5 :>

-- | Alias of `applyPipeline`, applies a pipeline to an image
(-:>) :: (ColorModel cs e, ColorModel cs' e') =>
        Pipeline (Pixel cs e) (Pixel cs' e') -> Image cs e -> Image cs' e'
(-:>) = applyPipeline

infixl 5 -:>

-- | Applies a pipeline to an image
applyPipeline :: (ColorModel cs e, ColorModel cs' e') =>
        Pipeline (Pixel cs e) (Pixel cs' e') -> Image cs e -> Image cs' e'
applyPipeline p = Image . M.computeAs M.S . applyPipeline' p . M.delay . toArray

-- | Helper function for applying a pipeline.
-- 
-- Not exported
applyPipeline' :: Pipeline a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
applyPipeline' Id = id
applyPipeline' (f :> p) = applyPipeline' p . applyProcess f

-- | Applies a single process to an image array
applyProcess :: ImageProcess a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
applyProcess (PointProcess f) = M.map f
applyProcess (IPointProcess f) = M.imap f
applyProcess (MiscProcess f) = f
