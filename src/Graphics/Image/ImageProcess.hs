{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Image.ImageProcess (
    ImageProcess(..),

    PointProcess(..),
    IPointProcess(..),
    MiscProcess(..),
    Pipeline(..),

    processImage,
    (-:>),
) where

import Graphics.Image
import qualified Data.Massiv.Array as M
import Graphics.Pixel (ColorModel, Pixel)

class ImageProcess ip where
    applyProcess :: ip a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b

-- | @applyProcess@ applied to an image
processImage :: (ColorModel cs e, ColorModel cs' e', ImageProcess ip) =>
                ip (Pixel cs e) (Pixel cs' e') -> Image cs e -> Image cs' e'
processImage p = Image . M.computeAs M.S . applyProcess p . M.delay . toArray

-- | Infix notation of @processImage@, with arguments flipped
(-:>) :: (ColorModel cs e,
          ColorModel cs' e',
          ImageProcess ip) =>
         Image cs e -> ip (Pixel cs e) (Pixel cs' e') -> Image cs' e'
(-:>) = flip processImage

infixl 6 -:>

-- | Main pipeline type, for use defining sequences of processes that can be
-- applied to images.
data Pipeline a b where
    -- | Pipeline constructor for linking together processes applied to an image
    (:>) :: (ImageProcess ipl, ImageProcess ipr) => ipl a x -> ipr x b -> Pipeline a b

infixr 5 :>

instance ImageProcess Pipeline where
    applyProcess :: Pipeline a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
    applyProcess (f :> g) = applyProcess g . applyProcess f

newtype PointProcess a b = PointProcess (a -> b)
newtype IPointProcess a b = IPointProcess (M.Ix2 -> a -> b)
newtype MiscProcess a b = MiscProcess (M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b)

instance ImageProcess (->) where
    applyProcess :: (a -> b) -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
    applyProcess = M.map

instance ImageProcess PointProcess where
    applyProcess :: PointProcess a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
    applyProcess (PointProcess f) = M.map f

instance ImageProcess IPointProcess where
    applyProcess :: IPointProcess a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
    applyProcess (IPointProcess f) = M.imap f

instance ImageProcess MiscProcess where
    applyProcess :: MiscProcess a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
    applyProcess (MiscProcess f) = f
