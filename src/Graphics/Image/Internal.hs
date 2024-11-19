{-# LANGUAGE GADTs #-}
module Graphics.Image.Internal (
    ImageArray,
    ImageProcess(..),
    Image(..),

    toArray,
    (!),
) where

import qualified Data.Massiv.Array as M

-- | Base array type for storing images
type ImageArray a = M.Array M.D M.Ix2 a

class ImageProcess ip where
    applyProcess :: ip a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b

data Image cs where
    BaseImage :: ImageArray cs -> Image cs
    -- | Pipeline constructor for linking together processes applied to an image
    (:>) :: (ImageProcess ip) => Image a -> ip a cs -> Image cs

toArray :: Image cs -> ImageArray cs
toArray (BaseImage img) = img
toArray (img :> f) = applyProcess f (toArray img)

(!) :: Image cs -> M.Ix2 -> cs
(!) = M.evaluate' . toArray
