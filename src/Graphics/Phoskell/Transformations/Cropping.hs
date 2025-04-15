{-# LANGUAGE ScopedTypeVariables #-}
-- | Cropping processes.
module Graphics.Phoskell.Transformations.Cropping (
    cropToSize,
    cropToAspectRatio,
    cropToAspectRatio',
) where

import Graphics.Phoskell.Transformations.Internal
import Graphics.Phoskell.Processes
import qualified Data.Massiv.Array as M
import Data.Fixed (mod')

-- | Crops an image to the given size.
cropToSize :: (Int,Int) -> ArrayProcess a a
cropToSize (newW,newH) = ArrayProcess (\img ->
        let (M.Sz2 h w) = M.size img
            (centreX :: Double) = fromIntegral w / 2
            (centreY :: Double) = fromIntegral h / 2
            w' = fromIntegral newW / 2
            h' = fromIntegral newH / 2
            xm = floor $ centreX - w'
            ym = floor $ centreY - h'
            xM = floor $ centreX + w' - 1
            yM = floor $ centreY + h' - 1
        in applyProcess (
                extractRegionUnsafe (xm,ym) (xM,yM)
            ) img
    )

-- | Crops an image to the given aspect ratio.
--
-- Aspect ratio should be given in terms of width relative to height,
-- e.g. (16,9) for 16:9.
--
-- An aspect ratio impossible for the image's size (e.g. 16:9 for a 2x2 image)
-- will result in an empty (0x0) image.
--
-- May throw an error if given an invalid aspect ratio.
cropToAspectRatio :: (Int,Int) -> ArrayProcess a a
cropToAspectRatio (relX,relY) = ArrayProcess (\img ->
        let (M.Sz2 h w) = M.size img
            d = gcd relX relY
            x = relX `div` d
            y = relY `div` d
        in case compare (w * y) (h * x) of
            EQ -> img
            GT -> let newH = h - (h `mod` y)
                      newW = x * newH `div` y
                  in applyProcess (cropToSize (newW,newH)) img
            LT -> let newW = w - (w `mod` x)
                      newH = y * newW `div` x
                  in applyProcess (cropToSize (newW,newH)) img
    )

-- | Crops an image to the given aspect ratio.
--
-- Aspect ratio should be given in terms of width relative to 1 height,
-- i.e. the value for width in width:height when height is 1.
--
-- Due to floating-point shenanigans, the output is not guaranteed to be exactly right.
--
-- May throw an error if given an invalid aspect ratio.
cropToAspectRatio' :: Double -> ArrayProcess a a
cropToAspectRatio' relW = ArrayProcess (\img ->
        let (M.Sz2 h w) = M.size img
            h' = fromIntegral h
            w' = fromIntegral w
        in case compare (w'/h') relW of
            EQ -> img
            GT -> let maxW = fromInteger $ floor (h' * relW)
                      newW = round $ maxW - (maxW `mod'` relW)
                      newH = round $ fromIntegral newW / relW
                  in applyProcess (cropToSize (newW,newH)) img
            LT -> let newW = round $ w' - (w' `mod'` relW)
                      newH = round $ fromIntegral newW / relW
                  in applyProcess (cropToSize (newW,newH)) img
    )
