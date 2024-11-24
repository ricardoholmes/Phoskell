module Graphics.Image.Convolution (
    convolution,
    meanFilter,
) where

import Graphics.Image
import Graphics.Image.ImageProcess
import Data.Massiv.Array (dropWindow, Border (..), computeAs, BL (BL))
import Data.Massiv.Array.Stencil (applyStencil, avgStencil, Stencil, samePadding)

convolution :: Fractional a => Stencil Ix2 a a -> MiscProcess a a
convolution stencil = MiscProcess convolve
        where
            convolve = dropWindow
                     . applyStencil padding stencil
                     . computeAs BL
            padding = samePadding stencil Continue

-- | Box blur, the kernel will always be square
meanFilter :: Fractional a => Int -> MiscProcess a a
meanFilter n = convolution $ avgStencil (Sz2 n n)
