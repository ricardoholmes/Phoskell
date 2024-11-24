module Graphics.Image.Convolution (
    convolution,
    meanFilter,
    gaussianFilter,
) where

import Graphics.Image
import Graphics.Image.ImageProcess
import Data.Massiv.Array

convolution :: Floating a => Stencil Ix2 a a -> MiscProcess a a
convolution stencil = MiscProcess convolve
        where
            convolve = dropWindow
                     . applyStencil padding stencil
                     . computeAs BL
            padding = samePadding stencil Continue

-- | Box blur, the kernel will always be square
meanFilter :: Floating a => Int -> MiscProcess a a
meanFilter n = convolution $ makeConvolutionStencilFromKernel kernel
    where
        area = fromIntegral (n*n)
        kernel = computeAs BL $ makeArrayR D Seq (Sz2 n n) (const (1/area))

-- | Kernel will always be square
gaussianFilter :: Floating a => Int -> a -> MiscProcess a a
gaussianFilter n sigma = convolution $ makeConvolutionStencilFromKernel kernel
    where
        r = n `div` 2 -- radius
        a = 1/(2*pi*sigma*sigma)
        kernel = computeAs BL $ makeArrayR D Seq (Sz2 n n) (\(Ix2 y x) ->
            let y' = fromIntegral $ y - r
                x' = fromIntegral $ x - r
            in a * exp (- ((x' * x' + y' * y') / (2 * sigma * sigma))))
