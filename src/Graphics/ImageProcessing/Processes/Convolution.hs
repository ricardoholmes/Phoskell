module Graphics.ImageProcessing.Processes.Convolution (
    convolution,
    meanFilter,
    gaussianFilter,
) where

import Graphics.ImageProcessing.Core.Pixel ( Pixel )
import Graphics.ImageProcessing.Processes ( MiscProcess(..) )
import Data.Massiv.Array
import Data.Word (Word8)

convolution :: Pixel p => Stencil Ix2 (p Double) (p Double) -> MiscProcess (p Word8) (p Word8)
convolution stencil = MiscProcess (fmap (fmap floor) . convolve . fmap (fmap fromIntegral))
        where
            convolve = dropWindow
                     . applyStencil padding stencil
                     . computeAs BL
            padding = samePadding stencil Continue

-- | Box blur, the kernel will always be square
meanFilter :: Pixel p => Int -> MiscProcess (p Word8) (p Word8)
meanFilter n = convolution $ makeConvolutionStencilFromKernel kernel
    where
        area = fromIntegral (n*n)
        kernel = computeAs BL $ makeArrayR D Par (Sz2 n n) (const (1/area))
{-# INLINE meanFilter #-}

-- | Kernel will always be square
gaussianFilter :: Pixel p => Int -> Double -> MiscProcess (p Word8) (p Word8)
gaussianFilter n sigma = convolution $ makeConvolutionStencilFromKernel kernel
    where
        r = n `div` 2 -- radius
        s = realToFrac sigma
        a = 1/(2*pi*s*s)
        kernel = computeAs BL $ makeArrayR D Par (Sz2 n n) (\(Ix2 y x) ->
            let y' = fromIntegral $ y - r
                x' = fromIntegral $ x - r
            in a * exp (- ((x' * x' + y' * y') / (2 * s * s))))
{-# INLINE gaussianFilter #-}
