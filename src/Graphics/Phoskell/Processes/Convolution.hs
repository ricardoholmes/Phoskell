module Graphics.Phoskell.Processes.Convolution (
    convolution,
    meanFilter,
    gaussianFilter,
) where

import Graphics.Phoskell.Core.Pixel ( Pixel )
import Graphics.Phoskell.Core.Image ( ArrayProcess(..) )
import Data.Massiv.Array
import Data.Word (Word8)

-- | Given a convolution stencil, apply convolution to the image.
--
-- Parameters:
-- - Convolution stencil to use.
convolution :: Pixel p => Stencil Ix2 (p Double) (p Double) -> ArrayProcess (p Word8) (p Word8)
convolution stencil = ArrayProcess (fmap (fmap floor) . convolve . fmap (fmap fromIntegral))
        where
            convolve = dropWindow
                     . applyStencil padding stencil
                     . computeAs BN
            padding = samePadding stencil Continue

-- | Box blur, the kernel will always be square
--
-- Parameters:
-- - Side length of box.
--
-- The overall area of the filter will be $(n, n)$ in terms of @(width, height)@.
meanFilter :: Pixel p => Int -> ArrayProcess (p Word8) (p Word8)
meanFilter n = convolution $ makeConvolutionStencilFromKernel kernel
    where
        area = fromIntegral (n*n)
        kernel = makeArrayR U Par (Sz2 n n) (const (1/area))
{-# INLINE meanFilter #-}

-- | Gaussian blur with a square kernel
--
-- Parameters:
-- - Box radius radius, $n$.
--
-- The overall area of the filter will be $(2n+1, 2n+1)$ in terms of @(width, height)@.
gaussianFilter :: Pixel p => Int -> Double -> ArrayProcess (p Word8) (p Word8)
gaussianFilter n sigma = convolution $ makeConvolutionStencilFromKernel kernel
    where
        r = n `div` 2 -- radius
        s = realToFrac sigma
        a = 1/(2*pi*s*s)
        kernel = makeArrayR U Par (Sz2 n n) (\(Ix2 y x) ->
            let y' = fromIntegral $ y - r
                x' = fromIntegral $ x - r
            in a * exp (- ((x' * x' + y' * y') / (2 * s * s))))
{-# INLINE gaussianFilter #-}
