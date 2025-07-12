{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Convolution processes.
module Graphics.Phoskell.Processes.Convolution (
    convolution,
    convolution',
    convolutionWithKernel,
    meanFilter,
    gaussianFilter,
    gaussianFilter',
    medianFilter,
) where

import Data.List (insert)
import Data.Massiv.Array hiding ((:>))
import Data.Massiv.Array.Unsafe

import Graphics.Phoskell.Core.Pixel
import Graphics.Phoskell.Core.Image
import Graphics.Phoskell.Core.Colour

-- | Create centered padding with border type given and of the same size as stencil given.
--
-- Mostly equivalent to @samePadding@, but ignores the stencil's predefined center and instead
-- calculates the padding center based on the stencil's size.
derivePadding :: Stencil Ix2 e1 a -> Border e2 -> Padding Ix2 e2
derivePadding stencil border =
        Padding
            { paddingFromOrigin = sCenter
            , paddingFromBottom = liftSz2 (-) sSz (liftSz (+ 1) sCenter)
            , paddingWithElement = border
            }
    where
        sSz = getStencilSize stencil
        sCenter = liftSz (`div` 2) sSz
{-# INLINE derivePadding #-}

-- | Given a convolution stencil, apply convolution to the image.
convolution :: Pixel p => Stencil Ix2 (p Double) (p Double) -> ArrayProcess (p Word8) (p Word8)
convolution stencil = ArrayProcess (toPixelWord8 . convolve . toPixelDouble)
    where
        toPixelDouble = fmap (fmap fromIntegral)
        {-# INLINE toPixelDouble #-}
        toPixelWord8 = fmap (fmap (floor . max 0 . min 255))
        {-# INLINE toPixelWord8 #-}
        convolve = dropWindow
                    . applyStencil padding stencil
                    . computeAs U
        {-# INLINE convolve #-}
        padding = derivePadding stencil Continue
        {-# INLINE padding #-}
{-# INLINE convolution #-}

-- | Given a convolution stencil, apply convolution to the image.
--
-- This is different to 'convolution' in that it's for images holding Double values.
convolution' :: Pixel p => Stencil Ix2 (p Double) (p Double) -> ArrayProcess (p Double) (p Double)
convolution' stencil = ArrayProcess convolve
    where
        convolve = dropWindow
                    . applyStencil padding stencil
                    . computeAs U
        {-# INLINE convolve #-}
        padding = derivePadding stencil Continue
        {-# INLINE padding #-}
{-# INLINE convolution' #-}

-- | Apply convolution using the given kernel.
convolutionWithKernel :: Pixel p => [[p Double]] -> ArrayProcess (p Word8) (p Word8)
convolutionWithKernel (k :: [[p Double]]) = convolution $ makeConvolutionStencilFromKernel k'
    where
        k' :: Array U Ix2 (p Double)
        k' = fromLists' Par k
        {-# INLINE k' #-}
{-# INLINE convolutionWithKernel #-}

-- | Box blur given side length.
--
-- The dimensions of the filter will be @(n, n)@ in terms of @(width, height)@.
meanFilter :: Pixel p => Int -- ^ Side length @n@ of the filter.
                      -> ArrayProcess (p Word8) (p Word8) -- ^ Mean filter process.
meanFilter n = convolution stencil
    where
        sz = Sz2 n n
        {-# INLINE sz #-}
        stencil = avgStencil sz
        {-# INLINE stencil #-}
{-# INLINE meanFilter #-}

-- | Gaussian blur with a square kernel.
--
-- The overall area of the filter will be @(2n+1, 2n+1)@ in terms of @(width, height)@.
gaussianFilter :: Pixel p => Int -- ^ Side length @n@ of the filter.
                          -> Double -- ^ Standard deviation.
                          -> ArrayProcess (p Word8) (p Word8) -- ^ Gaussian filter.
gaussianFilter n !s = ArrayProcess (
            fmap (fmap (floor . max 0 . min 255))
            . dropWindow
            . applyStencil noPadding stencilY -- padding already dealt with
            . computeAs U
            . applyStencil padding stencilX
            . computeAs U
            . fmap (fmap fromIntegral)
        )
    where
        !r = fromIntegral n / 2 -- radius
        gauss1D x = exp (- (((x / s) ^ (2 :: Int)) / 2)) / (s * sqrt (2 * pi))
        {-# INLINE gauss1D #-}
        kernelVecX = makeArrayR U Par (Sz2 1 n) (\(_:.x) ->
                let x' = fromIntegral x - r
                in pure (gauss1D x')
            )
        stencilX = makeCorrelationStencilFromKernel kernelVecX
        stencilY = unsafeTransformStencil szT idxT stencilFunc stencilX
        stencilFunc f unsafeGetVal getVal = f (unsafeGetVal . idxT) (getVal . idxT) . idxT
        {-# INLINE stencilFunc #-}
        szT (Sz2 h w) = Sz2 w h
        {-# INLINE szT #-}
        idxT (y:.x) = x:.y
        {-# INLINE idxT #-}
        !r' = n `div` 2
        padding = Padding (Sz2 r' r') (Sz2 r' r') Continue
        {-# INLINE padding #-}
{-# INLINE gaussianFilter #-}

-- | Gaussian blur with a square kernel and default standard deviation.
--
-- Standard deviation is calculated @n/6@, where @n@ is the filter's side length.
--
-- The overall area of the filter will be @(2n+1, 2n+1)@ in terms of @(width, height)@.
gaussianFilter' :: Pixel p => Int -- ^ Side length @n@ of the filter.
                           -> ArrayProcess (p Word8) (p Word8) -- ^ Gaussian filter.
gaussianFilter' n = gaussianFilter n (fromIntegral n / 6)
{-# INLINE gaussianFilter' #-}

-- | Apply a median filter with the radius given.
--
-- The dimensions of the filter created will be @(2*r+1, 2*r+1)@ in terms of @(width, height)@.
medianFilter :: (Pixel p, Ord a, Unbox a) => Int -- ^ Radius @r@ of the filter.
                                          -> ArrayProcess (p a) (p a) -- ^ Median filter.
medianFilter r = ArrayProcess applyFilter
    where
        applyFilter = delay
                    . computeAs U
                    . applyStencil padding stencil
                    . computeAs U
        {-# INLINE applyFilter #-}
        padding = derivePadding stencil Continue
        {-# INLINE padding #-}
        stencil = fmap median <$> foldlStencil (\a e -> insert <$> e <*> a) (pure []) sz
        median xs = xs !! (2*r*r + 2*r) -- = ((2*r + 1)*(2*r + 1)) `div` 2
        {-# INLINE median #-}
        sz = Sz2 (2*r+1) (2*r+1)
        {-# INLINE sz #-}
{-# INLINE medianFilter #-}
