{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
-- | Convolution processes.
module Graphics.Phoskell.Processes.Convolution (
    convolution,
    convolution',
    convolutionWithKernel,
    meanFilter,
    gaussianFilter,
    sobelFilterX,
    sobelFilterY,
    sobelFilter,
    medianFilter,
) where

import Data.List (insert)
import Data.Massiv.Array hiding ((:>))
import Data.Word (Word8)

import Graphics.Phoskell.Core.Pixel ( Pixel )
import Graphics.Phoskell.Core.Image

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
gaussianFilter n sigma = convolution $ makeConvolutionStencilFromKernel kernel
    where
        !r = n `div` 2 -- radius
        !s = realToFrac sigma
        !a = 1/(2*pi*s*s)
        kernel = makeArrayR U Par (Sz2 n n) (\(Ix2 y x) ->
            let y' = fromIntegral $ y - r
                x' = fromIntegral $ x - r
            in a * exp (- ((x' * x' + y' * y') / (2 * s * s))))
        {-# INLINE kernel #-}
{-# INLINE gaussianFilter #-}

-- | Apply convolution using the horizontal sobel filter.
sobelFilterX :: Pixel p => ArrayProcess (p Word8) (p Word8)
sobelFilterX = convolution $ makeConvolutionStencil (Sz2 3 3) (1 :. 1) stencilF
    where
        stencilF f = f ((-1) :. -1) (-1) . f ((-1) :. 1) 1 .
                     f (  0  :. -1) (-2) . f (  0  :. 1) 2 .
                     f (  1  :. -1) (-1) . f (  1  :. 1) 1
        {-# INLINE stencilF #-}
{-# INLINE sobelFilterX #-}

-- | Apply convolution using the vertical sobel filter.
sobelFilterY :: Pixel p => ArrayProcess (p Word8) (p Word8)
sobelFilterY = convolution $ makeConvolutionStencil (Sz2 3 3) (1 :. 1) stencilF
    where
        stencilF f = f ((-1) :. -1) (-1) . f (1 :. -1) 1 .
                     f ((-1) :.  0) (-2) . f (1 :.  0) 2 .
                     f ((-1) :.  1) (-1) . f (1 :.  1) 1
        {-# INLINE stencilF #-}
{-# INLINE sobelFilterY #-}

-- | Apply the sobel filter.
sobelFilter :: Pixel p => ArrayProcess (p Word8) (p Word8)
sobelFilter = ArrayProcess (\arr ->
            let img = BaseImage arr :> fmap ((/255) . fromIntegral)
                dxImg = img :> convolution' convH :> fmap (^(2 :: Int))
                dyImg = img :> convolution' convV :> fmap (^(2 :: Int))
                img' = (dxImg + dyImg) :> fmap (min 1 . max 0 . sqrt)
            in toArray $ img' :> fmap (floor . (*255))
        )
    where
        convH = makeConvolutionStencil (Sz2 3 3) (1 :. 1) stencilH
        {-# INLINE convH #-}
        convV = makeConvolutionStencil (Sz2 3 3) (1 :. 1) stencilV
        {-# INLINE convV #-}
        stencilH f = f ((-1) :. -1) (-1) . f ((-1) :. 1) 1 .
                     f (  0  :. -1) (-2) . f (  0  :. 1) 2 .
                     f (  1  :. -1) (-1) . f (  1  :. 1) 1
        {-# INLINE stencilH #-}
        stencilV f = f ((-1) :. -1) (-1) . f (1 :. -1) 1 .
                     f ((-1) :.  0) (-2) . f (1 :.  0) 2 .
                     f ((-1) :.  1) (-1) . f (1 :.  1) 1
        {-# INLINE stencilV #-}

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
