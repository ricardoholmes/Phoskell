{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-} -- for == False in hysteresis

-- | Convolution processes.
module Graphics.Phoskell.Processes.Convolution (
    convolution,
    convolution',
    convolutionWithKernel,
    meanFilter,
    gaussianFilter,
    gaussianFilter',
    medianFilter,
    robertsCrossX,
    robertsCrossY,
    robertsCross,
    prewittX,
    prewittY,
    prewitt,
    sobelFilterX,
    sobelFilterY,
    sobelFilter,
    canny
) where

import Data.List (insert)
import Data.Massiv.Array hiding ((:>))
import Data.Massiv.Array.Unsafe
import System.IO.Unsafe

import Graphics.Phoskell.Core.Pixel ( Pixel, Pixel1 (..) )
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

-- | Apply convolution using the horizontal Roberts cross operator.
robertsCrossX :: Pixel p => ArrayProcess (p Word8) (p Word8)
robertsCrossX = convolution $ makeUnsafeConvolutionStencil (Sz2 2 2) (0 :. 0) stencilF
    where
        stencilF f = f (0 :. 0) 1 . f (1 :. 1) (-1)
        {-# INLINE stencilF #-}
{-# INLINE robertsCrossX #-}

-- | Apply convolution using the vertical Roberts cross operator.
robertsCrossY :: Pixel p => ArrayProcess (p Word8) (p Word8)
robertsCrossY = convolution $ makeUnsafeConvolutionStencil (Sz2 2 2) (0 :. 0) stencilF
    where
        stencilF f = f (0 :. 1) 1 . f (1 :. 0) (-1)
        {-# INLINE stencilF #-}
{-# INLINE robertsCrossY #-}

-- | Apply the Roberts cross operator.
robertsCross :: Pixel p => ArrayProcess (p Word8) (p Word8)
robertsCross = ArrayProcess (\arr ->
            let img = BaseImage arr :> fmap ((/255) . fromIntegral)
                dxImg = img :> convolution' convH :> fmap (^(2 :: Int))
                dyImg = img :> convolution' convV :> fmap (^(2 :: Int))
                img' = (dxImg + dyImg) :> fmap (min 1 . max 0 . sqrt)
            in toArray $ img' :> fmap (floor . (*255))
        )
    where
        convH = makeUnsafeConvolutionStencil (Sz2 2 2) (0 :. 0) stencilH
        {-# INLINE convH #-}
        convV = makeUnsafeConvolutionStencil (Sz2 2 2) (0 :. 0) stencilV
        {-# INLINE convV #-}
        stencilH f = f (0 :. 0) 1 . f (1 :. 1) (-1)
        {-# INLINE stencilH #-}
        stencilV f = f (0 :. 1) 1 . f (1 :. 0) (-1)
        {-# INLINE stencilV #-}
{-# INLINE robertsCross #-}

-- | Apply convolution using the horizontal Prewitt operator.
prewittX :: Pixel p => ArrayProcess (p Word8) (p Word8)
prewittX = convolution $ makeUnsafeConvolutionStencil (Sz2 3 3) (0 :. 0) stencilF
    where
        stencilF f = f ((-1) :. -1) (-1) . f ((-1) :. 1) 1 .
                     f (  0  :. -1) (-1) . f (  0  :. 1) 1 .
                     f (  1  :. -1) (-1) . f (  1  :. 1) 1
        {-# INLINE stencilF #-}
{-# INLINE prewittX #-}

-- | Apply convolution using the vertical Prewitt operator.
prewittY :: Pixel p => ArrayProcess (p Word8) (p Word8)
prewittY = convolution $ makeUnsafeConvolutionStencil (Sz2 3 3) (0 :. 0) stencilF
    where
        stencilF f = f ((-1) :. -1) 1 . f (1 :. -1) (-1) .
                     f ((-1) :.  0) 1 . f (1 :.  0) (-1) .
                     f ((-1) :.  1) 1 . f (1 :.  1) (-1)
        {-# INLINE stencilF #-}
{-# INLINE prewittY #-}

-- | Apply the Prewitt operator.
prewitt :: Pixel p => ArrayProcess (p Word8) (p Word8)
prewitt = ArrayProcess (\arr ->
            let img = BaseImage arr :> fmap ((/255) . fromIntegral)
                dxImg = img :> convolution' convH :> fmap (^(2 :: Int))
                dyImg = img :> convolution' convV :> fmap (^(2 :: Int))
                img' = (dxImg + dyImg) :> fmap (min 1 . max 0 . sqrt)
            in toArray $ img' :> fmap (floor . (*255))
        )
    where
        convH = makeUnsafeConvolutionStencil (Sz2 3 3) (0 :. 0) stencilH
        {-# INLINE convH #-}
        convV = makeUnsafeConvolutionStencil (Sz2 3 3) (0 :. 0) stencilV
        {-# INLINE convV #-}
        stencilH f = f ((-1) :. -1) (-1) . f ((-1) :. 1) 1 .
                     f (  0  :. -1) (-1) . f (  0  :. 1) 1 .
                     f (  1  :. -1) (-1) . f (  1  :. 1) 1
        {-# INLINE stencilH #-}
        stencilV f = f ((-1) :. -1) 1 . f (1 :. -1) (-1) .
                     f ((-1) :.  0) 1 . f (1 :.  0) (-1) .
                     f ((-1) :.  1) 1 . f (1 :.  1) (-1)
        {-# INLINE stencilV #-}
{-# INLINE prewitt #-}

-- | Apply convolution using the horizontal sobel filter.
sobelFilterX :: Pixel p => ArrayProcess (p Word8) (p Word8)
sobelFilterX = convolution $ makeUnsafeConvolutionStencil (Sz2 3 3) (0 :. 0) stencilF
    where
        stencilF f = f ((-1) :. -1) (-1) . f ((-1) :. 1) 1 .
                     f (  0  :. -1) (-2) . f (  0  :. 1) 2 .
                     f (  1  :. -1) (-1) . f (  1  :. 1) 1
        {-# INLINE stencilF #-}
{-# INLINE sobelFilterX #-}

-- | Apply convolution using the vertical sobel filter.
sobelFilterY :: Pixel p => ArrayProcess (p Word8) (p Word8)
sobelFilterY = convolution $ makeUnsafeConvolutionStencil (Sz2 3 3) (0 :. 0) stencilF
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
        convH = makeUnsafeConvolutionStencil (Sz2 3 3) (0 :. 0) stencilH
        {-# INLINE convH #-}
        convV = makeUnsafeConvolutionStencil (Sz2 3 3) (0 :. 0) stencilV
        {-# INLINE convV #-}
        stencilH f = f ((-1) :. -1) (-1) . f ((-1) :. 1) 1 .
                     f (  0  :. -1) (-2) . f (  0  :. 1) 2 .
                     f (  1  :. -1) (-1) . f (  1  :. 1) 1
        {-# INLINE stencilH #-}
        stencilV f = f ((-1) :. -1) (-1) . f (1 :. -1) 1 .
                     f ((-1) :.  0) (-2) . f (1 :.  0) 2 .
                     f ((-1) :.  1) (-1) . f (1 :.  1) 1
        {-# INLINE stencilV #-}
{-# INLINE sobelFilter #-}

-- | Apply canny edge detection, given low and high thresholds.
canny :: Double -> Double -> ArrayProcess Grey Binary
canny tLow tHigh = ArrayProcess (\arr ->
            let img = BaseImage arr :> gaussianFilter 5 1.4
                                    :> fmap (\px -> fromIntegral px / 255)
                dxImg = img :> convolution' convH :> (\(Pixel1 x) -> x)
                dyImg = img :> convolution' convV :> (\(Pixel1 x) -> x)
                magOrient = toArrayUnboxed $ magnitudeOrientation <$> dxImg <*> dyImg
                suppressed = compute $ suppress magOrient
            in Pixel1 <$> delay (hysteresis suppressed)
            -- in Pixel1 . floor . (*255) . max 0 . min 255 . fst <$> delay magOrient
        )
    where
        convH = makeUnsafeConvolutionStencil (Sz2 3 3) (0 :. 0) stencilH
        {-# INLINE convH #-}
        convV = makeUnsafeConvolutionStencil (Sz2 3 3) (0 :. 0) stencilV
        {-# INLINE convV #-}
        stencilH f = f ((-1) :. -1) (-1) . f ((-1) :. 1) 1 .
                     f (  0  :. -1) (-2) . f (  0  :. 1) 2 .
                     f (  1  :. -1) (-1) . f (  1  :. 1) 1
        {-# INLINE stencilH #-}
        stencilV f = f ((-1) :. -1) (-1) . f (1 :. -1) 1 .
                     f ((-1) :.  0) (-2) . f (1 :.  0) 2 .
                     f ((-1) :.  1) (-1) . f (1 :.  1) 1
        {-# INLINE stencilV #-}
        -- grad imgA imgB = (imgA :> (^(2 :: Int)) + imgB :> (^(2 :: Int))) :> sqrt
        -- {-# INLINE grad #-}
        -- dir imgY imgX = (imgY :> atan2) <*> imgX
        -- {-# INLINE dir #-}
        -- orientation imgY imgX = dir imgY imgX
        -- {-# INLINE orientation #-}
        magnitudeOrientation :: Double -> Double -> (Double, Int)
        magnitudeOrientation x y =
            let mag = sqrt (x*x + y*y)
                orientation
                    | x >= -tLow, x < tLow, y >= -tLow, y < tLow = 0 -- none
                    | otherwise =
                        let !d = atan2 y x
                            !dRot = 4 * d / pi - 0.5
                            !dNorm = if dRot < 0 then dRot + 4 else dRot
                        in if dNorm >= 2
                            then if dNorm >= 3
                                then 1 -- horizontal
                                else 4 -- diagonal NW-SE
                            else if dNorm >= 1
                                then 3 -- vertical
                                else 2 -- diagonal SW-NE
            in (mag, orientation)
        {-# INLINE magnitudeOrientation #-}
        suppress = dropWindow . mapStencil (Fill (0, 0)) (makeUnsafeStencil 3 1 comparePts)
        {-# INLINE suppress #-}
        comparePts _ getMag
            | o == 0 = 0 -- no edge
            | o == 1 = isMax (getMag (0 :. -1)) (getMag (0 :. 1))
            | o == 2 = isMax (getMag ((-1) :. 0)) (getMag (1 :. 0))
            | o == 3 = isMax (getMag ((-1) :. 1)) (getMag (1 :. -1))
            | o == 4 = isMax (getMag ((-1) :. -1)) (getMag (1 :. 1))
            | otherwise = 0 -- no edge
                    where
                        (!m, !o) = getMag (0 :. 0)
                        {-# INLINE isMax #-}
                        isMax intensity1 intensity2
                            | m < tLow = 0
                            | m < fst intensity1 = 0
                            | m < fst intensity2 = 0
                            | m < tHigh = 128
                            | otherwise = 255
        {-# INLINE comparePts #-}
{-# INLINE canny #-}

-- | Select indices of strong edges.
--
-- Taken from the 'hip': (https://github.com/lehins/hip)
selectStrong :: Array U Ix2 Word8 -> Array S Ix1 Ix1
selectStrong = compute
             . simapMaybe
                    (\ !ix !e ->
                        if e == 255 
                            then Just ix
                            else Nothing)
             . flatten
{-# INLINE selectStrong #-}

-- | Apply thresholding with hysteresis.
--
-- Taken from the library 'hip' (https://github.com/lehins/hip)
hysteresis :: Array U Ix2 Word8 -- ^ Image with strong and weak edges set.
            -> Array U Ix2 Bool
hysteresis arr = unsafePerformIO $ do
        let !strong = selectStrong arr
            !szStrong = size strong
        vStack <- unsafeNew (Sz lenImg)
        unsafeArrayLinearCopy strong 0 vStack 0 szStrong
        edges <- newMArray sz False
        burn edges vStack (unSz szStrong)
        unsafeFreeze (getComp arr) edges
        where
            !sz = size arr
            !lenImg = totalElem sz
            burn !edges !vStack = go
                where
                    push :: Ix2 -> Int -> IO Int
                    push !ix !top =
                        case indexM arr ix of
                            Nothing -> pure top
                            Just src -> do
                                dst <- unsafeRead edges ix
                                if dst == False && src == 128
                                    -- Rescue the weak edge and push onto the stack
                                    then (top + 1) <$ unsafeWrite vStack top (toLinearIndex sz ix)
                                    else pure top
                    {-# INLINE push #-}
                    go !top
                        | top == 0 = return ()
                        | otherwise = do
                            let !top' = top - 1
                            -- Pop the first strong edge off the stack, look at all its neighbours, and
                            -- if any of these neighbors is a weak edge we rescue it by labelling it as
                            -- strong and adding it to the stack
                            i <- unsafeLinearRead vStack top'
                            let (y :. x) = fromLinearIndex sz i
                            unsafeLinearWrite edges i True
                            push   (y - 1 :. x - 1) top' >>=
                              push (y - 1 :. x    ) >>=
                              push (y - 1 :. x + 1) >>=
                              push (y     :. x - 1) >>=
                              push (y     :. x + 1) >>=
                              push (y + 1 :. x - 1) >>=
                              push (y + 1 :. x    ) >>=
                              push (y + 1 :. x + 1) >>=
                              go
{-# INLINE hysteresis #-}
