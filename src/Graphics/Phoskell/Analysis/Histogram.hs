{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Histogram generation functions.
module Graphics.Phoskell.Analysis.Histogram (
    Histogram,
    Histogrammable(..),
    arrayHistogram,
    histogram1,
    histogram2,
    histogram3,
    histogram4,
) where

import Graphics.Phoskell.Core
import qualified Data.Massiv.Array as M
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (modify)

-- | Type alias defining return type of histogram calculating functions.
type Histogram = [Int]

-- | "Pixel" types which support histograms.
class Histogrammable p where
    -- | General histogram function.
    histogram :: Image p -> [Histogram]

-- | Histogram for an image's underlying array type.
--
-- Adapted from [Haskell Image Processing (HIP)](https://hackage.haskell.org/package/hip).
arrayHistogram :: ImageArray Word8 -> Histogram
arrayHistogram img = V.toList $ V.modify countBins (V.replicate 256 0)
    where
        incrementBin v x = modify v (+1) $ fromIntegral x
        countBins v = M.mapM_ (incrementBin v) img

-- | Histogram for single channel image.
histogram1 :: Image (Pixel1 Word8) -> Histogram
histogram1 img = hist
    where
        img' = toArray img
        hist = arrayHistogram (M.map (\(Pixel1 p) -> p) img')
{-# INLINE histogram1 #-}

instance Histogrammable (Pixel1 Word8) where
    histogram :: Image (Pixel1 Word8) -> [Histogram]
    histogram = (:[]) . histogram1
    {-# INLINE histogram #-}

-- | Histogram for 2-channel image.
histogram2 :: Image (Pixel2 Word8) -> (Histogram, Histogram)
histogram2 img = (hist1, hist2)
    where
        img' = toArray img
        hist1 = arrayHistogram (M.map (\(Pixel2 p _) -> p) img')
        hist2 = arrayHistogram (M.map (\(Pixel2 _ p) -> p) img')
{-# INLINE histogram2 #-}

instance Histogrammable (Pixel2 Word8) where
    histogram :: Image (Pixel2 Word8) -> [Histogram]
    histogram img = [c1,c2]
        where (c1,c2) = histogram2 img
    {-# INLINE histogram #-}

-- | Histogram for 3-channel image.
histogram3 :: Image (Pixel3 Word8) -> (Histogram, Histogram, Histogram)
histogram3 img = (hist1, hist2, hist3)
    where
        img' = toArray img
        hist1 = arrayHistogram (M.map (\(Pixel3 p _ _) -> p) img')
        hist2 = arrayHistogram (M.map (\(Pixel3 _ p _) -> p) img')
        hist3 = arrayHistogram (M.map (\(Pixel3 _ _ p) -> p) img')
{-# INLINE histogram3 #-}

instance Histogrammable (Pixel3 Word8) where
    histogram :: Image (Pixel3 Word8) -> [Histogram]
    histogram img = [c1,c2,c3]
        where (c1,c2,c3) = histogram3 img
    {-# INLINE histogram #-}


-- | Histogram for 4-channel image.
histogram4 :: Image (Pixel4 Word8) -> (Histogram, Histogram, Histogram, Histogram)
histogram4 img = (hist1, hist2, hist3, hist4)
    where
        img' = toArray img
        hist1 = arrayHistogram (M.map (\(Pixel4 p _ _ _) -> p) img')
        hist2 = arrayHistogram (M.map (\(Pixel4 _ p _ _) -> p) img')
        hist3 = arrayHistogram (M.map (\(Pixel4 _ _ p _) -> p) img')
        hist4 = arrayHistogram (M.map (\(Pixel4 _ _ _ p) -> p) img')
{-# INLINE histogram4 #-}

instance Histogrammable (Pixel4 Word8) where
    histogram :: Image (Pixel4 Word8) -> [Histogram]
    histogram img = [c1,c2,c3,c4]
        where (c1,c2,c3,c4) = histogram4 img
    {-# INLINE histogram #-}

