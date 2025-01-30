{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.ImageProcessing.Analysis.Histogram (
    Histogrammable(..),
    histogram1,
    histogram2,
    histogram3,
    histogram4,
) where

import Graphics.ImageProcessing.Core
import Data.Massiv.Array ( flatten, toList )
import Data.Word (Word8)

class Pixel p => Histogrammable p where
    histogram :: Image (p Word8) -> [[Int]]

-- | Histogram for single channel image
histogram1 :: Image (Pixel1 Word8) -> [Int]
histogram1 img = bins
    where
        dropGray (Pixel1 x) = fromIntegral x
        imgList = toList $ flatten $ toArray (dropGray <$> img)
        bins = count (replicate 256 0) imgList

instance Histogrammable Pixel1 where
    histogram :: Image (Pixel1 Word8) -> [[Int]]
    histogram = (:[]) . histogram1

-- | Histogram for 2-channel image
histogram2 :: Image (Pixel2 Word8) -> ([Int],[Int])
histogram2 img = (bins1, bins2)
    where
        takeCh1 (Pixel2 x _) = fromIntegral x
        takeCh2 (Pixel2 _ x) = fromIntegral x
        imgList1 = toList $ flatten $ toArray (takeCh1 <$> img)
        imgList2 = toList $ flatten $ toArray (takeCh2 <$> img)
        bins1 = count (replicate 256 0) imgList1
        bins2 = count (replicate 256 0) imgList2

instance Histogrammable Pixel2 where
    histogram :: Image (Pixel2 Word8) -> [[Int]]
    histogram img = [c1,c2]
        where (c1,c2) = histogram2 img

-- | Histogram for 3-channel image
histogram3 :: Image (Pixel3 Word8) -> ([Int],[Int],[Int])
histogram3 img = (bins1, bins2, bins3)
    where
        takeCh1 (Pixel3 x _ _) = fromIntegral x
        takeCh2 (Pixel3 _ x _) = fromIntegral x
        takeCh3 (Pixel3 _ _ x) = fromIntegral x
        imgList1 = toList $ flatten $ toArray (takeCh1 <$> img)
        imgList2 = toList $ flatten $ toArray (takeCh2 <$> img)
        imgList3 = toList $ flatten $ toArray (takeCh3 <$> img)
        bins1 = count (replicate 256 0) imgList1
        bins2 = count (replicate 256 0) imgList2
        bins3 = count (replicate 256 0) imgList3

instance Histogrammable Pixel3 where
    histogram :: Image (Pixel3 Word8) -> [[Int]]
    histogram img = [c1,c2,c3]
        where (c1,c2,c3) = histogram3 img

-- | Histogram for 4-channel image
histogram4 :: Image (Pixel4 Word8) -> ([Int],[Int],[Int],[Int])
histogram4 img = (bins1, bins2, bins3, bins4)
    where
        takeCh1 (Pixel4 x _ _ _) = fromIntegral x
        takeCh2 (Pixel4 _ x _ _) = fromIntegral x
        takeCh3 (Pixel4 _ _ x _) = fromIntegral x
        takeCh4 (Pixel4 _ _ _ x) = fromIntegral x
        imgList1 = toList $ flatten $ toArray (takeCh1 <$> img)
        imgList2 = toList $ flatten $ toArray (takeCh2 <$> img)
        imgList3 = toList $ flatten $ toArray (takeCh3 <$> img)
        imgList4 = toList $ flatten $ toArray (takeCh4 <$> img)
        bins1 = count (replicate 256 0) imgList1
        bins2 = count (replicate 256 0) imgList2
        bins3 = count (replicate 256 0) imgList3
        bins4 = count (replicate 256 0) imgList4

instance Histogrammable Pixel4 where
    histogram :: Image (Pixel4 Word8) -> [[Int]]
    histogram img = [c1,c2,c3,c4]
        where (c1,c2,c3,c4) = histogram4 img

count :: [Int] -> [Int] -> [Int]
count xs [] = xs
count xs (p:ps) = count xs' ps
    where
        (l,x:r) = splitAt p xs
        xs' = l ++ (x+1:r)
