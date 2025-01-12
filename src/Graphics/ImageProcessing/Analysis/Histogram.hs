{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Graphics.ImageProcessing.Analysis.Histogram (
    histogramGray
) where

import Graphics.ImageProcessing.Core ( Gray, toArray, Pixel1(..) )
import Graphics.ImageProcessing ( Image(..) )
import Data.Massiv.Array ( flatten, toList )

histogramGray :: Image Gray -> [Int]
histogramGray img = bins
    where
        dropGray (Pixel1 x) = fromIntegral x
        imgList = toList $ flatten $ toArray (dropGray <$> img)
        bins = count (replicate 256 0) imgList

count :: [Int] -> [Int] -> [Int]
count xs [] = xs
count xs (p:ps) = count xs' ps
    where
        (l,x:r) = splitAt p xs
        xs' = l ++ (x+1:r)
