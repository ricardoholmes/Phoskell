module Graphics.ImageProcessing.Synthesis.Graph (
    -- bar charts
    drawBarChart,
    drawBarChart',

    -- histogram functions
    drawHistogramSingle,
    drawHistogramsDualH,
    drawHistogramsDualV,
    drawHistogramsQuad,
    drawHistogramsRGBY,
) where

import Data.Massiv.Array (Ix2(..))
import Data.Word (Word8)

import qualified Data.Massiv.Array as M

import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Core.Image (PointProcess(PointProcess))
import Graphics.ImageProcessing.Core.Colour (rgbToGrey)
import Graphics.ImageProcessing.Analysis
import Graphics.ImageProcessing.Synthesis.Internal
import Graphics.ImageProcessing.Synthesis.Stack

-- | Generate a bar chart image.
--
-- Parameters:
-- - Size in form @(width, height)@.
-- - Background colour.
-- - Value and colour of each bar.
--
-- The bar values are scaled to calculate pixel height, s.t. the tallest bar is
-- as tall as the image itself.
--
-- Disclaimer: the bars will always all share the same size.
-- This may leave empty space to the right of the bars if they cannot be evenly
-- sized to fit within the desired width.
-- Also, if the number of bars is greater than the desired width, an error will be thrown.
drawBarChart :: Pixel p => (Int,Int) -> p Word8 -> [(Int, p Word8)] -> Image (p Word8)
drawBarChart (w,h) bg [] = canvas (w,h) bg
drawBarChart (w,h) bg bars = BaseImage $ M.makeArrayR M.D M.Par sz (\(y:.x) ->
            let barIdx = x `div` barWidth
                (height,colour) = barHeights !! barIdx
                y' = h - y -- invert to make bottom left the origin
            in if y' <= height then colour else bg
        )
    where
        sz = M.Sz2 h w
        barWidth = w `div` length bars
        highest = maximum (fst <$> bars)
        barHeights = fmap (\(b,c) ->
                                if highest == 0 -- avoid zero division
                                    then (0,c)
                                    else ((b * h) `div` highest,c)
                          ) bars
                    ++ repeat (0,0) -- leave rest of area empty

-- | Generate a bar chart image, with all bars having the same colour.
--
-- Parameters:
-- - Size in form @(width, height)@.
-- - Background colour.
-- - Colour of the bars.
-- - Value that each bar represents.
--
-- Equivalent to @drawBarChart@ but with the same colour for all bars.
drawBarChart' :: Pixel p => (Int,Int) -> p Word8 -> p Word8 -> [Int] -> Image (p Word8)
drawBarChart' sz bg fg bars = drawBarChart sz bg [(b,fg) | b <- bars]

-- | Draw a histogram for a single-channel image.
--
-- Parameters:
-- - Image to draw the histogram of.
-- - Size of the histogram in terms @(width,height)@.
-- - Background colour.
-- - Bar colour.
drawHistogramSingle :: Pixel p => Image (Pixel1 Word8)
                               -> (Int,Int) -> p Word8
                               -> p Word8 -> Image (p
                               Word8)
drawHistogramSingle img sz bg fg = drawBarChart' sz bg fg hist
    where hist = histogram1 img

-- | Draw histograms for a 2-channel image, stacked horizontally.
--
-- Parameters:
-- - Image to draw the histogram of.
-- - Size of histograms in terms @(width,height)@ (both histograms will share the same size).
-- - Background colour.
-- - Bar colour for first channel.
-- - Bar colour for second channel.
drawHistogramsDualH :: Pixel p => Image (Pixel2 Word8)
                               -> (Int,Int) -> p Word8
                               -> p Word8 -> p Word8
                               -> Image (p Word8)
drawHistogramsDualH img sz bg fg1 fg2 = stackHorizontally bg histImg1 histImg2
    where
        (hist1, hist2) = histogram2 img
        histImg1 = drawBarChart' sz bg fg1 hist1
        histImg2 = drawBarChart' sz bg fg2 hist2

-- | Draw histograms for a 2-channel image, stacked vertically.
--
-- Parameters:
-- - Image to draw the histogram of.
-- - Size of histograms in terms @(width,height)@ (both histograms will share the same size).
-- - Background colour.
-- - Bar colour for first channel.
-- - Bar colour for second channel.
drawHistogramsDualV :: Pixel p => Image (Pixel2 Word8)
                               -> (Int,Int) -> p Word8
                               -> p Word8 -> p Word8
                               -> Image (p Word8)
drawHistogramsDualV img sz bg fg1 fg2 = stackVertically bg histImg1 histImg2
    where
        (hist1, hist2) = histogram2 img
        histImg1 = drawBarChart' sz bg fg1 hist1
        histImg2 = drawBarChart' sz bg fg2 hist2

-- | Draw histograms for a 4-channel image, arranged into quadrants.
--
-- Parameters:
-- - Image to draw the histogram of.
-- - Size of histograms in terms @(width,height)@ (all histograms will share the same size).
-- - Background colour.
-- - Bar colour for first channel.
-- - Bar colour for second channel.
-- - Bar colour for third channel.
-- - Bar colour for fourth channel.
drawHistogramsQuad :: Pixel p => Image (Pixel4 Word8)
                              -> (Int,Int) -> p Word8
                              -> p Word8 -> p Word8 -> p Word8 -> p Word8
                              -> Image (p Word8)
drawHistogramsQuad img sz bg fg1 fg2 fg3 fg4 = quadrants bg h1 h2 h3 h4
    where
        (hist1,hist2,hist3,hist4) = histogram4 img
        h1 = drawBarChart' sz bg fg1 hist1
        h2 = drawBarChart' sz bg fg2 hist2
        h3 = drawBarChart' sz bg fg3 hist3
        h4 = drawBarChart' sz bg fg4 hist4

-- | Draw histograms for an RGB image arranged into quadrants, with the fourth
-- showing luma.
--
-- Parameters:
-- - Image to draw the histogram of.
-- - Size of each histogram in terms @(width,height)@ (all histograms will share the same size).
-- - Background colour.
-- - Bar colour for red channel.
-- - Bar colour for green channel.
-- - Bar colour for blue channel.
-- - Bar colour for luma.
drawHistogramsRGBY :: Pixel p => Image RGB
                              -> (Int,Int) -> p Word8
                              -> p Word8 -> p Word8 -> p Word8 -> p Word8
                              -> Image (p Word8)
drawHistogramsRGBY img = drawHistogramsQuad img'
    where
        img' = img :> PointProcess (\p@(Pixel3 r g b) -> Pixel4 r g b (getLuma p))
        getLuma = (\(Pixel1 p) -> p) . rgbToGrey
