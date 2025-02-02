module Graphics.ImageProcessing.Synthesis (
    canvas,
    generateImage,
    generateImage',
    simpleGradientH,
    simpleGradientV,
    multiColorGradientH,
    multiColorGradientV,
    drawBarChart,
    drawBarChart',
    stackVertically,
    stackHorizontally,
    quadrants,
) where

import Graphics.ImageProcessing.Core
import qualified Data.Massiv.Array as M
import Data.Word (Word8)
import Data.Massiv.Array (Ix2(..))
import Graphics.ImageProcessing.Analysis (imageSize)
import Graphics.ImageProcessing.Transformations (zoomToSize)

-- | Generate a blank image filled with a single value.
--
-- - First parameter is the size in terms @(width,height)@.
-- - Second parameter is the value to use to fill.
canvas :: (Int,Int) -> a -> Image a
canvas (w,h) x = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (const x)

-- | Generate an image from a function.
--
-- - First parameter is the top-left coordinates, i.e. @(min x, min y)@.
-- - Second parameter is the bottom-right coordinates, i.e. @(max x, max y)@.
-- - Third parameter is the function to use, taking @(x,y)@ and returning the pixel value.
-- 
-- Values not in the range [0..255] will be clamped to be in the range.
generateImage :: (Pixel p, Integral a, Integral b) => (Int,Int) -> (Int,Int) -> ((a,a) -> p b) -> Image (p Word8)
generateImage (xm,ym) (xM,yM) f = BaseImage $ M.makeArrayR M.D M.Par sz (\(y:.x) ->
                                    let y' = fromIntegral (y + ym)
                                        x' = fromIntegral (x + xm)
                                    in fromIntegral . clamp <$> f (x',y')
                                )
    where
        sz = M.Sz2 (yM - ym + 1) (xM - xm + 1)
        clamp v = min (max 0 v) 255

-- | Generate an image from a function.
--
-- Equivalent to @generateImage@, except the functions returns non-integer pixel values
-- between 0 and 1 (values outside of the range will be clamped).
generateImage' :: (Pixel p, RealFloat a) => (Int,Int) -> (Int,Int) -> ((a,a) -> p a) -> Image (p Word8)
generateImage' (xm,ym) (xM,yM) f = BaseImage $ M.makeArrayR M.D M.Par sz (\(y:.x) ->
                                    let y' = fromIntegral (y + ym)
                                        x' = fromIntegral (x + xm)
                                    in round . (*255) . clamp01 <$> f (x',y')
                                )
    where
        sz = M.Sz2 (yM - ym + 1) (xM - xm + 1)
        clamp01 v = min (max 0 v) 1

-- | Generate an image made up of a 2-color linear horizontal gradient.
--
-- Parameters:
-- - Size in terms @(width,height)@.
-- - Initial color, at the leftmost part of the image.
-- - Final color, at the rightmost part of the image.
simpleGradientH :: Pixel p => (Int,Int) -> p Word8 -> p Word8 -> Image (p Word8)
simpleGradientH (w,h) l r = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(_:.x) ->
                                round <$> lerp (percent x w)
                            )
    where
        percent :: Int -> Int -> Double
        percent val total = fromIntegral val / fromIntegral total
        l' = fromIntegral <$> l
        r' = fromIntegral <$> r
        lerp p = l' + ((r' - l') `multScalar` p)

-- | Generate an image made up of a 2-color linear vertical gradient.
--
-- Parameters:
-- - Size in terms @(width,height)@.
-- - Initial color, at the top of the image.
-- - Final color, at the bottom of the image.
simpleGradientV :: Pixel p => (Int,Int) -> p Word8 -> p Word8 -> Image (p Word8)
simpleGradientV (w,h) l r = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(y:._) ->
                                round <$> lerp (percent y h)
                            )
    where
        percent :: Int -> Int -> Double
        percent val total = fromIntegral val / fromIntegral total
        l' = fromIntegral <$> l
        r' = fromIntegral <$> r
        lerp p = l' + ((r' - l') `multScalar` p)

-- | Generate an image made up of an n-color linear horizontal gradient.
--
-- Parameters:
-- - Size in terms @(width,height)@.
-- - Initial color, at the left of the image.
-- - Evenly-spaced colors to reach throughout the gradient.
multiColorGradientH :: Pixel p => (Int,Int) -> p Word8 -> [p Word8] -> Image (p Word8)
multiColorGradientH (w,h) c [] = canvas (w,h) c
multiColorGradientH (w,h) c cs = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(_:.x) ->
                                    round <$> lerp (percent x w)
                                )
    where
        len = length cs
        percent :: Int -> Int -> Double
        percent val total = fromIntegral (len * val) / fromIntegral total
        cs' = fmap fromIntegral <$> (c:cs)
        lerp p = if (floor p :: Int) == (ceiling p :: Int)
                    then cs' !! floor p
                    else
                        let l = cs' !! floor p
                            r = cs' !! ceiling p
                            p' = p - fromIntegral (floor p :: Int)
                        in l + ((r - l) `multScalar` p')

-- | Generate an image made up of an n-color linear vertical gradient.
--
-- Parameters:
-- - Size in terms @(width,height)@.
-- - Initial color, at the top of the image.
-- - Evenly-spaced colors to reach throughout the gradient.
multiColorGradientV :: Pixel p => (Int,Int) -> p Word8 -> [p Word8] -> Image (p Word8)
multiColorGradientV (w,h) c [] = canvas (w,h) c
multiColorGradientV (w,h) c cs = BaseImage $ M.makeArrayR M.D M.Par (M.Sz2 h w) (\(y:._) ->
                                    round <$> lerp (percent y h)
                                )
    where
        len = length cs
        percent :: Int -> Int -> Double
        percent val total = fromIntegral (len * val) / fromIntegral total
        cs' = fmap fromIntegral <$> (c:cs)
        lerp p = if (floor p :: Int) == (ceiling p :: Int)
                    then cs' !! floor p
                    else
                        let l = cs' !! floor p
                            r = cs' !! ceiling p
                            p' = p - fromIntegral (floor p :: Int)
                        in l + ((r - l) `multScalar` p')

-- | Generate a bar chart image.
--
-- Parameters:
-- - Size in form @(width, height)@.
-- - Background color.
-- - Value and color of each bar.
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
                (height,color) = barHeights !! barIdx
                y' = h - y -- invert to make bottom left the origin
            in if y' <= height then color else bg
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

-- | Generate a bar chart image, with all bars having the same color.
--
-- Parameters:
-- - Size in form @(width, height)@.
-- - Background color.
-- - Color of the bars.
-- - Value that each bar represents.
--
-- Equivalent to @drawBarChart@ but with the same color for all bars.
drawBarChart' :: Pixel p => (Int,Int) -> p Word8 -> p Word8 -> [Int] -> Image (p Word8)
drawBarChart' sz bg fg bars = drawBarChart sz bg [(b,fg) | b <- bars]

-- | Stack two images vertically on top of one another to create a new image.
--
-- Parameters:
-- - Image that goes on the top.
-- - Image that goes on the bottom.
-- - Color for the background, in case the images don't share the same width.
--
-- Given that the upper image's dimensions are @(w1,h1)@, and the lower image's
-- are @(w2,h2)@, the output image's dimensions are @(max(w1,w2), h1+h2)@.
--
-- If the images do not share the same width, the less wide image will be
-- centered horizontally and the empty space on its sides will be filled with
-- the color given.
stackVertically :: Pixel p => p Word8 -> Image (p Word8) -> Image (p Word8) -> Image (p Word8)
stackVertically bg top bot = generateImage (0,-h1) (w-1,h2-1) (\(x,y) ->
                                if y < 0
                                    then M.evaluate' top' (y + h1:.x)
                                    else M.evaluate' bot' (y:.x)
                            )
    where
        (M.Sz2 h1 w1) = imageSize top
        (M.Sz2 h2 w2) = imageSize bot
        w = max w1 w2
        top' = toArray $ if w == w1 then top else top :> zoomToSize (w,h1) bg
        bot' = toArray $ if w == w2 then bot else bot :> zoomToSize (w,h2) bg

-- | Stack two images horizontally to create a new image.
--
-- Parameters:
-- - Image that goes on the left.
-- - Image that goes on the right.
-- - Color for the background, in case the images don't share the same width.
--
-- Given that the upper image's dimensions are @(w1,h1)@, and the lower image's
-- are @(w2,h2)@, the output image's dimensions are @(max(w1,w2), h1+h2)@.
--
-- If the images do not share the same width, the less wide image will be
-- centered vertically and the empty space on its sides will be filled with
-- the color given.
stackHorizontally :: Pixel p => p Word8 -> Image (p Word8) -> Image (p Word8) -> Image (p Word8)
stackHorizontally bg top bot = generateImage (-w1,0) (w2-1,h-1) (\(x,y) ->
                                if x < 0
                                    then M.evaluate' top' (y:.x + w1)
                                    else M.evaluate' bot' (y:.x)
                            )
    where
        (M.Sz2 h1 w1) = imageSize top
        (M.Sz2 h2 w2) = imageSize bot
        h = max h1 h2
        top' = toArray $ if h == h1 then top else top :> zoomToSize (w1,h) bg
        bot' = toArray $ if h == h2 then bot else bot :> zoomToSize (w2,h) bg

-- | Place four images into quadrants.
--
-- Parameters:
-- - Background color.
-- - Top-left image.
-- - Top-right image.
-- - Bottom-left image.
-- - Bottom-right image.
--
-- Given image dimensions @(w1,h1)@, @(w2,h2)@, @(w3,h3)@, and @(w4,h4)@, in
-- order of the parameters, the output image will have dimensions
-- @(max(w1,w3)+max(w2,w4), max(h1,h2)+max(h3,h4))@.
quadrants :: Pixel p => p Word8 -> Image (p Word8) -> Image (p Word8) -> Image (p Word8) -> Image (p Word8) -> Image (p Word8)
quadrants bg tl tr bl br = generateImage (-leftW, -topH) (rightW-1, botH-1) (\(x,y) ->
                                case (y<0,x<0) of
                                    (True,True) -> M.evaluate' tl' (y+topH:.x+leftW)
                                    (True,False) -> M.evaluate' tr' (y+topH:.x)
                                    (False,True) -> M.evaluate' bl' (y:.x+leftW)
                                    (False,False) -> M.evaluate' br' (y:.x)
                            )
    where
        (M.Sz2 h1 w1) = imageSize tl
        (M.Sz2 h2 w2) = imageSize tr
        (M.Sz2 h3 w3) = imageSize bl
        (M.Sz2 h4 w4) = imageSize br
        topH = max h1 h2
        botH = max h3 h4
        leftW = max w1 w3
        rightW = max w2 w4
        tl' = toArray $ if topH == h1 && leftW == w1 then tl else tl :> zoomToSize (leftW,topH) bg
        tr' = toArray $ if topH == h2 && rightW == w2 then tr else tr :> zoomToSize (rightW,topH) bg
        bl' = toArray $ if botH == h3 && leftW == w3 then bl else bl :> zoomToSize (leftW,botH) bg
        br' = toArray $ if botH == h4 && rightW == w4 then br else br :> zoomToSize (rightW,botH) bg
