{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.ImageProcessing.Transformations (
    transpose,
    mirrorX,
    mirrorY,
    extractRegion,
    extractRegionUnsafe,
    zoomToSize,
    zoom,
    letterboxToAspectRatio,
) where

import Graphics.ImageProcessing.Processes
import Data.Massiv.Array ( Ix2 ((:.)) )
import qualified Data.Massiv.Array as M
import Data.Maybe (fromMaybe)

transpose :: ArrayProcess a a
transpose = ArrayProcess M.transpose

mirrorX :: ArrayProcess a a
mirrorX = ArrayProcess (M.reverse M.Dim1)

mirrorY :: ArrayProcess a a
mirrorY = ArrayProcess (M.reverse M.Dim2)

-- | Extracts the region of the image within the square defined by the coords given.
--
-- - First parameter is the top-left coords, i.e.: (xMin, yMin).
-- - Second parameter is the bottom-right coords, i.e.: (xMax, yMax).
-- - Third parameter is the value to use for any area outside of the original image.
--
-- Effectively changes the viewport size and position without moving the image.
--
-- Note: The ranges given are inclusive on both ends.
extractRegion :: (Int,Int) -> (Int,Int) -> a -> ArrayProcess a a
extractRegion (xm,ym) (xM,yM) v = ArrayProcess (\img ->
                            let ySz = yM-ym+1
                                xSz = xM-xm+1
                                sz = M.Sz2 ySz xSz
                            in M.makeArray M.Par sz (\(y:.x) ->
                                let y' = y + ym
                                    x' = x + xm
                                    ix = y':.x'
                                in fromMaybe v (M.evaluateM img ix)
                            )
                        )

extractRegionUnsafe :: (Int,Int) -> (Int,Int) -> ArrayProcess a a
extractRegionUnsafe (xm,ym) (xM,yM) = ArrayProcess (\img ->
                            let ySz = yM-ym+1
                                xSz = xM-xm+1
                                sz = M.Sz2 ySz xSz
                            in M.makeArray M.Par sz (\(y:.x) ->
                                let y' = y + ym
                                    x' = x + xm
                                    ix = y':.x'
                                in M.evaluate' img ix
                            )
                        )

-- | Extract region from the centre of the image with dimensions given.
--
-- Must also be given value to use for background (i.e. area that the image did not cover),
-- this will be ignored if the dimension given is strictly equal to or less than the image's.
zoomToSize :: (Int,Int) -> a -> ArrayProcess a a
zoomToSize (newW,newH) v = ArrayProcess (\img ->
        let (M.Sz2 h w) = M.size img
            (centreX :: Double) = fromIntegral w / 2
            (centreY :: Double) = fromIntegral h / 2
            w' = fromIntegral newW / 2
            h' = fromIntegral newH / 2
            xm = floor $ centreX - w'
            ym = floor $ centreY - h'
            xM = floor $ centreX + w' - 1
            yM = floor $ centreY + h' - 1
        in applyProcess (extractRegion (xm,ym) (xM,yM) v) img
    )

-- | Extract region from centre of the image based on multiplier given.
--
-- Must also be given value to use for background (i.e. area that the image did not cover),
-- this is ignored if multiplier >= 1.
--
-- Does not scale the image, a higher value will return a smaller area of the image.
zoom :: Double -> a -> ArrayProcess a a
zoom m v = ArrayProcess (\img ->
        let (M.Sz2 h w) = M.size img
            centreX = fromIntegral w / 2 :: Double
            centreY = fromIntegral h / 2 :: Double
            w' = (fromIntegral w / m) / 2
            h' = (fromIntegral h / m) / 2
            xm = floor $ centreX - w'
            ym = floor $ centreY - h'
            xM = floor $ centreX + w' - 1
            yM = floor $ centreY + h' - 1
        in applyProcess (extractRegion (xm,ym) (xM,yM) v) img
    )

-- | Adds borders to make an image have the given aspect ratio.
--
-- Aspect ratio should be given in terms of width relative to height,
-- e.g. (16,9) for 16:9.
--
-- Value to give borders must also be given.
letterboxToAspectRatio :: (Int,Int) -> a -> ArrayProcess a a
letterboxToAspectRatio (relX,relY) v = ArrayProcess (\img ->
        let (M.Sz2 h w) = M.size img
            d = gcd relX relY
            x = relX `div` d
            y = relY `div` d
        in case compare (w * y) (h * x) of
            EQ -> img
            GT -> let newW = w + (x - w `mod` x)
                      newH = y * newW `div` x
                  in applyProcess (zoomToSize (newW,newH) v) img
            LT -> let newH = h + (y - h `mod` y)
                      newW = x * newH `div` y
                  in applyProcess (zoomToSize (newW,newH) v) img
    )
