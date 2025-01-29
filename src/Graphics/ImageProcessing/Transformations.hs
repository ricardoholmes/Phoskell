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
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe)

transpose :: MiscProcess a a
transpose = MiscProcess M.transpose

mirrorX :: MiscProcess a a
mirrorX = MiscProcess (M.reverse M.Dim1)

mirrorY :: MiscProcess a a
mirrorY = MiscProcess (M.reverse M.Dim2)

-- | Extracts the region of the image within the square defined by the coords given.
--
-- - First parameter is the bottom-left coords, i.e.: (xMin, yMin).
-- - Second parameter is the top-right coords, i.e.: (xMax, yMax).
-- - Third parameter is the value to use for any area outside original image.
--
-- Effectively changes the viewport size and position without moving the image.
--
-- Note: The ranges given are inclusive on both ends.
extractRegion :: NFData a => (Int,Int) -> (Int,Int) -> a -> MiscProcess a a
extractRegion (xm,ym) (xM,yM) v = MiscProcess (\img ->
                            let img' = M.computeAs M.BN img
                                ySz = yM-ym+1
                                xSz = xM-xm+1
                                sz = M.Sz2 ySz xSz
                            in M.makeArray M.Par sz (\(y:.x) ->
                                let y' = y + ym
                                    x' = x + xm
                                    ix = y':.x'
                                in fromMaybe v (M.index img' ix)
                            )
                        )

extractRegionUnsafe :: NFData a => (Int,Int) -> (Int,Int) -> MiscProcess a a
extractRegionUnsafe (xm,ym) (xM,yM) = MiscProcess (\img ->
                            let img' = M.computeAs M.BN img
                                ySz = yM-ym+1
                                xSz = xM-xm+1
                                sz = M.Sz2 ySz xSz
                            in M.makeArray M.Par sz (\(y:.x) ->
                                let y' = y + ym
                                    x' = x + xm
                                    ix = y':.x'
                                in img' M.! ix
                            )
                        )

-- | Extract region from the centre of the image with dimensions given.
--
-- Must also be given value to use for background (i.e. area that the image did not cover),
-- this will be ignored if the dimension given is strictly equal to or less than the image's.
zoomToSize :: NFData a => (Int,Int) -> a -> MiscProcess a a
zoomToSize (newW,newH) v = MiscProcess (\img ->
        let (M.Sz2 h w) = M.size img
            (centreX :: Double) = fromIntegral w / 2
            (centreY :: Double) = fromIntegral h / 2
            w' = fromIntegral newW / 2
            h' = fromIntegral newH / 2
            xm = ceiling $ centreX - w'
            ym = ceiling $ centreY - h'
            xM = ceiling $ centreX + w' - 1
            yM = ceiling $ centreY + h' - 1
        in applyProcess (extractRegion (xm,ym) (xM,yM) v) img
    )

-- | Extract region from centre of the image based on multiplier given.
--
-- Must also be given value to use for background (i.e. area that the image did not cover),
-- this is ignored if multiplier >= 1.
--
-- Does not scale the image, a higher value will return a smaller area of the image.
zoom :: NFData a => Double -> a -> MiscProcess a a
zoom m v = MiscProcess (\img ->
        let (M.Sz2 h w) = M.size img
            centreX = w `div` 2
            centreY = h `div` 2
            h' = round (fromIntegral h / m) `div` 2
            w' = round (fromIntegral w / m) `div` 2
            xm = centreX - w'
            ym = centreY - h'
            xM = centreX + w'
            yM = centreY + h'
        in applyProcess (extractRegion (xm,ym) (xM,yM) v) img
    )

-- | Adds borders to make an image have the given aspect ratio.
--
-- Aspect ratio should be given in terms of width relative to height,
-- e.g. (16,9) for 16:9.
--
-- Value to give borders must also be given.
letterboxToAspectRatio :: NFData a => (Int,Int) -> a -> MiscProcess a a
letterboxToAspectRatio (relX,relY) v = MiscProcess (\img ->
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
