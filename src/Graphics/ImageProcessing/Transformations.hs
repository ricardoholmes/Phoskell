-- Geometric transformations
module Graphics.ImageProcessing.Transformations (
    extractRegion,
    transpose,
    translate,
    mirrorX,
    mirrorY,
    cropToSize,
    cropToAspectRatio,
    cropToAspectRatio',
) where

import Graphics.ImageProcessing.Processes (MiscProcess (MiscProcess), ImageProcess (applyProcess))
import Graphics.ImageProcessing.Transformations.Translation
import Data.Massiv.Array ( Ix2 ((:.)) )
import qualified Data.Massiv.Array as M
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe)
import Data.Fixed (mod')

transpose :: MiscProcess a a
transpose = MiscProcess M.transpose

mirrorX :: MiscProcess a a
mirrorX = MiscProcess (M.reverse M.Dim1)

mirrorY :: MiscProcess a a
mirrorY = MiscProcess (M.reverse M.Dim2)

-- | Extracts the region of the image within the square defined by the coords given.
--
-- - First parameter is the start coords, i.e.: (xMin, yMin).
-- - Second parameter is the y range, i.e.: (xMax, yMax).
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

cropToSize :: NFData a => (Int,Int) -> MiscProcess a a
cropToSize (x,y) = MiscProcess (\img ->
        let (M.Sz2 h w) = M.size img
            centreX = w `div` 2
            centreY = h `div` 2
            xHalfDown = x `div` 2
            yHalfDown = y `div` 2
            xHalfUp = xHalfDown + if even x then 0 else 1
            yHalfUp = yHalfDown + if even y then 0 else 1
            xm = centreX - xHalfDown
            ym = centreY - yHalfDown
            xM = centreX + xHalfUp - 1
            yM = centreY + yHalfUp - 1
            crop' = extractRegionUnsafe (xm,ym) (xM,yM)
        in applyProcess crop' img
    )

-- | Crops an image to the given aspect ratio.
--
-- Aspect ratio should be given in terms of width relative to height,
-- e.g. (16,9) for 16:9.
--
-- An aspect ratio impossible for the image's size (e.g. 16:9 for a 2x2 image)
-- will result in an empty (0x0) image.
--
-- May throw an error if given an invalid aspect ratio.
cropToAspectRatio :: NFData a => (Int,Int) -> MiscProcess a a
cropToAspectRatio (relX,relY) = MiscProcess (\img ->
        let (M.Sz2 h w) = M.size img
            d = gcd relX relY
            x = relX `div` d
            y = relY `div` d
        in case compare (w * y) (h * x) of
            EQ -> img
            GT -> let newH = h - (h `mod` y)
                      newW = x * newH `div` y
                  in applyProcess (cropToSize (newW,newH)) img
            LT -> let newW = w - (w `mod` x)
                      newH = y * newW `div` x
                  in applyProcess (cropToSize (newW,newH)) img
    )

-- | Crops an image to the given aspect ratio.
--
-- Aspect ratio should be given in terms of width relative to 1 height,
-- i.e. the value for width in width:height when height is 1.
--
-- Due to floating-point shenanigans, the output is not guaranteed to be exactly right.
--
-- May throw an error if given an invalid aspect ratio.
cropToAspectRatio' :: NFData a => Double -> MiscProcess a a
cropToAspectRatio' relW = MiscProcess (\img ->
        let (M.Sz2 h w) = M.size img
            h' = fromIntegral h
            w' = fromIntegral w
        in case compare (w'/h') relW of
            EQ -> img
            GT -> let maxW = fromInteger $ floor (h' * relW)
                      newW = round $ maxW - (maxW `mod'` relW)
                      newH = round $ fromIntegral newW / relW
                  in applyProcess (cropToSize (newW,newH)) img
            LT -> let newW = round $ w' - (w' `mod'` relW)
                      newH = round $ fromIntegral newW / relW
                  in applyProcess (cropToSize (newW,newH)) img
    )
