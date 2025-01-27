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

cropToAspectRatio :: NFData a => (Int,Int) -> MiscProcess a a
cropToAspectRatio (x,y) = cropToAspectRatio' (x'/y')
    where
        x' = fromIntegral x
        y' = fromIntegral y

cropToAspectRatio' :: NFData a => Double -> MiscProcess a a
cropToAspectRatio' relW = MiscProcess (\img ->
        let (M.Sz2 h w) = M.size img
            h' = fromIntegral h
            w' = fromIntegral w
        in case compare (w'/h') relW of
            EQ -> img
            GT -> let newW = round (h' * relW)
                  in applyProcess (cropToSize (newW,h)) img
            LT -> let newH = round (w' / relW)
                  in applyProcess (cropToSize (w,newH)) img
    )
