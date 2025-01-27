module Graphics.ImageProcessing.Transformations (
    transpose,
    mirrorX,
    mirrorY,
    extractRegion,
    extractRegionUnsafe,
    zoom,
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

-- | Extract region based on multiplier given.
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
