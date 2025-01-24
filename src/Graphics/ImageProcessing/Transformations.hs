-- Geometric transformations
module Graphics.ImageProcessing.Transformations (
    translate,
    cropTo,
) where

import Graphics.ImageProcessing.Transformations.Translation
import Graphics.ImageProcessing.Processes (MiscProcess (MiscProcess))
import Data.Massiv.Array ( Ix2 ((:.)) )
import qualified Data.Massiv.Array as M
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe)

-- | Crops the image to the given x and y ranges.
--
-- - First parameter is the x range, i.e.: (xMin, xMax).
-- - Second parameter is the y range, i.e.: (yMin, yMax).
--
-- Effectively changes the viewport size and position without moving the image.
--
-- Note: The ranges given are inclusive on both ends.
cropTo :: (Num a, NFData a) => (Int,Int) -> (Int,Int) -> MiscProcess a a
cropTo (xm,xM) (ym,yM) = MiscProcess (\img ->
                            let img' = M.computeAs M.BN img
                                ySz = yM-ym+1
                                xSz = xM-xm+1
                                sz = M.Sz2 ySz xSz
                            in M.makeArray M.Par sz (\(y:.x) ->
                                let y' = y + ym
                                    x' = x + xm
                                    ix = y':.x'
                                in fromMaybe 0 (M.index img' ix)
                            )
                        )
