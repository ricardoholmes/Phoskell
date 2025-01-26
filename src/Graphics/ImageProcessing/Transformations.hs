-- Geometric transformations
module Graphics.ImageProcessing.Transformations (
    extractRegion,
    transpose,
    translate,
    mirrorX,
    mirrorY,
) where

import Graphics.ImageProcessing.Processes (MiscProcess (MiscProcess))
import Graphics.ImageProcessing.Transformations.Translation
import Data.Massiv.Array ( Ix2 ((:.)) )
import qualified Data.Massiv.Array as M
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe)

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

transpose :: MiscProcess a a
transpose = MiscProcess M.transpose

mirrorX :: MiscProcess a a
mirrorX = MiscProcess (M.reverse M.Dim1)

mirrorY :: MiscProcess a a
mirrorY = MiscProcess (M.reverse M.Dim2)
