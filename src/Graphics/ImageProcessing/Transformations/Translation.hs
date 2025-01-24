module Graphics.ImageProcessing.Transformations.Translation (
    translate,
) where

import Graphics.ImageProcessing.Processes (MiscProcess (MiscProcess))
import Graphics.ImageProcessing.Core (Pixel)
import Data.Massiv.Array ( Ix2 ((:.)), BL (..), (!) )
import qualified Data.Massiv.Array as M
import Data.Ix (Ix(inRange))

-- | Given distance for each axis, moves the image accordingly
--
-- Distances must be given in (x,y) format.
--
-- The image will not change size, any area moved out of the image will be lost.
-- Similarly, any area no longer covered by the image will simply become black (0).
translate :: (Pixel p, Num a) => (Int,Int) -> MiscProcess (p a) (p a)
translate (xMove,yMove) = MiscProcess (\img -> let img' = M.computeAs BL img
                                               in M.imap (\c _ -> translatePixel img' c) img')
    where
        translatePixel img' (y:.x) = translatePixel' img' (y - yMove) (x - xMove)
        translatePixel' img' y' x' = if inRange (0,maxY) y' && inRange (0,maxX) x'
                                        then img' ! (y':.x')
                                        else 0
            where (M.Sz2 maxY maxX) = M.size img'
