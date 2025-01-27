module Graphics.ImageProcessing.Transformations.Translation (
    translate,
    translateWrap,
) where

import Graphics.ImageProcessing.Processes (MiscProcess (MiscProcess))
import Data.Massiv.Array ( Ix2 ((:.)), BN (..), (!) )
import qualified Data.Massiv.Array as M
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe)

-- | Given distance for each axis, moves the image accordingly
--
-- Displacement must be given in (x,y) format.
--
-- Any area no longer covered by the image will be replaced with the value given
-- in the second parameter.
--
-- The image will not change size, and any area moved out of the image will be lost.
translate :: NFData a => (Int,Int) -> a -> MiscProcess a a
translate (xMove,yMove) v = MiscProcess (\img ->
        let img' = M.computeAs BN img
        in M.imap (\(y:.x) _ -> 
            let y' = y - yMove
                x' = x - xMove
            in fromMaybe v $ M.index img' (y':.x')
        ) img'
    )

-- | Given distance for each axis, moves the image accordingly and wrapping if needed.
--
-- Distances must be given in (x,y) format.
--
-- The image will not change size, but any area moved out of the image will be
-- wrapped to the opposite side.
translateWrap :: NFData a => (Int,Int) -> MiscProcess a a
translateWrap (xMove,yMove) = MiscProcess (\img ->
        let img' = M.computeAs BN img
            (M.Sz2 maxY maxX) = M.size img'
        in M.imap (\(y:.x) _ ->
            let y' = (y - yMove) `mod` maxY
                x' = (x - xMove) `mod` maxX
            in img' ! (y':.x')
        ) img'
    )
