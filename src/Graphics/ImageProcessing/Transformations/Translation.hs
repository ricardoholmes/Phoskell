module Graphics.ImageProcessing.Transformations.Translation (
    translate,
    translateWrap,
    shearX,
    shearY,
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

shearX :: NFData a => Double -> a -> MiscProcess a a
shearX offset v = MiscProcess (\img ->
        let img' = M.computeAs BN img
            (M.Sz2 h _) = M.size img'
            centreY = fromIntegral h / 2
        in M.imap (\(y:.x) _ ->
            let x' = fromIntegral x
                y' = fromIntegral y - centreY
                newX = round $ x' + (offset * y')
            in fromMaybe v $ M.index img' (y:.newX)
        ) img'
    )

shearY :: NFData a => Double -> a -> MiscProcess a a
shearY offset v = MiscProcess (\img ->
        let img' = M.computeAs BN img
            (M.Sz2 _ w) = M.size img'
            centreX = fromIntegral w / 2
        in M.imap (\(y:.x) _ ->
            let y' = fromIntegral y
                x' = fromIntegral x - centreX
                newY = round $ y' + (offset * x')
            in fromMaybe v $ M.index img' (newY:.x)
        ) img'
    )
