module Graphics.Phoskell.Transformations.Translation (
    translate,
    translateWrap,
    shearX,
    shearY,
) where

import Graphics.Phoskell.Core.Image ( ArrayProcess (ArrayProcess) )
import Data.Massiv.Array ( Ix2 ((:.)) )
import qualified Data.Massiv.Array as M
import Data.Maybe (fromMaybe)

-- | Given distance for each axis, moves the image accordingly
--
-- Displacement must be given in (x,y) format.
--
-- Any area no longer covered by the image will be replaced with the value given
-- in the second parameter.
--
-- The image will not change size, and any area moved out of the image will be lost.
translate :: (Int,Int) -> a -> ArrayProcess a a
translate (xMove,yMove) v = ArrayProcess (\img ->
        M.imap (\(y:.x) _ -> 
            let y' = y - yMove
                x' = x - xMove
            in fromMaybe v $ M.evaluateM img (y':.x')
        ) img
    )

-- | Given distance for each axis, moves the image accordingly and wrapping if needed.
--
-- Distances must be given in (x,y) format.
--
-- The image will not change size, but any area moved out of the image will be
-- wrapped to the opposite side.
translateWrap :: (Int,Int) -> ArrayProcess a a
translateWrap (xMove,yMove) = ArrayProcess (\img ->
        let (M.Sz2 maxY maxX) = M.size img
        in M.imap (\(y:.x) _ ->
            let y' = (y - yMove) `mod` maxY
                x' = (x - xMove) `mod` maxX
            in M.evaluate' img (y':.x')
        ) img
    )

-- | Apply a shear along the X axis.
shearX :: Double -> a -> ArrayProcess a a
shearX offset v = ArrayProcess (\img ->
        let (M.Sz2 h _) = M.size img
            centreY = fromIntegral h / 2
        in M.imap (\(y:.x) _ ->
            let x' = fromIntegral x
                y' = fromIntegral y - centreY
                newX = round $ x' + (offset * y')
            in fromMaybe v $ M.evaluateM img (y:.newX)
        ) img
    )

-- | Apply a shear along the Y axis.
shearY :: Double -> a -> ArrayProcess a a
shearY offset v = ArrayProcess (\img ->
        let (M.Sz2 _ w) = M.size img
            centreX = fromIntegral w / 2
        in M.imap (\(y:.x) _ ->
            let y' = fromIntegral y
                x' = fromIntegral x - centreX
                newY = round $ y' + (offset * x')
            in fromMaybe v $ M.evaluateM img (newY:.x)
        ) img
    )
