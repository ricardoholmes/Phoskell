{-# LANGUAGE ScopedTypeVariables #-}
-- | Image scaling processes.
module Graphics.Phoskell.Transformations.Scaling (
    scaleBy,
    scaleXBy,
    scaleYBy,
    scaleTo
) where
import Graphics.Phoskell.Processes (ArrayProcess(ArrayProcess))
import Data.Massiv.Array (Ix2((:.)))
import qualified Data.Massiv.Array as M

-- | Scale by a multiplier.
scaleBy :: Double -> ArrayProcess a a
scaleBy m = ArrayProcess (\img ->
        let (M.Sz2 h w) = M.size img
            h' = fromIntegral h
            w' = fromIntegral w
            newH = floor $ h' * m
            newW = floor $ w' * m
            sz = M.Sz2 newH newW
        in M.makeArray M.Par sz (\(y:.x) ->
            let y' = floor $ fromIntegral y / m
                x' = floor $ fromIntegral x / m
            in M.evaluate' img (y':.x')
        )
    )

-- | Scale horizontal axis by a multiplier.
scaleXBy :: Double -> ArrayProcess a a
scaleXBy m = ArrayProcess (\img ->
        let (M.Sz2 h w) = M.size img
            w' = fromIntegral w
            newW = floor $ w' * m
            sz = M.Sz2 h newW
        in M.makeArray M.Par sz (\(y:.x) ->
            let x' = floor $ fromIntegral x / m
            in M.evaluate' img (y:.x')
        )
    )

-- | Scale vertical axis by a multiplier.
scaleYBy :: Double -> ArrayProcess a a
scaleYBy m = ArrayProcess (\img ->
        let (M.Sz2 h w) = M.size img
            h' = fromIntegral h
            newH = floor $ h' * m
            sz = M.Sz2 newH w
        in M.makeArray M.Par sz (\(y:.x) ->
            let y' = floor $ fromIntegral y / m
            in M.evaluate' img (y':.x)
        )
    )

-- | Scale to the given size.
scaleTo :: (Int,Int) -> ArrayProcess a a
scaleTo (newW,newH) = ArrayProcess (\img ->
        let (M.Sz2 h w) = M.size img
            (hMult :: Double) = fromIntegral h / fromIntegral newH
            (wMult :: Double) = fromIntegral w / fromIntegral newW
            newSz = M.Sz2 newH newW
        in M.makeArray M.Par newSz (\(y:.x) ->
            let y' = floor $ fromIntegral y * hMult
                x' = floor $ fromIntegral x * wMult
            in M.evaluate' img (y':.x')
        )
    )
