{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.ImageProcessing.Transformations.Scaling (
    scaleBy,
    scaleXBy,
    scaleYBy,
    scaleTo
) where
import Graphics.ImageProcessing.Processes (MiscProcess(MiscProcess))
import Control.DeepSeq
import Data.Massiv.Array (Ix2((:.)))
import qualified Data.Massiv.Array as M

-- | Scale by a multiplier.
scaleBy :: NFData a => Double -> MiscProcess a a
scaleBy m = MiscProcess (\img ->
        let img' = M.computeAs M.BN img
            (M.Sz2 h w) = M.size img'
            h' = fromIntegral h
            w' = fromIntegral w
            newH = floor $ h' * m
            newW = floor $ w' * m
            sz = M.Sz2 newH newW
        in M.makeArray M.Par sz (\(y:.x) ->
            let y' = floor $ fromIntegral y / m
                x' = floor $ fromIntegral x / m
            in img' M.! (y':.x')
        )
    )

-- | Scale horizontal axis by a multiplier.
scaleXBy :: NFData a => Double -> MiscProcess a a
scaleXBy m = MiscProcess (\img ->
        let img' = M.computeAs M.BN img
            (M.Sz2 h w) = M.size img'
            w' = fromIntegral w
            newW = floor $ w' * m
            sz = M.Sz2 h newW
        in M.makeArray M.Par sz (\(y:.x) ->
            let x' = floor $ fromIntegral x / m
            in img' M.! (y:.x')
        )
    )

-- | Scale vertical axis by a multiplier.
scaleYBy :: NFData a => Double -> MiscProcess a a
scaleYBy m = MiscProcess (\img ->
        let img' = M.computeAs M.BN img
            (M.Sz2 h w) = M.size img'
            h' = fromIntegral h
            newH = floor $ h' * m
            sz = M.Sz2 newH w
        in M.makeArray M.Par sz (\(y:.x) ->
            let y' = floor $ fromIntegral y / m
            in img' M.! (y':.x)
        )
    )

-- | Scale to the given size.
scaleTo :: NFData a => (Int,Int) -> MiscProcess a a
scaleTo (newW,newH) = MiscProcess (\img ->
        let img' = M.computeAs M.BN img
            (M.Sz2 h w) = M.size img'
            (hMult :: Double) = fromIntegral h / fromIntegral newH
            (wMult :: Double) = fromIntegral w / fromIntegral newW
            newSz = M.Sz2 newH newW
        in M.makeArray M.Par newSz (\(y:.x) ->
            let y' = floor $ fromIntegral y * hMult
                x' = floor $ fromIntegral x * wMult
            in img' M.! (y':.x')
        )
    )
