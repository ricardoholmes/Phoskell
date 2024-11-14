{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment (getArgs)
import Graphics.IO (readImage, writeImage)
import Graphics.ImageProcess (PointProcess(PointProcess), (-:>))
import Graphics.Color.Space ( Word8, dropAlpha, Alpha )
import Graphics.Pixel.ColorSpace (liftPixel)
import Graphics.Image (Image)
import Graphics.Color.Model (RGB)

main :: IO ()
main = do args <- getArgs
          let fname = head args
          (img :: Image (Alpha RGB) Word8) <- readImage fname
          let (img' :: Image RGB Word8) = img -:> PointProcess (liftPixel (dropAlpha . negate))
          writeImage "out.png" img'
