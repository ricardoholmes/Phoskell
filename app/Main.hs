module Main where

import System.Environment ( getArgs )
import Graphics.Image
import Graphics.Image.IO
import Graphics.Image.ImageProcess
import Graphics.Image.Color
import Graphics.Image.Pixel
import Graphics.Image.Convolution
import Graphics.Image.Threshold

main :: IO ()
main = do args <- getArgs
          let fname = head args
          img <- readImageRGB fname
          let img' = img
                   :> PointProcess (**0.5)
                   :> PointProcess rgbToGray
                   :> meanFilter 5
                   :> threshold 0.5
          writeImageRGB "input.png" img
          writeImageBinary "output.png" img'
          let custom = generateImage (Sz2 1000 1000) (\(y :. x) ->
                  let x' = fromIntegral (abs (x - 500)) / 500
                      y' = fromIntegral (abs (y - 500)) / 500
                  in Pixel3 x' ((x'*x' + y'*y') / 2) y'
                )
          writeImageRGB "custom.png" custom
