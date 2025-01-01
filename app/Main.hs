module Main where

import System.Environment ( getArgs )
import Graphics.ImageProcessing
import Graphics.ImageProcessing.IO
import Graphics.ImageProcessing.Processes
import Graphics.ImageProcessing.Core.Pixel
import Graphics.ImageProcessing.Core.Color
import Graphics.ImageProcessing.Processes.Convolution
import Graphics.ImageProcessing.Processes.Threshold

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
