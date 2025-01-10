{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import System.Environment ( getArgs )
import Graphics.ImageProcessing
import Graphics.ImageProcessing.IO
import Graphics.ImageProcessing.Processes
import Graphics.ImageProcessing.Core.Pixel
import Graphics.ImageProcessing.Core.Color
import Graphics.ImageProcessing.Processes.Threshold
import Graphics.ImageProcessing.Analysis.Histogram (histogramGray)

main :: IO ()
main = do args <- getArgs
          let fname = head args
          img <- readImageRGB fname
          print $ histogramGray (img :> PointProcess rgbToGray)
          let img' = img
                  --  :> PointProcess (**0.5) -- i'll fix gamma correction shortly
                   :> PointProcess rgbToGray
                  --  :> meanFilter 5 -- i'll fix convolution shortly too
                   :> threshold 128
          writeImageRGB "input.png" img
          writeImageBinary "output.png" img'
          let custom = generateImage (Sz2 1000 1000) (\(y :. x) ->
                  let x' = fromIntegral (abs (x - 500)) / 500
                      y' = fromIntegral (abs (y - 500)) / 500
                  in Pixel3 x' ((x'*x' + y'*y') / 2) y'
                ) :> PointProcess (fmap floor . (*255))
          writeImageRGB "custom.png" custom
