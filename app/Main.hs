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
import Graphics.ImageProcessing.Processes.Convolution (meanFilter)

main :: IO ()
main = do args <- getArgs
          let fname = head args
          img <- readImageRGB fname
          let img' = img
                  --  :> PointProcess (**0.5) -- i'll fix gamma correction shortly
                   :> PointProcess rgbToGray
                   :> meanFilter 5
                   :> threshold 128
          putStrLn "START"
          writeImageRGB "input.png" img
          putStrLn "INPUT DONE"
          writeImageBinary "output.png" img'
          putStrLn "OUTPUT DONE"

          let hist = histogramGray (img :> PointProcess rgbToGray)
          let mostCommon = maximum hist
          print mostCommon
          let imgHist = generateImage (Sz2 2048 4096) (\(y :. x) ->
                  let x' = x `div` 16 -- 256 bars * 16 pixels = 4096 pixels
                      height = ((hist !! x') * 2048) `div` mostCommon
                      y' = 2048 - y -- invert to make bottom left the origin
                  in if y' <= height then Pixel1 0 else Pixel1 255)
          writeImageGray "histogram.png" imgHist
          putStrLn "HISTOGRAM DONE"

          let custom = generateImage (Sz2 1000 1000) (\(y :. x) ->
                  let x' = fromIntegral (abs (x - 500)) / 500
                      y' = fromIntegral (abs (y - 500)) / 500
                  in Pixel3 x' ((x'*x' + y'*y') / 2) y'
                ) :> PointProcess (fmap floor . (*255))
          writeImageRGB "custom.png" custom
          putStrLn "CUSTOM DONE"
