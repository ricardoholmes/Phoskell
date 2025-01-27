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
import Graphics.ImageProcessing.Processes.Point
import Graphics.ImageProcessing.Transformations
import Graphics.ImageProcessing.Transformations.Cropping
import Graphics.ImageProcessing.Transformations.Translation
import Graphics.ImageProcessing.Transformations.Rotation
import Graphics.ImageProcessing.Transformations.Scaling

main :: IO ()
main = do args <- getArgs
          let fname = head args
          img <- readImageRGB fname
          let img' = img
                   :> gammaCorrect 0.5
                   :> PointProcess rgbToGray
                   :> meanFilter 5
                   :> threshold 128
          putStrLn "START"
          writeImageRGB "input.png" img
          putStrLn "INPUT DONE"
          writeImageBinary "output.png" img'
          putStrLn "OUTPUT DONE"

          writeImageHSV "hsv.png" (img :> PointProcess rgbToHSV)

          writeImageRGB "output-gain.png" (img :> applyGain 2)
          writeImageRGB "output-addbias.png" (img :> addBias 50)
          writeImageRGB "output-subbias.png" (img :> subtractBias 50)
          writeImageRGB "output-gain-bias.png" (img :> applyGain 2 :> subtractBias 50)
          writeImageRGB "output-adjust-hue.png" (img :> alterHue (+128))
          putStrLn "POINT PROCESSES DONE"

          writeImageRGB "output-translate.png" (img :> translate (100,100) 0)
          writeImageRGB "output-translate-wrapped.png" (img :> translateWrap (5000,5000))
          writeImageRGB "output-shear-x.png" (img :> shearX 0.1 0)
          writeImageRGB "output-shear-y.png" (img :> shearY 0.1 0)
          writeImageRGB "output-extract.png" (img :> extractRegion (1500,300) (2500,700) 0)
          writeImageRGB "output-rot90.png" (img :> rotate90)
          writeImageRGB "output-rot180.png" (img :> rotate180)
          writeImageRGB "output-crop-res480.png" (img :> cropToSize (854,480))
          writeImageRGB "output-crop-ar43.png" (img :> cropToAspectRatio (4,3))
          writeImageRGB "output-rot90-ar43.png" (img :> rotate90 :> cropToAspectRatio (4,3))
          writeImageRGB "output-rot45.png" (img :> rotateDeg 45 0)
          writeImageRGB "output-scalemul2.png" (img :> scaleBy 2)
          writeImageRGB "output-scalediv10.png" (img :> scaleBy 0.1)
          writeImageRGB "output-scaleXmul2.png" (img :> scaleXBy 2)
          writeImageRGB "output-scaleYdiv2.png" (img :> scaleYBy 0.5)
          writeImageRGB "output-scale1080p.png" (img :> scaleTo (1920,1080))
          writeImageRGB "output-zoom-in.png" (img :> zoom 2 0)
          writeImageRGB "output-zoom-out.png" (img :> zoom 0.5 0)
          putStrLn "TRANSFORMATIONS DONE"

          let hist = histogramGray (img :> PointProcess rgbToGray)
          let mostCommon = maximum hist
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
