{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import System.Environment ( getArgs )

import Graphics.ImageProcessing.IO.Input
import Graphics.ImageProcessing.IO.Output
import Graphics.ImageProcessing.Processes
import Graphics.ImageProcessing.Core.Image
import Graphics.ImageProcessing.Core.Pixel
import Graphics.ImageProcessing.Core.Color
import Graphics.ImageProcessing.Processes.Threshold
import Graphics.ImageProcessing.Analysis.Histogram (histogram1, histogram3)
import Graphics.ImageProcessing.Processes.Convolution (meanFilter)
import Graphics.ImageProcessing.Processes.Point
import Graphics.ImageProcessing.Transformations
import Graphics.ImageProcessing.Transformations.Cropping
import Graphics.ImageProcessing.Transformations.Translation
import Graphics.ImageProcessing.Transformations.Rotation
import Graphics.ImageProcessing.Transformations.Scaling
import Graphics.ImageProcessing.Processes.Histogram (contrastStretch, equaliseHistogram)
import Graphics.ImageProcessing.Synthesis (generateImage)

drawImgHist :: [Int] -> Pixel3 Word8 -> Image (Pixel3 Word8)
drawImgHist hist fg = generateImage (0,0) (2047,1023) (\(x,y) ->
        let x' = x `div` 8 -- 256 bars * 8 pixels = 2048 pixels
            height = ((hist !! x') * 1024) `div` maximum hist
            y' = 1024 - y -- invert to make bottom left the origin
            i = fromIntegral x'
        in if y' <= height then fg else Pixel3 i i i
    )

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

          let imgWorseContrastGray = img :> PointProcess rgbToGray :> PointProcess (\(Pixel1 p) -> Pixel1 (p `div` 2))
          writeImageGray "output-gray.png" imgWorseContrastGray
          writeImageRGB "output-gray-histogram.png" (drawImgHist (histogram1 imgWorseContrastGray) (Pixel3 255 255 0))
          writeImageGray "output-gray-contrast-stretch.png" (imgWorseContrastGray :> contrastStretch)
          writeImageRGB "output-gray-contrast-stretch-histogram.png" (drawImgHist (histogram1 $ imgWorseContrastGray :> contrastStretch) (Pixel3 255 255 0))
          writeImageGray "output-gray-equalise-histogram.png" (imgWorseContrastGray :> equaliseHistogram)
          writeImageRGB "output-gray-equalise-histogram-histogram.png" (drawImgHist (histogram1 $ imgWorseContrastGray :> equaliseHistogram) (Pixel3 255 255 0))
          putStrLn "GRAYSCALE HISTOGRAM MANIPULATION DONE"

          let imgWorseContrastRGB = img :> PointProcess (fmap (`div` 2))
          writeImageRGB "output-bad-contrast.png" imgWorseContrastRGB
          writeImageRGB "output-bad-contrast-histogram.png" (drawImgHist (histogram1 $ imgWorseContrastRGB :> PointProcess rgbToGray) (Pixel3 255 255 0))
          writeImageRGB "output-bad-contrast-stretched.png" (imgWorseContrastRGB :> contrastStretch)
          writeImageRGB "output-bad-contrast-stretched-histogram.png" (drawImgHist (histogram1 $ imgWorseContrastRGB :> contrastStretch :> PointProcess rgbToGray) (Pixel3 255 255 0))
          writeImageRGB "output-bad-contrast-equalised.png" (imgWorseContrastRGB :> equaliseHistogram)
          writeImageRGB "output-bad-contrast-equalised-histogram.png" (drawImgHist (histogram1 $ imgWorseContrastRGB :> equaliseHistogram :> PointProcess rgbToGray) (Pixel3 255 255 0))
          putStrLn "RGB HISTOGRAM MANIPULATION DONE"

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
          writeImageRGB "output-zoom-out-slightly.png" (img :> PointProcess (const 0) :> zoomToSize (2049,1187) 255)
          writeImageRGB "output-letterbox-219.png" (img :> letterboxToAspectRatio (21,9) 0)
          writeImageRGB "output-letterbox-921.png" (img :> letterboxToAspectRatio (9,21) 0)
          putStrLn "TRANSFORMATIONS DONE"

          let imgSmall = img :> scaleBy 0.25
          writeImageRGB "histogram-image.png" imgSmall -- i.e. image used for histogram(s)
          let (histR,histG,histB) = histogram3 imgSmall
          let histY = histogram1 (imgSmall :> PointProcess rgbToGray)
          let imgHistR = drawImgHist histR red
          let imgHistG = drawImgHist histG green
          let imgHistB = drawImgHist histB blue
          let imgHistY = drawImgHist histY (Pixel3 255 255 0)
          let imgHist = generateImage (0,0) (4096,2048) (\(x,y) ->
                    let subImg = case (x < 2048, y < 1024) of
                            (True,True) -> imgHistR
                            (True,False) -> imgHistG
                            (False,True) -> imgHistB
                            (False,False) -> imgHistY
                    in subImg ! (x `mod` 2048, y `mod` 1024)
                )
          writeImageRGB "histogram.png" imgHist
          putStrLn "HISTOGRAM DONE"

          let custom = generateImage (-500,-500) (500,500) (\(x,y) ->
                    let x' = fromIntegral (abs x) / 500
                        y' = fromIntegral (abs y) / 500
                    in round <$> 255 * Pixel3 x' ((x'*x' + y'*y') / 2) y'
                )
          writeImageRGB "custom.png" custom
          putStrLn "CUSTOM DONE"
