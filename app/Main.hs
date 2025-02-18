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
import Graphics.ImageProcessing.Analysis.Histogram
import Graphics.ImageProcessing.Processes.Convolution (meanFilter)
import Graphics.ImageProcessing.Processes.Point
import Graphics.ImageProcessing.Transformations
import Graphics.ImageProcessing.Transformations.Cropping
import Graphics.ImageProcessing.Transformations.Translation
import Graphics.ImageProcessing.Transformations.Rotation
import Graphics.ImageProcessing.Transformations.Scaling
import Graphics.ImageProcessing.Processes.Histogram
import Graphics.ImageProcessing.Synthesis
import Graphics.ImageProcessing.Processes.Alpha
import Graphics.ImageProcessing.Analysis
import FunctionalImages
import FunctionalAnimations
import System.Exit (exitSuccess)
import Control.Monad ( unless )
import System.IO (hFlush, stdout)
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import System.Directory (createDirectoryIfMissing)

percent :: Int -> Int -> String
percent x y = show (fromIntegral (floor (1000 * x' / y')) / 10) ++ "%"
    where
        x' = fromIntegral x
        y' = fromIntegral y

pad :: Show a => Int -> a -> String
pad n x = reverse $ take n x'
    where x' = reverse (show x) ++ repeat '0'

getPathInput :: String -> Int -> String
getPathInput p x = p ++ "/input/output_" ++ pad 5 x ++ ".jpg"

getPathOutput :: String -> Int -> String
getPathOutput p x = p ++ "/output/output_" ++ pad 5 x ++ ".jpg"

frameCount :: Int
frameCount = 500

alterVideo :: String -> UTCTime -> Int -> IO ()
alterVideo p t n = do let inpPath = getPathInput p n
                      img <- readImageRGBA inpPath
                      let outPath = getPathOutput p n
                      let img' = img :> invertColorsNotAlpha :> meanFilter 5 :> rotate90 :> scaleYBy 0.5
                      writeImageRGBA outPath img'
                      t' <- getCurrentTime
                      let elapsed = fromIntegral (floor (diffUTCTime t' t * 10)) / 10
                      putStrLn ("\r" ++ inpPath ++ " --> " ++ outPath ++ " (" ++ show elapsed ++ "s)")
                      putStr (pad 4 n ++ "/" ++ show frameCount ++ " | " ++ percent n frameCount ++ " (" ++ show elapsed ++ "s)")
                      _ <- hFlush stdout
                      unless (n >= frameCount) $ alterVideo p t (n+1)

{-
main :: IO ()
main = do args <- getArgs
          let basePath = head args
          t <- getCurrentTime
          alterVideo basePath t 1
          putStrLn ""
-}

main :: IO ()
main = do args <- getArgs
          let fname = head args
          imgIn <- readImageRGB fname
          let img = imgIn :> scaleBy 0.25
          putStrLn "start"
          writeImageRGB "input.png" img
          putStrLn "input"
          writeImageGray "output.png" (img :> PointProcess rgbToGray)
          putStrLn "output"
          anim <- readFAnim ["input.png", "output.png"]
          let sz = imageSize' img
          let frames = fAnimToImages anim (-1) 0.01 sz 1
          createDirectoryIfMissing False "output-frames/"
          writeImages (\i -> "output-frames/" ++ pad 3 i ++ ".png") (take 301 frames)
          putStrLn "done"

{-
main :: IO ()
main = do let polar (r,t) = if even (round (r/10 + t*5/pi)) then 1 else 0
          let img = fromPolarF polar
          let rotF θ = applyTransformF (\(x,y) -> (
                    x*cos θ + y*sin θ,
                    y*cos θ - x*sin θ
                ))
          let anim t = rotF t img
          let frames = fAnimToImages anim 0 0.01 (100,100) 0.1
          createDirectoryIfMissing False "output-frames/"
          writeImages (\i -> "output-frames/" ++ pad 4 i ++ ".jpg") (take 1000 frames)
-}

{-
drawImgHist :: [Int] -> Pixel3 Word8 -> Image (Pixel3 Word8)
drawImgHist hist fg = drawBarChart' (2048,1024) 0 fg hist

rot :: Image RGB -> Int -> Image RGB
rot img 0 = img
rot img x = rot (img :> rotateDeg 1 0) (x-1)

rot' :: Image RGB -> Int -> Image RGB
rot' img 0 = img
rot' img x = rot' (img :> rotateDeg (-1) 0) (x-1)

main :: IO ()
main = do args <- getArgs
          let fname = head args
          img <- readImageRGB fname

          imgRGBA <- readImageRGBA fname
          let fimg (x,y) = mkSmallDouble <$> Pixel4 (x-y) (x+y) (y-x) 1
          writeImageRGBA "test-f.png" (fImageToImage fimg (2,2) 0.001)

          let imgF = imageToFImage imgRGBA
          writeImageRGBA "read-f-no-interpolate.png" (fImageToImage (imageToFImageN imgRGBA) (100,100) 0.1)
          writeImageRGBA "read-f-interpolate.png" (fImageToImage imgF (100,100) 0.1)

          let polar (r,t) = if even (round (r/10 + t*5/pi)) then Pixel4 0.5 0 0.5 0.5 else Pixel4 0 0 0 0
          writeImageRGBA "read-f-polar.png" (fImageToImage (overlayImageF imgF (fromPolarF polar)) (500,500) 0.5)

          let rotF θ = applyTransformF (\(x,y) -> (
                    x*cos θ + y*sin θ,
                    y*cos θ - x*sin θ
                ))
          let rotDegF θ = rotF (θ/180 * pi)
          writeImageRGBA "read-f-rot90.png" (fImageToImage (rotDegF 90 imgF) (3000,3000) 5)
          writeImageRGBA "read-f-rotfull.png" (fImageToImage (repeatProcessF 360 (rotDegF 1) imgF) (3000,3000) 5)

          let squares (x,y) = if even (floor (x/50) + floor (y/50)) then 1 else 0
          writeImageRGBA "read-f-rot120.png" (fImageToImage (overlayImageF squares $ rotDegF 120 imgF) (3000,3000) 5)

          _ <- exitSuccess

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

          writeImageRGB "rotated-badf.png" (rot img 360)
          putStrLn "rotatef DONE"
          writeImageRGB "rotated-badb.png" (rot' img 360)
          putStrLn "rotateb DONE"
          writeImageRGB "rotated-badfb.png" (rot' (rot img 360) 360)
          putStrLn "rotatefb DONE"
          writeImageRGB "rotated-badbf.png" (rot (rot' img 360) 360)
          putStrLn "rotatebf DONE"

          let imgA = img :> addAlphaChannel
          writeImageRGBA "alpha-overlay.png" (imgA :> overlayImage (simpleGradientH (imageSize imgA) redA greenA :> alterAlpha (const 128)))
          writeImageRGBA "alpha-no-red.png" (imgA :> chromaKeyRemove redA 1)
          writeImageRGBA "alpha-just-red.png" (imgA :> chromaKeyExtract redA 1)
          writeImageRGBA "alpha-recover.png" ((imgA :> chromaKeyRemove redA 1) + (imgA :> chromaKeyExtract redA 1))
          putStrLn "ALPHA CHANNEL MANIPULATION DONE"

          img2 <- readImageRGB "../image.png"
          writeImageRGB "stack-vertical.png" (stackVertically 0 img img2)
          writeImageRGB "stack-horizontal.png" (stackHorizontally 0 img img2)
          writeImageRGB "stack-quadrants.png" (quadrants 0 img img2 img2 img)
          putStrLn "IMAGE STACKING DONE"

          writeImageBinary "output-otsu.png" (img :> PointProcess rgbToGray :> otsuThreshold)
          putStrLn "THRESHOLDING DONE"

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
          let imgHist = quadrants 0 imgHistR imgHistG imgHistB imgHistY
          writeImageRGB "histogram.png" imgHist
          putStrLn "HISTOGRAM DONE"

          let custom = generateImage' (-500,-500) (500,500) (\(x,y) ->
                    let x' = abs x / 500
                        y' = abs y / 500
                    in Pixel3 x' ((x'*x' + y'*y') / 2) y'
                )
          writeImageRGB "custom.png" custom
          putStrLn "CUSTOM DONE"

          writeImageRGB "gradient-2-color-h.png" (simpleGradientH (500,250) red blue)
          writeImageRGB "gradient-2-color-v.png" (simpleGradientV (250,500) red blue)
          writeImageRGB "gradient-5-color-h.png" (multiColorGradientH (1000,1000) 0 [blue,red,green,255])
          writeImageRGB "gradient-5-color-v.png" (multiColorGradientV (1000,1000) 0 [blue,red,green,255])
          putStrLn "GRADIENTS DONE"
-}
