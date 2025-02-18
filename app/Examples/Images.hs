{-# OPTIONS_GHC -Wno-type-defaults #-}
module Examples.Images (
    exampleFImage,
    exampleImageManipulation,
    exampleImageHistograms,
    exampleCustomImages,
) where

import Data.List
import System.Directory
import Graphics.ImageProcessing.IO
import Graphics.ImageProcessing.Core
import FunctionalImages
import Graphics.ImageProcessing.Processes.Point
import Graphics.ImageProcessing.Core.Image
import Graphics.ImageProcessing.Core.Color
import Graphics.ImageProcessing.Processes.Convolution
import Graphics.ImageProcessing.Processes.Threshold
import Graphics.ImageProcessing.Transformations.Rotation
import Graphics.ImageProcessing.Processes.Alpha
import Graphics.ImageProcessing.Synthesis
import Graphics.ImageProcessing.Analysis
import Graphics.ImageProcessing.Transformations.Scaling
import Graphics.ImageProcessing.Processes.Histogram
import Graphics.ImageProcessing.Transformations.Translation
import Graphics.ImageProcessing.Transformations
import Graphics.ImageProcessing.Transformations.Cropping

outBaseFolder :: String
outBaseFolder = "output-images"

getOutPath :: String -> String -> FilePath
getOutPath folder fname = intercalate "/" [outBaseFolder, folder, fname]

mkOutFolder :: String -> IO ()
mkOutFolder folder = createDirectoryIfMissing True path
    where path = intercalate "/" [outBaseFolder, folder]

-- Functional Images --

writeFImage :: String -> Image RGBA -> IO ()
writeFImage fname = writeImageRGBA (getOutPath "functional" fname)

exampleFImage :: FilePath -> IO ()
exampleFImage path = do mkOutFolder "functional"
                        img <- readImageRGBA path
                        let fimg (x,y) = mkSmallDouble <$> Pixel4 (x-y) (x+y) (y-x) 1
                        writeFImage "test-f.png" (fImageToImage fimg (2,2) 0.001)

                        let imgF = imageToFImage img
                        writeFImage "read-f-no-interpolate.png" (fImageToImage (imageToFImageN img) (100,100) 0.1)
                        writeFImage "read-f-interpolate.png" (fImageToImage imgF (100,100) 0.1)

                        let polar (r,t) = if even (round (r/10 + t*5/pi)) then Pixel4 0.5 0 0.5 0.5 else Pixel4 0 0 0 0
                        writeFImage "read-f-polar.png" (fImageToImage (overlayImageF imgF (fromPolarF polar)) (500,500) 0.5)

                        let rotF θ = applyTransformF (\(x,y) -> (
                                    x*cos θ + y*sin θ,
                                    y*cos θ - x*sin θ
                                ))
                        let rotDegF θ = rotF (θ/180 * pi)
                        writeFImage "read-f-rot90.png" (fImageToImage (rotDegF 90 imgF) (3000,3000) 5)
                        writeFImage "read-f-rotfull.png" (fImageToImage (repeatProcessF 360 (rotDegF 1) imgF) (3000,3000) 5)

                        let squares (x,y) = if even (floor (x/50) + floor (y/50)) then 1 else 0
                        writeFImage "read-f-rot120.png" (fImageToImage (overlayImageF squares $ rotDegF 120 imgF) (3000,3000) 5)

-- Image Manipulation --

rot :: Image RGB -> Int -> Image RGB
rot img 0 = img
rot img x = rot (img :> rotateDeg 1 0) (x-1)

rot' :: Image RGB -> Int -> Image RGB
rot' img 0 = img
rot' img x = rot' (img :> rotateDeg (-1) 0) (x-1)

exampleImageManipulation :: FilePath -> IO ()
exampleImageManipulation fp = do
        img <- readImageRGB fp
        mkOutFolder "manip"
        setCurrentDirectory (outBaseFolder ++ "/manip/")
        let img' = img
                    :> gammaCorrect 0.5
                    :> PointProcess rgbToGray
                    :> meanFilter 5
                    :> threshold 128
        putStrLn "manipulation: START"
        writeImageRGB "input.png" img
        putStrLn "manipulation: INPUT DONE"
        writeImageBinary "output.png" img'
        putStrLn "manipulation: OUTPUT DONE"

        let imgA = img :> addAlphaChannel
        writeImageRGBA "alpha-overlay.png" (imgA :> overlayImage (simpleGradientH (imageSize imgA) redA greenA :> alterAlpha (const 128)))
        writeImageRGBA "alpha-no-red.png" (imgA :> chromaKeyRemove redA 1)
        writeImageRGBA "alpha-just-red.png" (imgA :> chromaKeyExtract redA 1)
        writeImageRGBA "alpha-recover.png" ((imgA :> chromaKeyRemove redA 1) + (imgA :> chromaKeyExtract redA 1))
        putStrLn "manipulation: ALPHA CHANNEL MANIPULATION DONE"

        let img2 = img :> scaleXBy 0.5 :> scaleYBy 1.5
        writeImageRGB "stack-vertical.png" (stackVertically 0 img img2)
        writeImageRGB "stack-horizontal.png" (stackHorizontally 0 img img2)
        writeImageRGB "stack-quadrants.png" (quadrants 0 img img2 img2 img)
        putStrLn "manipulation: IMAGE STACKING DONE"

        writeImageBinary "output-otsu.png" (img :> PointProcess rgbToGray :> otsuThreshold)
        putStrLn "manipulation: THRESHOLDING DONE"

        writeImageRGB "output-gain.png" (img :> applyGain 2)
        writeImageRGB "output-addbias.png" (img :> addBias 50)
        writeImageRGB "output-subbias.png" (img :> subtractBias 50)
        writeImageRGB "output-gain-bias.png" (img :> applyGain 2 :> subtractBias 50)
        writeImageRGB "output-adjust-hue.png" (img :> alterHue (+128))
        putStrLn "manipulation: POINT PROCESSES DONE"

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
        putStrLn "manipulation: TRANSFORMATIONS DONE"

        writeImageRGB "rotated-badf.png" (rot img 360)
        writeImageRGB "rotated-badb.png" (rot' img 360)
        writeImageRGB "rotated-badfb.png" (rot' (rot img 360) 360)
        writeImageRGB "rotated-badbf.png" (rot (rot' img 360) 360)
        putStrLn "manipulation: REPEATED ROTATIONS DONE"

        setCurrentDirectory "../.." -- leave output directory

exampleImageHistograms :: FilePath -> IO ()
exampleImageHistograms fp = do
        img <- readImageRGB fp
        mkOutFolder "histograms"
        setCurrentDirectory (outBaseFolder ++ "/histograms/")
        putStrLn "histograms: START"

        let imgWorseContrastGray = img :> PointProcess rgbToGray :> PointProcess (fmap (`div` 2))
        writeImageGray "output-gray.png" imgWorseContrastGray
        writeImageRGB "output-gray-histogram.png" (drawHistogramSingle imgWorseContrastGray (2048,1024) 0 255)
        writeImageGray "output-gray-contrast-stretch.png" (imgWorseContrastGray :> contrastStretch)
        writeImageRGB "output-gray-contrast-stretch-histogram.png" (drawHistogramSingle (imgWorseContrastGray :> contrastStretch) (2048,1024) 0 255)
        writeImageGray "output-gray-equalise-histogram.png" (imgWorseContrastGray :> equaliseHistogram)
        writeImageRGB "output-gray-equalise-histogram-histogram.png" (drawHistogramSingle (imgWorseContrastGray :> equaliseHistogram) (2048,1024) 0 255)
        putStrLn "histograms: GRAYSCALE HISTOGRAM MANIPULATION DONE"

        let imgWorseContrastRGB = img :> PointProcess (fmap (`div` 2))
        writeImageRGB "output-bad-contrast.png" imgWorseContrastRGB
        writeImageRGB "output-bad-contrast-histogram.png" (drawHistogramSingle (imgWorseContrastRGB :> PointProcess rgbToGray) (2048,1024) 0 255)
        writeImageRGB "output-bad-contrast-stretched.png" (imgWorseContrastRGB :> contrastStretch)
        writeImageRGB "output-bad-contrast-stretched-histogram.png" (drawHistogramSingle (imgWorseContrastRGB :> contrastStretch :> PointProcess rgbToGray) (2048,1024) 0 255)
        writeImageRGB "output-bad-contrast-equalised.png" (imgWorseContrastRGB :> equaliseHistogram)
        writeImageRGB "output-bad-contrast-equalised-histogram.png" (drawHistogramSingle (imgWorseContrastRGB :> equaliseHistogram :> PointProcess rgbToGray) (2048,1024) 0 255)
        putStrLn "histograms: RGB HISTOGRAM MANIPULATION DONE"

        let imgSmall = img :> scaleBy 0.25
        writeImageRGB "histogram-image.png" imgSmall -- i.e. image used for histogram(s)
        writeImageRGB "histogram.png" (drawHistogramsRGBY imgSmall (4096,2048) 0 red green blue 255)
        putStrLn "histograms: RGBY HISTOGRAM DONE"

        setCurrentDirectory "../.." -- leave output directory

-- Custom Images --
exampleCustomImages :: IO ()
exampleCustomImages = do
        mkOutFolder "custom"
        let custom = generateImage' (-500,-500) (500,500) (\(x,y) ->
                    let x' = abs x / 500
                        y' = abs y / 500
                    in Pixel3 x' ((x'*x' + y'*y') / 2) y'
                )
        writeImageRGB (getOutPath "custom" "custom.png") custom
        putStrLn "custom: CUSTOM DONE"

        writeImageRGB (getOutPath "custom" "gradient-2-color-h.png") (simpleGradientH (500,250) red blue)
        writeImageRGB (getOutPath "custom" "gradient-2-color-v.png") (simpleGradientV (250,500) red blue)
        writeImageRGB (getOutPath "custom" "gradient-5-color-h.png") (multiColorGradientH (1000,1000) 0 [blue,red,green,255])
        writeImageRGB (getOutPath "custom" "gradient-5-color-v.png") (multiColorGradientV (1000,1000) 0 [blue,red,green,255])
        putStrLn "custom: GRADIENTS DONE"

