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
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

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

timeBetweenStr :: UTCTime -> UTCTime -> String
timeBetweenStr t t' = show $ fromIntegral (floor (diffUTCTime t' t * 10)) / 10

outputMilestoneM :: IO a -> String -> UTCTime -> IO a
outputMilestoneM cmd x t = do
    putStrLn $ "--- manipulation: " ++ x ++ " ---"
    t1 <- getCurrentTime
    v <- cmd
    t2 <- getCurrentTime
    let totalTime = timeBetweenStr t t2
    let cmdTime = timeBetweenStr t1 t2
    putStrLn $ "--- manipulation: " ++ x ++ " (" ++ cmdTime ++ "s) ---"
    putStrLn $ "\n=== " ++ totalTime ++ "s ===\n"
    return v

outputM :: IO a -> String -> IO a
outputM cmd txt = do
    t1 <- getCurrentTime
    v <- cmd
    t2 <- getCurrentTime
    let cmdTime = timeBetweenStr t1 t2
    putStrLn $ "manipulation: " ++ txt ++ " [" ++ cmdTime ++ "s]"
    return v

exampleStart :: Image RGB -> IO ()
exampleStart img = do
    let img' = img
                :> gammaCorrect 0.5
                :> PointProcess rgbToGray
                :> meanFilter 5
                :> threshold 128
    outputM (writeImageRGB "input.png" img) "input"
    outputM (writeImageBinary "output.png" img') "output"

exampleAlphaChannel :: Image RGB -> IO ()
exampleAlphaChannel img = do
    let imgA = img :> addAlphaChannel
    let alphaOverlay = imgA :> overlayImage (simpleGradientH (imageSize imgA) redA greenA :> alterAlpha (const 128))
    let alphaNoRed = imgA :> chromaKeyRemove redA 1
    let alphaJustRed = imgA :> chromaKeyExtract redA 1
    let alphaRecover = (imgA :> chromaKeyRemove redA 1) + (imgA :> chromaKeyExtract redA 1)
    outputM (writeImageRGBA "alpha-overlay.png" alphaOverlay) "alphaOverlay"
    outputM (writeImageRGBA "alpha-no-red.png" alphaNoRed) "alphaNoRed"
    outputM (writeImageRGBA "alpha-just-red.png" alphaJustRed) "alphaJustRed"
    outputM (writeImageRGBA "alpha-recover.png" alphaRecover) "alphaRecover"

exampleStacking :: Image RGB -> IO ()
exampleStacking img = do
    let img2 = img :> scaleXBy 0.5 :> scaleYBy 1.5
    let stackVertical = stackVertically 0 img img2
    let stackHorizontal = stackHorizontally 0 img img2
    let stackQuadrants = quadrants 0 img img2 img2 img
    outputM (writeImageRGB "stack-vertical.png" stackVertical) "stackVertical"
    outputM (writeImageRGB "stack-horizontal.png" stackHorizontal) "stackHorizontal"
    outputM (writeImageRGB "stack-quadrants.png" stackQuadrants) "stackQuadrants"

exampleThreshold :: Image RGB -> IO ()
exampleThreshold img = do
    let imgGray = img :> PointProcess rgbToGray
    outputM (writeImageBinary "output-thresh.png" (imgGray :> threshold 128)) "threshold"
    outputM (writeImageBinary "output-otsu.png" (imgGray :> otsuThreshold)) "otsuThreshold"

examplePoint :: Image RGB -> IO ()
examplePoint img = do
    outputM (writeImageRGB "output-gain.png" (img :> applyGain 2)) "gain"
    outputM (writeImageRGB "output-addbias.png" (img :> addBias 50)) "addBias"
    outputM (writeImageRGB "output-subbias.png" (img :> subtractBias 50)) "subBias"
    outputM (writeImageRGB "output-gain-bias.png" (img :> applyGain 2 :> subtractBias 50)) "gainBias"
    outputM (writeImageRGB "output-adjust-hue.png" (img :> alterHue (+128))) "adjustHue"

exampleTransformations :: Image RGB -> IO ()
exampleTransformations img = do
    outputM (writeImageRGB "output-translate.png" (img :> translate (100,100) 0)) "translate"
    outputM (writeImageRGB "output-translate-wrapped.png" (img :> translateWrap (5000,5000))) "translateWrapped"
    outputM (writeImageRGB "output-shear-x.png" (img :> shearX 0.1 0)) "shearX"
    outputM (writeImageRGB "output-shear-y.png" (img :> shearY 0.1 0)) "shearY"
    outputM (writeImageRGB "output-extract.png" (img :> extractRegion (1500,300) (2500,700) 0)) "extract"
    outputM (writeImageRGB "output-rot90.png" (img :> rotate90)) "rot90"
    outputM (writeImageRGB "output-rot180.png" (img :> rotate180)) "rot180"
    outputM (writeImageRGB "output-crop-res480.png" (img :> cropToSize (854,480))) "cropRes480"
    outputM (writeImageRGB "output-crop-ar43.png" (img :> cropToAspectRatio (4,3))) "cropAr43"
    outputM (writeImageRGB "output-rot90-ar43.png" (img :> rotate90 :> cropToAspectRatio (4,3))) "rot90Ar43"
    outputM (writeImageRGB "output-rot45.png" (img :> rotateDeg 45 0)) "rot45"
    outputM (writeImageRGB "output-scalemul2.png" (img :> scaleBy 2)) "scalemul2"
    outputM (writeImageRGB "output-scalediv10.png" (img :> scaleBy 0.1)) "scalediv10"
    outputM (writeImageRGB "output-scaleXmul2.png" (img :> scaleXBy 2)) "scaleXmul2"
    outputM (writeImageRGB "output-scaleYdiv2.png" (img :> scaleYBy 0.5)) "scaleYdiv2"
    outputM (writeImageRGB "output-scale1080p.png" (img :> scaleTo (1920,1080))) "scale1080p"
    outputM (writeImageRGB "output-zoom-in.png" (img :> zoom 2 0)) "zoomIn"
    outputM (writeImageRGB "output-zoom-out.png" (img :> zoom 0.5 0)) "zoomOut"
    outputM (writeImageRGB "output-zoom-out-slightly.png" (img :> PointProcess (const 0) :> zoomToSize (2049,1187) 255)) "zoomOutSlightly"
    outputM (writeImageRGB "output-letterbox-219.png" (img :> letterboxToAspectRatio (21,9) 0)) "letterbox219"
    outputM (writeImageRGB "output-letterbox-921.png" (img :> letterboxToAspectRatio (9,21) 0)) "letterbox921"

exampleRepeatRotations :: Image RGB -> IO ()
exampleRepeatRotations img = do
    outputM (writeImageRGB "rotated-repeat-cw.png" (rot img 360)) "cw"
    outputM (writeImageRGB "rotated-repeat-acw.png" (rot' img 360)) "acw"
    outputM (writeImageRGB "rotated-repeat-cw-acw.png" (rot' (rot img 360) 360)) "cw-acw"
    outputM (writeImageRGB "rotated-repeat-acw-cw.png" (rot (rot' img 360) 360)) "acw-cw"

exampleConvolution :: Image RGB -> IO ()
exampleConvolution img = do
    outputM (writeImageRGB "mean-filter-5.png" (img :> meanFilter 5)) "mean5"
    outputM (writeImageRGB "mean-filter-10.png" (img :> meanFilter 10)) "mean10"
    outputM (writeImageRGB "mean-filter-20.png" (img :> meanFilter 20)) "mean20"
    outputM (writeImageRGB "gaussian-filter-5-2.png" (img :> gaussianFilter 5 2)) "gaussian5-2"
    outputM (writeImageRGB "gaussian-filter-10-4.png" (img :> gaussianFilter 10 4)) "gaussian10-4"
    outputM (writeImageRGB "gaussian-filter-20-8.png" (img :> gaussianFilter 20 8)) "gaussian20-8"

exampleImageManipulation :: FilePath -> IO ()
exampleImageManipulation fp = do
    startTime <- getCurrentTime
    mkOutFolder "manip"
    img <- outputMilestoneM (outputM (readImageRGB fp) "read") "READ" startTime
    setCurrentDirectory (outBaseFolder ++ "/manip/")

    outputMilestoneM (exampleStart img) "START" startTime
    outputMilestoneM (exampleAlphaChannel img) "ALPHA" startTime
    outputMilestoneM (exampleStacking img) "STACKING" startTime
    outputMilestoneM (exampleThreshold img) "THRESHOLDING" startTime
    outputMilestoneM (examplePoint img) "POINT" startTime
    outputMilestoneM (exampleTransformations img) "TRANSFORMATIONS" startTime
    outputMilestoneM (exampleRepeatRotations img) "REPEATED ROTATIONS" startTime
    outputMilestoneM (exampleConvolution img) "CONVOLUTION" startTime

    setCurrentDirectory "../.." -- leave output directory

-- Histograms + Histogram Manipulation --

outputMilestoneH :: IO a -> String -> UTCTime -> IO a
outputMilestoneH cmd x t = do
    putStrLn $ "--- histogram: " ++ x ++ " ---"
    t1 <- getCurrentTime
    v <- cmd
    t2 <- getCurrentTime
    let totalTime = timeBetweenStr t t2
    let cmdTime = timeBetweenStr t1 t2
    putStrLn $ "--- histogram: " ++ x ++ " (" ++ cmdTime ++ "s) ---"
    putStrLn $ "\n=== " ++ totalTime ++ "s ===\n"
    return v

outputH :: IO a -> String -> IO a
outputH cmd txt = do
    t1 <- getCurrentTime
    v <- cmd
    t2 <- getCurrentTime
    let cmdTime = timeBetweenStr t1 t2
    putStrLn $ "histogram: " ++ txt ++ " [" ++ cmdTime ++ "s]"
    return v

exampleHistManipGray :: Image RGB -> IO ()
exampleHistManipGray img = do
    let imgWorseContrastGray = img :> PointProcess rgbToGray :> PointProcess (fmap (`div` 2))
    outputH (writeImageGray "gray.png" imgWorseContrastGray) "badGray"
    outputH (writeImageRGB "gray-histogram.png" (drawHistogramSingle imgWorseContrastGray (2048,1024) 0 255)) "badGrayHist"
    let stretched = imgWorseContrastGray :> contrastStretch
    outputH (writeImageGray "gray-contrast-stretch.png" stretched) "grayStretch"
    outputH (writeImageRGB "gray-contrast-stretch-histogram.png" (drawHistogramSingle stretched (2048,1024) 0 255)) "grayStretchHist"
    let equalised = imgWorseContrastGray :> equaliseHistogram
    outputH (writeImageGray "gray-equalise-histogram.png" equalised) "grayEqualised"
    outputH (writeImageRGB "gray-equalise-histogram-histogram.png" (drawHistogramSingle equalised (2048,1024) 0 255)) "grayEqualisedHist"

exampleHistManipRGB :: Image RGB -> IO ()
exampleHistManipRGB img = do
    let imgWorseContrastRGB = img :> PointProcess (fmap (`div` 2))
    outputH (writeImageRGB "bad-contrast.png" imgWorseContrastRGB) "badRGB"
    outputH (writeImageRGB "bad-contrast-histogram.png" (drawHistogramsRGBY imgWorseContrastRGB (2048,1024) 0 red green blue 255)) "badRGBHist"
    let stretched = imgWorseContrastRGB :> contrastStretch
    outputH (writeImageRGB "bad-contrast-stretched.png" stretched) "badRGBStretched"
    outputH (writeImageRGB "bad-contrast-stretched-histogram.png" (drawHistogramsRGBY stretched (2048,1024) 0 red green blue 255)) "badRGBStretchedHist"
    let equalised = imgWorseContrastRGB :> equaliseHistogram
    outputH (writeImageRGB "bad-contrast-equalised.png" equalised) "badRGBEqualised"
    outputH (writeImageRGB "bad-contrast-equalised-histogram.png" (drawHistogramsRGBY equalised (2048,1024) 0 red green blue 255)) "badRGBEqualised"

exampleHistGen :: Image RGB -> IO ()
exampleHistGen img = do
    outputH (writeImageRGB "histogram-red.png" (drawHistogramSingle (img :> PointProcess takeRedRGB) (2048,1024) 0 red)) "histRed"
    outputH (writeImageRGB "histogram-green.png" (drawHistogramSingle (img :> PointProcess takeGreenRGB) (2048,1024) 0 green)) "histGreen"
    outputH (writeImageRGB "histogram-blue.png" (drawHistogramSingle (img :> PointProcess takeBlueRGB) (2048,1024) 0 blue)) "histBlue"
    outputH (writeImageRGB "histogram-luma.png" (drawHistogramSingle (img :> PointProcess rgbToGray) (2048,1024) 0 255)) "histLuma"
    outputH (writeImageRGB "histogram-rgby.png" (drawHistogramsRGBY img (4096,2048) 0 red green blue 255)) "histRGBY"

exampleImageHistograms :: FilePath -> IO ()
exampleImageHistograms fp = do
    startTime <- getCurrentTime
    mkOutFolder "histograms"
    img <- outputMilestoneH (outputH (readImageRGB fp) "read") "READ" startTime
    setCurrentDirectory (outBaseFolder ++ "/histograms/")

    outputMilestoneH (exampleHistGen img) "HISTOGRAM GENERATION" startTime
    outputMilestoneH (exampleHistManipGray img) "GRAYSCALE HISTOGRAM MANIPULATION" startTime
    outputMilestoneH (exampleHistManipRGB img) "RGB HISTOGRAM MANIPULATION" startTime

    setCurrentDirectory "../.." -- leave outputM directory

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

