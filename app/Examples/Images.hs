{-# OPTIONS_GHC -Wno-type-defaults #-}
module Examples.Images (
    exampleFImage,
    exampleImageManipulation,
    exampleImageHistograms,
    exampleCustomImages,
) where

import Data.List
import Data.Time
import System.Directory
import Graphics.ImageProcessing.IO
import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Analysis
import Graphics.ImageProcessing.Processes
import Graphics.ImageProcessing.Synthesis
import Graphics.ImageProcessing.Functional
import Graphics.ImageProcessing.Transformations

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

                        let blurrySpiral = fromPolarF (\(r,t) -> pure $ mkSmallDouble $ (sin (t + r) + 1) / 2)
                        writeFImage "blurry-spiral.png" (fImageToImage blurrySpiral (5,5) 0.01)

-- Image Manipulation --

rot :: Image RGB -> Int -> Image RGB
rot img 0 = img
rot img x = rot (img :> rotateDeg 1 black) (x-1)

rot' :: Image RGB -> Int -> Image RGB
rot' img 0 = img
rot' img x = rot' (img :> rotateDeg (-1) black) (x-1)

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
                :> PointProcess rgbToGrey
                :> meanFilter 5
                :> threshold 127
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
    let stackVertical = stackVertically black img img2
    let stackHorizontal = stackHorizontally black img img2
    let stackQuadrants = quadrants black img img2 img2 img
    outputM (writeImageRGB "stack-vertical.png" stackVertical) "stackVertical"
    outputM (writeImageRGB "stack-horizontal.png" stackHorizontal) "stackHorizontal"
    outputM (writeImageRGB "stack-quadrants.png" stackQuadrants) "stackQuadrants"

exampleThreshold :: Image RGB -> IO ()
exampleThreshold img = do
    let imgGrey = img :> PointProcess rgbToGrey
    outputM (writeImageBinary "output-thresh.png" (imgGrey :> threshold 127)) "threshold"
    outputM (writeImageBinary "output-otsu.png" (imgGrey :> otsuThreshold)) "otsuThreshold"

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
    outputM (writeImageRGB "output-shear-x.png" (img :> shearX 0.1 black)) "shearX"
    outputM (writeImageRGB "output-shear-y.png" (img :> shearY 0.1 black)) "shearY"
    outputM (writeImageRGB "output-extract.png" (img :> extractRegion (1500,300) (2500,700) black)) "extract"
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
    outputM (writeImageRGB "output-zoom-in.png" (img :> zoom 2 black)) "zoomIn"
    outputM (writeImageRGB "output-zoom-out.png" (img :> zoom 0.5 black)) "zoomOut"
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

exampleHistManipGrey :: Image RGB -> IO ()
exampleHistManipGrey img = do
    let imgWorseContrastGrey = img :> PointProcess rgbToGrey :> PointProcess (fmap (`div` 2))
    outputH (writeImageGrey "grey.png" imgWorseContrastGrey) "badGrey"
    outputH (writeImageRGB "grey-histogram.png" (drawHistogramSingle imgWorseContrastGrey (2048,1024) black white)) "badGreyHist"
    let stretched = imgWorseContrastGrey :> contrastStretch
    outputH (writeImageGrey "grey-contrast-stretch.png" stretched) "greyStretch"
    outputH (writeImageRGB "grey-contrast-stretch-histogram.png" (drawHistogramSingle stretched (2048,1024) black white)) "greyStretchHist"
    let equalised = imgWorseContrastGrey :> equaliseHistogram
    outputH (writeImageGrey "grey-equalise-histogram.png" equalised) "greyEqualised"
    outputH (writeImageRGB "grey-equalise-histogram-histogram.png" (drawHistogramSingle equalised (2048,1024) black white)) "greyEqualisedHist"

exampleHistManipRGB :: Image RGB -> IO ()
exampleHistManipRGB img = do
    let imgWorseContrastRGB = img :> PointProcess (fmap (`div` 2))
    outputH (writeImageRGB "bad-contrast.png" imgWorseContrastRGB) "badRGB"
    outputH (writeImageRGB "bad-contrast-histogram.png" (drawHistogramsRGBY imgWorseContrastRGB (2048,1024) black red green blue white)) "badRGBHist"
    let stretched = imgWorseContrastRGB :> contrastStretch
    outputH (writeImageRGB "bad-contrast-stretched.png" stretched) "badRGBStretched"
    outputH (writeImageRGB "bad-contrast-stretched-histogram.png" (drawHistogramsRGBY stretched (2048,1024) black red green blue white)) "badRGBStretchedHist"
    let equalised = imgWorseContrastRGB :> equaliseHistogram
    outputH (writeImageRGB "bad-contrast-equalised.png" equalised) "badRGBEqualised"
    outputH (writeImageRGB "bad-contrast-equalised-histogram.png" (drawHistogramsRGBY equalised (2048,1024) black red green blue white)) "badRGBEqualised"

exampleHistGen :: Image RGB -> IO ()
exampleHistGen img = do
    outputH (writeImageRGB "histogram-red.png" (drawHistogramSingle (img :> PointProcess takeRedRGB) (2048,1024) black red)) "histRed"
    outputH (writeImageRGB "histogram-green.png" (drawHistogramSingle (img :> PointProcess takeGreenRGB) (2048,1024) black green)) "histGreen"
    outputH (writeImageRGB "histogram-blue.png" (drawHistogramSingle (img :> PointProcess takeBlueRGB) (2048,1024) black blue)) "histBlue"
    outputH (writeImageRGB "histogram-luma.png" (drawHistogramSingle (img :> PointProcess rgbToGrey) (2048,1024) black white)) "histLuma"
    outputH (writeImageRGB "histogram-rgby.png" (drawHistogramsRGBY img (4096,2048) black red green blue white)) "histRGBY"

exampleImageHistograms :: FilePath -> IO ()
exampleImageHistograms fp = do
    startTime <- getCurrentTime
    mkOutFolder "histograms"
    img <- outputMilestoneH (outputH (readImageRGB fp) "read") "READ" startTime
    setCurrentDirectory (outBaseFolder ++ "/histograms/")

    outputMilestoneH (exampleHistGen img) "HISTOGRAM GENERATION" startTime
    outputMilestoneH (exampleHistManipGrey img) "GreySCALE HISTOGRAM MANIPULATION" startTime
    outputMilestoneH (exampleHistManipRGB img) "RGB HISTOGRAM MANIPULATION" startTime

    setCurrentDirectory "../.." -- leave output directory

-- Custom Images --

outputMilestoneC :: IO a -> String -> UTCTime -> IO a
outputMilestoneC cmd x t = do
    putStrLn $ "--- custom: " ++ x ++ " ---"
    t1 <- getCurrentTime
    v <- cmd
    t2 <- getCurrentTime
    let totalTime = timeBetweenStr t t2
    let cmdTime = timeBetweenStr t1 t2
    putStrLn $ "--- custom: " ++ x ++ " (" ++ cmdTime ++ "s) ---"
    putStrLn $ "\n=== " ++ totalTime ++ "s ===\n"
    return v

outputC :: IO a -> String -> IO a
outputC cmd txt = do
    t1 <- getCurrentTime
    v <- cmd
    t2 <- getCurrentTime
    let cmdTime = timeBetweenStr t1 t2
    putStrLn $ "custom: " ++ txt ++ " [" ++ cmdTime ++ "s]"
    return v

exampleNoiseGen :: IO ()
exampleNoiseGen = do
    outputC (writeImageRGB "salt-pepper-0.png" (saltAndPepperNoise 42 (500,500) 0)) "saltAndPepper-0"
    outputC (writeImageRGB "salt-pepper-01.png" (saltAndPepperNoise 42 (500,500) 0.01)) "salt-pepper-01"
    outputC (writeImageRGB "salt-pepper-25.png" (saltAndPepperNoise 42 (500,500) 0.25)) "salt-pepper-25"
    outputC (writeImageRGB "salt-pepper-50.png" (saltAndPepperNoise 42 (500,500) 0.50)) "salt-pepper-50"
    outputC (writeImageRGB "salt-pepper-75.png" (saltAndPepperNoise 42 (500,500) 0.75)) "salt-pepper-75"
    outputC (writeImageRGB "salt-pepper-99.png" (saltAndPepperNoise 42 (500,500) 0.99)) "salt-pepper-99"
    outputC (writeImageRGB "salt-pepper-1.png" (saltAndPepperNoise 42 (500,500) 1)) "salt-pepper-1"

    outputC (writeImageRGB "uniform-rgb.png" (uniformNoise 42 (500,500))) "uniformRGB"
    outputC (writeImageGrey "uniform-grey.png" (uniformNoise 42 (500,500))) "uniformGrey"
    outputC (writeImageRGB "uniform-rgb-hist.png" (drawHistogramsRGBY (uniformNoise 42 (500,500)) (1024,512) black red green blue white)) "uniformRGB-hist"
    outputC (writeImageRGB "uniform-grey-hist.png" (drawHistogramSingle (uniformNoise 42 (500,500)) (1024,512) black white)) "uniformGrey-hist"

    outputC (writeImageRGB "gaussian-rgb.png" (gaussianNoise 42 (500,500))) "gaussianRGB"
    outputC (writeImageGrey "gaussian-grey.png" (gaussianNoise 42 (500,500))) "gaussianGrey"
    outputC (writeImageRGB "gaussian-rgb-hist.png" (drawHistogramsRGBY (gaussianNoise 42 (500,500)) (1024,512) black red green blue white)) "gaussianRGB-hist"
    outputC (writeImageRGB "gaussian-grey-hist.png" (drawHistogramSingle (gaussianNoise 42 (500,500)) (1024,512) black white)) "gaussianGrey-hist"

exampleCustomImages :: IO ()
exampleCustomImages = do
    mkOutFolder "custom"
    setCurrentDirectory (outBaseFolder ++ "/custom/")
    startTime <- getCurrentTime
    let custom = generateImage' (-500,-500) (500,500) (\(x,y) ->
                let x' = abs x / 500
                    y' = abs y / 500
                in Pixel3 x' ((x'*x' + y'*y') / 2) y'
            )
    writeImageRGB "custom.png" custom
    putStrLn "custom: CUSTOM DONE"

    writeImageRGB "gradient-2-colour-h.png" (simpleGradientH (500,250) red blue)
    writeImageRGB "gradient-2-colour-v.png" (simpleGradientV (250,500) red blue)
    writeImageRGB "gradient-5-colour-h.png" (multiColourGradientH (1000,750) black [blue,red,green,white])
    writeImageRGB "gradient-5-colour-v.png" (multiColourGradientV (750,1000) black [blue,red,green,white])
    putStrLn "custom: GRADIENTS DONE"

    outputMilestoneC exampleNoiseGen "NOISE GENERATION" startTime

    setCurrentDirectory "../.." -- leave output directory
