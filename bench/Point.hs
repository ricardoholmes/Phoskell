-- benchmarking of various point processes
module Main where

import Criterion.Main
import System.Directory
import Graphics.Phoskell.Core
import Graphics.Phoskell.Processes.Point
import Graphics.Phoskell.Synthesis (simpleGradientH)
import Graphics.Phoskell.IO.Output
import Graphics.Phoskell.Processes.Threshold (threshold)

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/point/"

-- | Benchmark RGBA image, writing it to 'outDir' with filename given
benchImageRGBA :: FilePath -> Image RGBA -> Benchmark
benchImageRGBA fname img = bench fname $ nfIO (writeImageRGBA (outDir ++ fname) img)

-- | Benchmark RGB image, writing it to 'outDir' with filename given
benchImageRGB :: FilePath -> Image RGB -> Benchmark
benchImageRGB fname img = bench fname $ nfIO (writeImageRGB (outDir ++ fname) img)

-- | Benchmark HSV image, writing it to 'outDir' with filename given
benchImageHSV :: FilePath -> Image HSV -> Benchmark
benchImageHSV fname img = bench fname $ nfIO (writeImageHSV (outDir ++ fname) img)

-- | Benchmark HSL image, writing it to 'outDir' with filename given
benchImageHSL :: FilePath -> Image HSL -> Benchmark
benchImageHSL fname img = bench fname $ nfIO (writeImageHSL (outDir ++ fname) img)

-- | Benchmark Grey image, writing it to 'outDir' with filename given
benchImageGrey :: FilePath -> Image Grey -> Benchmark
benchImageGrey fname img = bench fname $ nfIO (writeImageGrey (outDir ++ fname) img)

-- | Benchmark Binary image, writing it to 'outDir' with filename given
benchImageBinary :: FilePath -> Image Binary -> Benchmark
benchImageBinary fname img = bench fname $ nfIO (writeImageBinary (outDir ++ fname) img)

setupEnv :: IO (Image RGBA)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (simpleGradientH (1000,1000) greenA redA)

biasBenchmarks :: Image RGBA -> Benchmark
biasBenchmarks img = bgroup "bias" [
        benchImageRGBA "bias-add-50.png" (img :> addBias 50),
        benchImageRGBA "bias-sub-50.png" (img :> subtractBias 50)
    ]

gainBenchmarks :: Image RGBA -> Benchmark
gainBenchmarks img = bgroup "Gain" [
        benchImageRGBA "gain-2.png" (img :> applyGain 2),
        benchImageRGBA "gain-0_5.png" (img :> applyGain 0.5)
    ]

gammaBenchmarks :: Image RGBA -> Benchmark
gammaBenchmarks img = bgroup "Gamma Correction" [
        benchImageRGBA "gamma-2.png" (img :> gammaCorrect 2),
        benchImageRGBA "gamma-0_5.png" (img :> gammaCorrect 0.5)
    ]

invBenchmarks :: Image RGBA -> Benchmark
invBenchmarks img = bgroup "Colour Inversion" [
        benchImageRGBA "inv-colours.png" (img :> invertColours),
        benchImageRGBA "inv-colours-not-alpha.png" (img :> invertColoursNotAlpha)
    ]

multipleBenchmarks :: Image RGBA -> Benchmark
multipleBenchmarks img = bgroup "Multiple" [
        benchImageRGBA "gain2-biasAdd20.png" (img :> gammaCorrect 2 :> addBias 20),
        benchImageRGBA "gamma2-gain2-biasSub20.png" (img :> gammaCorrect 2 :> applyGain 2 :> subtractBias 20)
    ]

colourBenchmarks :: Image RGBA -> Benchmark
colourBenchmarks img = bgroup "Colour" [
        benchImageRGB "rgba-rgb.png" (img :> rgbaToRGB),
        benchImageHSV "rgba-hsv.png" (img :> rgbaToHSV),
        benchImageHSL "rgba-hsl.png" (img :> rgbaToHSL),
        benchImageGrey "rgba-grey.png" (img :> rgbaToGrey),
        benchImageBinary "rgba-binary.png" (img :> rgbaToGrey :> threshold 127)
    ]

pointBenchmarks :: Image RGBA -> Benchmark
pointBenchmarks img = bgroup "Point Processes" [
        benchImageRGBA "unchanged.png" img, -- baseline
        biasBenchmarks img,
        gainBenchmarks img,
        gammaBenchmarks img,
        invBenchmarks img,
        multipleBenchmarks img,
        colourBenchmarks img
    ]

main :: IO ()
main = defaultMain [
            env setupEnv pointBenchmarks
        ]
