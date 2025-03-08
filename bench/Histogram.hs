-- benchmarking of various point processes
module Main where

import Criterion.Main
import System.Directory
import Graphics.ImageProcessing.IO
import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Synthesis (simpleGradientH, drawHistogramsQuad, drawHistogramSingle)
import Graphics.ImageProcessing.Core.Color
import Graphics.ImageProcessing.Processes.Histogram (contrastStretch, equaliseHistogram)
import Graphics.ImageProcessing.Analysis.Histogram

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/"

-- | Benchmark RGBA image, writing it to @outDir@ with filename given
benchRGBA :: FilePath -> Image RGBA -> Benchmark
benchRGBA fname img = bench fname $ nfIO (writeImageRGBA (outDir ++ fname) img)

setupEnv :: IO (Image RGBA)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (simpleGradientH (1000,1000) 100 200)

calcHistBenchmarks :: Image RGBA -> Benchmark
calcHistBenchmarks img = bgroup "Calculation" [
        bench "histogram RGBA" $ nf histogram img,
        bench "histogram4 RGBA" $ nf histogram4 img,
        bench "histogram red" $ nf histogram (img :> takeRedRGBA),
        bench "histogram gray" $ nf histogram (img :> rgbaToGray)
    ]

contrastAdjBenchmarks :: Image RGBA -> Benchmark
contrastAdjBenchmarks img = bgroup "Contrast Adjustment" [
        benchRGBA "contrast-stretch.png" (img :> contrastStretch),
        benchRGBA "equalise-histogram.png" (img :> equaliseHistogram)
    ]

drawHistBenchmarks :: Image RGBA -> Benchmark
drawHistBenchmarks img = bgroup "Generation" [
        benchRGBA "hist-red.png" (drawHistogramSingle (img :> takeRedRGBA) (1000,1000) 0 redA),
        benchRGBA "hist-blue.png" (drawHistogramSingle (img :> takeBlueRGBA) (1000,1000) 0 blueA),
        benchRGBA "hist-green.png" (drawHistogramSingle (img :> takeGreenRGBA) (1000,1000) 0 greenA),
        benchRGBA "hist-alpha.png" (drawHistogramSingle (img :> takeAlphaRGBA) (1000,1000) 0 255),
        benchRGBA "hist-gray.png" (drawHistogramSingle (img :> rgbaToGray) (1000,1000) 0 255),
        benchRGBA "hist-rgba.png" (drawHistogramsQuad img (500,500) 0 redA blueA greenA (Pixel4 255 255 0 255))
    ]

histBenchmarks :: Image RGBA -> Benchmark
histBenchmarks img = bgroup "Histograms" [
        calcHistBenchmarks img,
        contrastAdjBenchmarks img,
        drawHistBenchmarks img
    ]

main :: IO ()
main = do
        defaultMain [
                env setupEnv histBenchmarks
            ]
