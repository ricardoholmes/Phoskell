-- benchmarking of various histogram-related functions and processes
module Main where

import Criterion.Main
import System.Directory
import Graphics.Phoskell.Core
import Graphics.Phoskell.Synthesis (simpleGradientH, drawHistogramsQuad, drawHistogramSingle)
import Graphics.Phoskell.Processes.Histogram (contrastStretch, equaliseHistogram)
import Graphics.Phoskell.Analysis.Histogram

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/histogram/"

-- | Benchmark RGBA image, writing it to @outDir@ with filename given
benchRGBA :: FilePath -> Image RGBA -> Benchmark
benchRGBA fname img = bench fname $ whnf toArrayStorable img

setupEnv :: IO (Image RGBA)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (simpleGradientH (1000,1000) 100 200)

calcHistBenchmarks :: Image RGBA -> Benchmark
calcHistBenchmarks img = bgroup "Calculation" [
        bench "histogram RGBA" $ nf histogram img,
        bench "histogram4 RGBA" $ nf histogram4 img,
        bench "histogram red" $ nf histogram (img :> takeRedRGBA),
        bench "histogram green" $ nf histogram (img :> takeGreenRGBA),
        bench "histogram blue" $ nf histogram (img :> takeBlueRGBA),
        bench "histogram alpha" $ nf histogram (img :> takeAlphaRGBA),
        bench "histogram grey" $ nf histogram (img :> rgbaToGrey)
    ]

contrastAdjBenchmarks :: Image RGBA -> Benchmark
contrastAdjBenchmarks img = bgroup "Contrast Adjustment" [
        benchRGBA "unchanged.png" img, -- baseline
        benchRGBA "contrast-stretch.png" (img :> contrastStretch),
        benchRGBA "equalise-histogram.png" (img :> equaliseHistogram)
    ]

drawHistBenchmarks :: Image RGBA -> Benchmark
drawHistBenchmarks img = bgroup "Generation" [
        benchRGBA "histogram-red.png" (drawHistogramSingle (img :> takeRedRGBA) (1000,1000) blackA redA),
        benchRGBA "histogram-blue.png" (drawHistogramSingle (img :> takeBlueRGBA) (1000,1000) blackA blueA),
        benchRGBA "histogram-green.png" (drawHistogramSingle (img :> takeGreenRGBA) (1000,1000) blackA greenA),
        benchRGBA "histogram-alpha.png" (drawHistogramSingle (img :> takeAlphaRGBA) (1000,1000) blackA whiteA),
        benchRGBA "histogram-grey.png" (drawHistogramSingle (img :> rgbaToGrey) (1000,1000) blackA whiteA),
        benchRGBA "histogram-rgba.png" (drawHistogramsQuad img (500,500) blackA redA blueA greenA yellowA)
    ]

histBenchmarks :: Image RGBA -> Benchmark
histBenchmarks img = bgroup "Histograms" [
        calcHistBenchmarks img,
        contrastAdjBenchmarks img,
        drawHistBenchmarks img
    ]

main :: IO ()
main = defaultMain [
            env setupEnv histBenchmarks
        ]
