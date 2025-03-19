-- benchmarking of various point processes
module Main where

import Criterion.Main
import System.Directory
import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Core.Image
import Graphics.ImageProcessing.Processes.Point
import Graphics.ImageProcessing.Synthesis (canvas)

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/point/"

-- | Benchmark RGBA image, writing it to @outDir@ with filename given
benchRGBA :: FilePath -> Image RGBA -> Benchmark
benchRGBA fname img = bench fname $ whnf toArrayUnboxed img

setupEnv :: IO (Image RGBA)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (canvas (1000,1000) 127)

biasBenchmarks :: Image RGBA -> Benchmark
biasBenchmarks img = bgroup "bias" [
        benchRGBA "bias-add-50.png" (img :> addBias 50),
        benchRGBA "bias-sub-50.png" (img :> subtractBias 50)
    ]

gainBenchmarks :: Image RGBA -> Benchmark
gainBenchmarks img = bgroup "Gain" [
        benchRGBA "gain-2.png" (img :> applyGain 2),
        benchRGBA "gain-0_5.png" (img :> applyGain 0.5)
    ]

gammaBenchmarks :: Image RGBA -> Benchmark
gammaBenchmarks img = bgroup "Gamma Correction" [
        benchRGBA "gamma-2.png" (img :> gammaCorrect 2),
        benchRGBA "gamma-0_5.png" (img :> gammaCorrect 0.5)
    ]

invBenchmarks :: Image RGBA -> Benchmark
invBenchmarks img = bgroup "Colour Inversion" [
        benchRGBA "inv-colours.png" (img :> invertColours),
        benchRGBA "inv-colours-not-alpha.png" (img :> invertColoursNotAlpha)
    ]

multipleBenchmarks :: Image RGBA -> Benchmark
multipleBenchmarks img = bgroup "Multiple" [
        benchRGBA "gain2-biasAdd20.png" (img :> gammaCorrect 2 :> addBias 20),
        benchRGBA "gamma2-gain2-biasSub20.png" (img :> gammaCorrect 2 :> applyGain 2 :> subtractBias 20)
    ]

pointBenchmarks :: Image RGBA -> Benchmark
pointBenchmarks img = bgroup "Point Processes" [
        benchRGBA "unchanged.png" img, -- baseline
        biasBenchmarks img,
        gainBenchmarks img,
        gammaBenchmarks img,
        invBenchmarks img,
        multipleBenchmarks img
    ]

main :: IO ()
main = defaultMain [
            env setupEnv pointBenchmarks
        ]
