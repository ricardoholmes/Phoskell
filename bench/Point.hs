-- benchmarking of various point processes
module Main where

import Criterion.Main
import System.Directory
import Graphics.Phoskell.Core
import Graphics.Phoskell.Processes.Point
import Graphics.Phoskell.Synthesis (canvas)
import Foreign (Storable)

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/point/"

-- | Benchmark image by computing as storable and evaluating weak-head normal form.
benchImage :: Storable p => FilePath -> Image p -> Benchmark
benchImage fname img = bench fname $ whnf toArrayStorable img

setupEnv :: IO (Image RGBA)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (canvas (1000,1000) 127)

biasBenchmarks :: Image RGBA -> Benchmark
biasBenchmarks img = bgroup "bias" [
        benchImage "bias-add-50.png" (img :> addBias 50),
        benchImage "bias-sub-50.png" (img :> subtractBias 50)
    ]

gainBenchmarks :: Image RGBA -> Benchmark
gainBenchmarks img = bgroup "Gain" [
        benchImage "gain-2.png" (img :> applyGain 2),
        benchImage "gain-0_5.png" (img :> applyGain 0.5)
    ]

gammaBenchmarks :: Image RGBA -> Benchmark
gammaBenchmarks img = bgroup "Gamma Correction" [
        benchImage "gamma-2.png" (img :> gammaCorrect 2),
        benchImage "gamma-0_5.png" (img :> gammaCorrect 0.5)
    ]

invBenchmarks :: Image RGBA -> Benchmark
invBenchmarks img = bgroup "Colour Inversion" [
        benchImage "inv-colours.png" (img :> invertColours),
        benchImage "inv-colours-not-alpha.png" (img :> invertColoursNotAlpha)
    ]

multipleBenchmarks :: Image RGBA -> Benchmark
multipleBenchmarks img = bgroup "Multiple" [
        benchImage "gain2-biasAdd20.png" (img :> gammaCorrect 2 :> addBias 20),
        benchImage "gamma2-gain2-biasSub20.png" (img :> gammaCorrect 2 :> applyGain 2 :> subtractBias 20)
    ]

colourBenchmarks :: Image RGBA -> Benchmark
colourBenchmarks img = bgroup "Colour" [
        benchImage "rgba-rgb.png" (img :> rgbaToRGB),
        benchImage "rgba-hsv.png" (img :> rgbaToHSV),
        benchImage "rgba-hsl.png" (img :> rgbaToHSL),
        benchImage "rgba-grey.png" (img :> rgbaToGrey)
    ]

pointBenchmarks :: Image RGBA -> Benchmark
pointBenchmarks img = bgroup "Point Processes" [
        benchImage "unchanged.png" img, -- baseline
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
