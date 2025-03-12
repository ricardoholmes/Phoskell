-- benchmarking of various image synthesis/creation functions
module Main where

import Criterion.Main
import System.Directory
import Graphics.ImageProcessing.IO
import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Synthesis
import Graphics.ImageProcessing.Core.Color

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/synthesis/"

setupEnv :: IO (Image RGBA)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (simpleGradientH (1000,1000) redA blueA)

-- | Benchmark RGBA image, writing it to @outDir@ with filename given
benchRGBA :: FilePath -> Image RGBA -> Benchmark
benchRGBA fname img = bench fname $ nfIO (writeImageRGBA (outDir ++ fname) img)

-- | Benchmark RGB image, writing it to @outDir@ with filename given
benchRGB :: FilePath -> Image RGB -> Benchmark
benchRGB fname img = bench fname $ nfIO (writeImageRGB (outDir ++ fname) img)

-- | Benchmark Gray image, writing it to @outDir@ with filename given
benchGray :: FilePath -> Image Gray -> Benchmark
benchGray fname img = bench fname $ nfIO (writeImageGray (outDir ++ fname) img)

canvasBenchmarks :: Benchmark
canvasBenchmarks = bgroup "Canvas" [
            benchRGBA "canvas-black.png" (canvas (1000,1000) 0),
            benchRGBA "canvas-white.png" (canvas (1000,1000) 255)
        ]

benchSimpleGradients :: Benchmark
benchSimpleGradients = bgroup "Simple" [
            bgroup "Horizontal" [
                    benchGray "gradient-gray-h.png" (simpleGradientH (1000,1000) 0 255),
                    benchRGB "gradient-rgb-h.png" (simpleGradientH (1000,1000) 0 255),
                    benchRGBA "gradient-rgba-h.png" (simpleGradientH (1000,1000) 0 255)
                ],
            bgroup "Vertical" [
                    benchGray "gradient-gray-v.png" (simpleGradientV (1000,1000) 0 255),
                    benchRGB "gradient-rgb-v.png" (simpleGradientV (1000,1000) 0 255),
                    benchRGBA "gradient-rgba-v.png" (simpleGradientV (1000,1000) 0 255)
                ]
        ]

benchMultiGradients :: Benchmark
benchMultiGradients = bgroup "Multi-color" [
            bgroup "Horizontal" [
                    benchRGBA "gradient-h-3-color.png" (multiColorGradientH (1000,1000) 0 [255,0]),
                    benchRGBA "gradient-h-5-color.png" (multiColorGradientH (1000,1000) 0 [255,0,255,0]),
                    benchRGBA "gradient-h-8-color.png" (multiColorGradientH (1000,1000) 0 [255,0,255,0,255,0])
                ],
            bgroup "Vertical" [
                    benchRGBA "gradient-v-3-color.png" (multiColorGradientV (1000,1000) 0 [255,0]),
                    benchRGBA "gradient-v-5-color.png" (multiColorGradientV (1000,1000) 0 [255,0,255,0]),
                    benchRGBA "gradient-v-8-color.png" (multiColorGradientV (1000,1000) 0 [255,0,255,0,255,0])
                ]
        ]

gradientBenchmarks :: Benchmark
gradientBenchmarks = bgroup "Gradient" [
            benchSimpleGradients,
            benchMultiGradients
        ]

stackBenchmarks :: Image RGBA -> Benchmark
stackBenchmarks img = bgroup "Stacking" [
            benchRGBA "unchanged.png" img, -- baseline
            benchRGBA "stack-horizontal.png" (stackHorizontally 0 img img),
            benchRGBA "stack-vertical.png" (stackVertically 0 img img),
            benchRGBA "stack-quadrants.png" (quadrants 0 img img img img)
        ]

synthesisBenchmarks :: Image RGBA -> Benchmark
synthesisBenchmarks img = bgroup "Synthesis" [
            canvasBenchmarks,
            gradientBenchmarks,
            stackBenchmarks img
        ]

main :: IO ()
main = defaultMain [
            env setupEnv synthesisBenchmarks
        ]
