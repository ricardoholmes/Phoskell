-- benchmarking of various image synthesis/creation functions
module Main where

import Criterion.Main
import System.Directory
import Graphics.Phoskell.Core.Image
import Graphics.Phoskell.Core.Colour
import Graphics.Phoskell.Synthesis

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/synthesis/"

setupEnv :: IO (Image RGBA)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (simpleGradientH (1000,1000) redA blueA)

-- | Benchmark RGBA image, writing it to @outDir@ with filename given
benchRGBA :: FilePath -> Image RGBA -> Benchmark
benchRGBA fname img = bench fname $ whnf toArrayUnboxed img

-- | Benchmark RGB image, writing it to @outDir@ with filename given
benchRGB :: FilePath -> Image RGB -> Benchmark
benchRGB fname img = bench fname $ whnf toArrayUnboxed img

-- | Benchmark Grey image, writing it to @outDir@ with filename given
benchGrey :: FilePath -> Image Grey -> Benchmark
benchGrey fname img = bench fname $ whnf toArrayUnboxed img

canvasBenchmarks :: Benchmark
canvasBenchmarks = bgroup "Canvas" [
            benchRGBA "canvas-black.png" (canvas (1000,1000) 0),
            benchRGBA "canvas-white.png" (canvas (1000,1000) 255)
        ]

benchSimpleGradients :: Benchmark
benchSimpleGradients = bgroup "Simple" [
            bgroup "Horizontal" [
                    benchGrey "gradient-grey-h.png" (simpleGradientH (1000,1000) 0 255),
                    benchRGB "gradient-rgb-h.png" (simpleGradientH (1000,1000) 0 255),
                    benchRGBA "gradient-rgba-h.png" (simpleGradientH (1000,1000) 0 255)
                ],
            bgroup "Vertical" [
                    benchGrey "gradient-grey-v.png" (simpleGradientV (1000,1000) 0 255),
                    benchRGB "gradient-rgb-v.png" (simpleGradientV (1000,1000) 0 255),
                    benchRGBA "gradient-rgba-v.png" (simpleGradientV (1000,1000) 0 255)
                ]
        ]

benchMultiGradients :: Benchmark
benchMultiGradients = bgroup "Multi-colour" [
            bgroup "Horizontal" [
                    benchRGBA "gradient-h-3-colour.png" (multiColourGradientH (1000,1000) 0 [255,0]),
                    benchRGBA "gradient-h-6-colour.png" (multiColourGradientH (1000,1000) 0 [255,0,255,0,255]),
                    benchRGBA "gradient-h-9-colour.png" (multiColourGradientH (1000,1000) 0 [255,0,255,0,255,0,255,0])
                ],
            bgroup "Vertical" [
                    benchRGBA "gradient-v-3-colour.png" (multiColourGradientV (1000,1000) 0 [255,0]),
                    benchRGBA "gradient-v-6-colour.png" (multiColourGradientV (1000,1000) 0 [255,0,255,0,255]),
                    benchRGBA "gradient-v-9-colour.png" (multiColourGradientV (1000,1000) 0 [255,0,255,0,255,0,255,0])
                ]
        ]

gradientBenchmarks :: Benchmark
gradientBenchmarks = bgroup "Gradient" [
            benchSimpleGradients,
            benchMultiGradients
        ]

benchUniform :: Benchmark
benchUniform = bgroup "Uniform" [
                benchGrey "noise-uniform-grey.png" (uniformNoise 42 (1000,1000)),
                benchRGB "noise-uniform-rgb.png" (uniformNoise 42 (1000,1000)),
                benchRGBA "noise-uniform-rgba.png" (uniformNoise 42 (1000,1000))
        ]

benchSaltPepper :: Benchmark
benchSaltPepper = bgroup "Salt And Pepper" [
            benchRGBA "noise-salt-pepper-0.png" (saltAndPepperNoise 42 (1000,1000) 0),
            benchRGBA "noise-salt-pepper-0_25.png" (saltAndPepperNoise 42 (1000,1000) 0.25),
            benchRGBA "noise-salt-pepper-0_5.png" (saltAndPepperNoise 42 (1000,1000) 0.5),
            benchRGBA "noise-salt-pepper-0_75.png" (saltAndPepperNoise 42 (1000,1000) 0.75),
            benchRGBA "noise-salt-pepper-1.png" (saltAndPepperNoise 42 (1000,1000) 1)
        ]

benchGaussian :: Benchmark
benchGaussian = bgroup "Gaussian" [
                benchGrey "noise-gaussian-grey.png" (gaussianNoise 42 (1000,1000)),
                benchRGB "noise-gaussian-rgb.png" (gaussianNoise 42 (1000,1000)),
                benchRGBA "noise-gaussian-rgba.png" (gaussianNoise 42 (1000,1000))
        ]

noiseBenchmarks :: Benchmark
noiseBenchmarks = bgroup "Noise" [
            benchUniform,
            benchSaltPepper,
            benchGaussian
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
            noiseBenchmarks,
            stackBenchmarks img
        ]

main :: IO ()
main = defaultMain [
            env setupEnv synthesisBenchmarks
        ]
