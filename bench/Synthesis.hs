-- benchmarking of various image synthesis/creation functions
module Main where

import Criterion.Main
import System.Directory
import Graphics.Phoskell.Core.Image
import Graphics.Phoskell.Core.Colour
import Graphics.Phoskell.Synthesis
import Graphics.Phoskell.IO.Output

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/synthesis/"

setupEnv :: IO (Image RGBA)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (simpleGradientH (1000,1000) redA blueA)

-- | Benchmark RGBA image, writing it to 'outDir' with filename given
benchImageRGBA :: FilePath -> Image RGBA -> Benchmark
benchImageRGBA fname img = bench fname $ nfIO (writeImageRGBA (outDir ++ fname) img)

-- | Benchmark RGB image, writing it to 'outDir' with filename given
benchImageRGB :: FilePath -> Image RGB -> Benchmark
benchImageRGB fname img = bench fname $ nfIO (writeImageRGB (outDir ++ fname) img)

-- | Benchmark Grey image, writing it to 'outDir' with filename given
benchImageGrey :: FilePath -> Image Grey -> Benchmark
benchImageGrey fname img = bench fname $ nfIO (writeImageGrey (outDir ++ fname) img)

canvasBenchmarks :: Benchmark
canvasBenchmarks = bgroup "Canvas" [
            benchImageRGBA "canvas-black.png" (canvas (1000,1000) 0),
            benchImageRGBA "canvas-white.png" (canvas (1000,1000) 255)
        ]

benchSimpleGradients :: Benchmark
benchSimpleGradients = bgroup "Simple" [
            bgroup "Horizontal" [
                    benchImageGrey "gradient-grey-h.png" (simpleGradientH (1000,1000) 0 255),
                    benchImageRGB "gradient-rgb-h.png" (simpleGradientH (1000,1000) 0 255),
                    benchImageRGBA "gradient-rgba-h.png" (simpleGradientH (1000,1000) 0 255)
                ],
            bgroup "Vertical" [
                    benchImageGrey "gradient-grey-v.png" (simpleGradientV (1000,1000) 0 255),
                    benchImageRGB "gradient-rgb-v.png" (simpleGradientV (1000,1000) 0 255),
                    benchImageRGBA "gradient-rgba-v.png" (simpleGradientV (1000,1000) 0 255)
                ]
        ]

benchMultiGradients :: Benchmark
benchMultiGradients = bgroup "Multi-colour" [
            bgroup "Horizontal" [
                    benchImageRGBA "gradient-h-3-colour.png" (multiColourGradientH (1000,1000) 0 [255,0]),
                    benchImageRGBA "gradient-h-6-colour.png" (multiColourGradientH (1000,1000) 0 [255,0,255,0,255]),
                    benchImageRGBA "gradient-h-9-colour.png" (multiColourGradientH (1000,1000) 0 [255,0,255,0,255,0,255,0])
                ],
            bgroup "Vertical" [
                    benchImageRGBA "gradient-v-3-colour.png" (multiColourGradientV (1000,1000) 0 [255,0]),
                    benchImageRGBA "gradient-v-6-colour.png" (multiColourGradientV (1000,1000) 0 [255,0,255,0,255]),
                    benchImageRGBA "gradient-v-9-colour.png" (multiColourGradientV (1000,1000) 0 [255,0,255,0,255,0,255,0])
                ]
        ]

gradientBenchmarks :: Benchmark
gradientBenchmarks = bgroup "Gradient" [
            benchSimpleGradients,
            benchMultiGradients
        ]

benchUniform :: Benchmark
benchUniform = bgroup "Uniform" [
                benchImageGrey "noise-uniform-grey.png" (uniformNoise 42 (1000,1000)),
                benchImageRGB "noise-uniform-rgb.png" (uniformNoise 42 (1000,1000)),
                benchImageRGBA "noise-uniform-rgba.png" (uniformNoise 42 (1000,1000))
        ]

benchSaltPepper :: Benchmark
benchSaltPepper = bgroup "Salt And Pepper" [
            benchImageRGBA "noise-salt-pepper-0.png" (saltAndPepperNoise 42 (1000,1000) 0),
            benchImageRGBA "noise-salt-pepper-0_25.png" (saltAndPepperNoise 42 (1000,1000) 0.25),
            benchImageRGBA "noise-salt-pepper-0_5.png" (saltAndPepperNoise 42 (1000,1000) 0.5),
            benchImageRGBA "noise-salt-pepper-0_75.png" (saltAndPepperNoise 42 (1000,1000) 0.75),
            benchImageRGBA "noise-salt-pepper-1.png" (saltAndPepperNoise 42 (1000,1000) 1)
        ]

benchGaussian :: Benchmark
benchGaussian = bgroup "Gaussian" [
                benchImageGrey "noise-gaussian-grey.png" (gaussianNoise 42 (1000,1000)),
                benchImageRGB "noise-gaussian-rgb.png" (gaussianNoise 42 (1000,1000)),
                benchImageRGBA "noise-gaussian-rgba.png" (gaussianNoise 42 (1000,1000))
        ]

noiseBenchmarks :: Benchmark
noiseBenchmarks = bgroup "Noise" [
            benchUniform,
            benchSaltPepper,
            benchGaussian
        ]

stackBenchmarks :: Image RGBA -> Benchmark
stackBenchmarks img = bgroup "Stacking" [
            benchImageRGBA "unchanged.png" img, -- baseline
            benchImageRGBA "stack-horizontal.png" (stackHorizontally 0 img img),
            benchImageRGBA "stack-vertical.png" (stackVertically 0 img img),
            benchImageRGBA "stack-quadrants.png" (quadrants 0 img img img img)
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
