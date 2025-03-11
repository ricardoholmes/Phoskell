-- benchmarking of various point processes
module Main where

import Criterion.Main
import System.Directory
import Graphics.ImageProcessing.IO
import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Synthesis (simpleGradientH)
import Graphics.ImageProcessing.Processes.Convolution

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/"

-- | Benchmark RGBA image, writing it to @outDir@ with filename given
benchRGBA :: FilePath -> Image RGBA -> Benchmark
benchRGBA fname img = bench fname $ nfIO (writeImageRGBA (outDir ++ fname) img)

setupEnv :: IO (Image RGBA)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (simpleGradientH (1000,1000) 0 255)

meanFilterBenchmarks :: Image RGBA -> Benchmark
meanFilterBenchmarks img = bgroup "Mean Filter" [
            benchRGBA "mean-5x5.png" (img :> meanFilter 2),
            benchRGBA "mean-11x11.png" (img :> meanFilter 5),
            benchRGBA "mean-17x17.png" (img :> meanFilter 8)
        ]

benchGaussian :: Int -> Image RGBA -> Benchmark
benchGaussian r img = bgroup dims [
            benchRGBA ("gaussian-" ++ dims ++ "-s2.png") (img :> gaussianFilter r 2),
            benchRGBA ("gaussian-" ++ dims ++ "-s5.png") (img :> gaussianFilter r 5),
            benchRGBA ("gaussian-" ++ dims ++ "-s8.png") (img :> gaussianFilter r 8)
        ]
    where
        d = show (r*2 + 1)
        dims = d ++ "x" ++ d

gaussianBenchmarks :: Image RGBA -> Benchmark
gaussianBenchmarks img = bgroup "Gaussian Blur" [
            benchGaussian 2 img,
            benchGaussian 5 img,
            benchGaussian 8 img
        ]

convBenchmarks :: Image RGBA -> Benchmark
convBenchmarks img = bgroup "Convolution" [
            benchRGBA "convolution-unchanged.png" img, -- baseline
            meanFilterBenchmarks img,
            gaussianBenchmarks img
        ]

main :: IO ()
main = defaultMain [
            env setupEnv convBenchmarks
        ]
