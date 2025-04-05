-- benchmarking of various convolution processes
module Main where

import Criterion.Main
import System.Directory
import Graphics.Phoskell.Core.Colour
import Graphics.Phoskell.Core.Image
import Graphics.Phoskell.Synthesis (simpleGradientH)
import Graphics.Phoskell.Processes.Convolution

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/convolution/"

-- | Benchmark RGBA image, writing it to @outDir@ with filename given
benchRGBA :: FilePath -> Image RGBA -> Benchmark
benchRGBA fname img = bench fname $ whnf toArrayStorable img

setupEnv :: IO (Image RGBA)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (simpleGradientH (1000,1000) 0 255)

meanFilterBenchmarks :: Image RGBA -> Benchmark
meanFilterBenchmarks img = bgroup "Mean Filter" [
            benchRGBA "mean-3x3.png" (img :> meanFilter 3),
            benchRGBA "mean-7x7.png" (img :> meanFilter 7),
            benchRGBA "mean-11x11.png" (img :> meanFilter 11)
        ]

benchGaussian :: Int -> Image RGBA -> Benchmark
benchGaussian r img = bgroup dims [
            benchRGBA ("gaussian-" ++ dims ++ "-s1.png") (img :> gaussianFilter r 1),
            benchRGBA ("gaussian-" ++ dims ++ "-s3.png") (img :> gaussianFilter r 3),
            benchRGBA ("gaussian-" ++ dims ++ "-s5.png") (img :> gaussianFilter r 5)
        ]
    where
        d = show r
        dims = d ++ "x" ++ d

gaussianBenchmarks :: Image RGBA -> Benchmark
gaussianBenchmarks img = bgroup "Gaussian Blur" [
            benchGaussian 3 img,
            benchGaussian 7 img,
            benchGaussian 11 img
        ]

convBenchmarks :: Image RGBA -> Benchmark
convBenchmarks img = bgroup "Convolution" [
            benchRGBA "unchanged.png" img, -- baseline
            meanFilterBenchmarks img,
            gaussianBenchmarks img
        ]

main :: IO ()
main = defaultMain [
            env setupEnv convBenchmarks
        ]
