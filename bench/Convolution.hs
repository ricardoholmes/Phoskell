-- benchmarking of various convolution processes
module Main where

import Criterion.Main
import System.Directory
import Graphics.Phoskell.Core.Colour
import Graphics.Phoskell.Core.Image
import Graphics.Phoskell.Synthesis (simpleGradientH)
import Graphics.Phoskell.Processes.Convolution
import Graphics.Phoskell.IO.Output (writeImageRGB)

-- | Folder name for outputting images
outDir :: FilePath
outDir = "bench-out/convolution/"

-- | Benchmark RGB image, writing it to 'outDir' with filename given
benchRGB :: FilePath -> Image RGB -> Benchmark
benchRGB fname img = bench fname $ nfIO (writeImageRGB (outDir ++ fname) img)

setupEnv :: IO (Image RGB)
setupEnv = do
        createDirectoryIfMissing True outDir
        return (simpleGradientH (1000,1000) 0 255)

meanFilterBenchmarks :: Image RGB -> Benchmark
meanFilterBenchmarks img = bgroup "Mean Filter" [
            benchRGB "mean-3x3.png" (img :> meanFilter 3),
            benchRGB "mean-5x5.png" (img :> meanFilter 5),
            benchRGB "mean-7x7.png" (img :> meanFilter 7)
        ]

benchGaussian :: Int -> Image RGB -> Benchmark
benchGaussian r img = bgroup dims [
            benchRGB ("gaussian-" ++ dims ++ "-s1.png") (img :> gaussianFilter r 1),
            benchRGB ("gaussian-" ++ dims ++ "-s3.png") (img :> gaussianFilter r 3),
            benchRGB ("gaussian-" ++ dims ++ "-s5.png") (img :> gaussianFilter r 5)
        ]
    where
        d = show r
        dims = d ++ "x" ++ d

gaussianBenchmarks :: Image RGB -> Benchmark
gaussianBenchmarks img = bgroup "Gaussian Blur" [
            benchGaussian 3 img,
            benchGaussian 5 img,
            benchGaussian 7 img
        ]

convBenchmarks :: Image RGB -> Benchmark
convBenchmarks img = bgroup "Convolution" [
            benchRGB "unchanged.png" img, -- baseline
            meanFilterBenchmarks img,
            gaussianBenchmarks img
        ]

main :: IO ()
main = defaultMain [
            env setupEnv convBenchmarks
        ]
