module Main where

import System.Environment (getArgs)
import Graphics.Image.IO
import Graphics.Image.ImageProcess (PointProcess(PointProcess))
import Graphics.Image (Image(..))
import Graphics.Image.Color (rgbToGray)
import Graphics.Image.Pixel

main :: IO ()
main = do args <- getArgs
          let fname = head args
          img <- readImageRGB fname
          let imgInv = img :> PointProcess (1-)
          let imgGray = img :> PointProcess rgbToGray
          let imgThresh = img :> PointProcess rgbToGray :> PointProcess (\(Pixel1 x) -> Pixel1 (x > 0.5))
          putStrLn "Starting original"
          writeImageRGB "out-original.png" img
          putStrLn "Finished original"
          putStrLn "Starting inverted"
          writeImageRGB "out-inverted.png" imgInv
          putStrLn "Finished inverted"
          putStrLn "Starting gray"
          writeImageGray "out-gray.png" imgGray
          putStrLn "Finished gray"
          putStrLn "Starting thresholding"
          writeImageBinary "out-thresh.png" imgThresh
          putStrLn "Finished thresholding"
