module Main where

import System.Environment ( getArgs )
import Graphics.Image.IO
import Graphics.Image.ImageProcess ( PointProcess(PointProcess) )
import Graphics.Image
import Graphics.Image.Color ( rgbToGray, takeRedRGB,  takeGreenRGB, takeBlueRGB )
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
          let img' = generateImage (Sz2 1000 1000) (\(y :. x) ->
                let x' = fromIntegral (abs (x - 500)) / 500
                    y' = fromIntegral (abs (y - 500)) / 500
                in Pixel3 x' ((x'*x' + y'*y') / 2) y')
          writeImageRGB "custom.png" img'
          writeImageGray "custom-Red.png" (img' :> PointProcess takeRedRGB)
          writeImageGray "custom-Green.png" (img' :> PointProcess takeGreenRGB)
          writeImageGray "custom-Blue.png" (img' :> PointProcess takeBlueRGB)
