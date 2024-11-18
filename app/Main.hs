module Main where

import System.Environment (getArgs)
import Graphics.Image.IO
import Graphics.Image.ImageProcess (PointProcess(PointProcess))
import Graphics.Image (Image(..))
import Graphics.Image.Color (rgbToGray)

main :: IO ()
main = do args <- getArgs
          let fname = head args
          img <- readImageRGB fname
          let img' = img :> PointProcess (1-)
          writeImageRGB "out-inverted.png" img'
          writeImageGray "out-gray.png" (img :> PointProcess rgbToGray)
