module Main where

import System.Environment (getArgs)
import Graphics.Image.IO (readImageRGB, writeImageRGB)
import Graphics.Image.ImageProcess (PointProcess(PointProcess))
import Graphics.Image (Image(..))

main :: IO ()
main = do args <- getArgs
          let fname = head args
          img <- readImageRGB fname
          let img' = img :> PointProcess (1-)
          writeImageRGB "out.png" img'
