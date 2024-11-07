module Main where

import Graphics.Image (generateImage)
import Graphics.IO (readImage)
import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          let fname = head args
          img <- readImage fname
          case img of
            Left s -> putStrLn s
            Right img' -> print (generateImage img')
