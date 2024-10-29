module Main where

import Graphics.MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  someFunc
