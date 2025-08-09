{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )

import Examples.Animations
import Examples.Images

tryIndex :: [a] -> Int -> Maybe a
tryIndex [] _ = Nothing
tryIndex (x:xs) idx
  | idx < 0  = Nothing
  | idx == 0  = Just x
  | otherwise = tryIndex xs (idx-1)
{-# INLINE tryIndex #-}

main :: IO ()
main = do args <- getArgs

          -- functional animation
          let imgPath1 = args `tryIndex` 0
          mapM_ exampleAnimToGrey imgPath1
          exampleAnimRotateSpiral
          exampleAnimZoomSpiral
          exampleAnimBlurrySpiral
          example2Bodies
          example3Bodies
          exampleGameOfLife

          -- video manipulation
          let videoBasePath = args `tryIndex` 1
          mapM_ exampleManipulateVideo videoBasePath

          -- functional images
          let imgPath2 = args `tryIndex` 2 <|> imgPath1
          mapM_ exampleFImage imgPath2

          -- image manipulation + histograms
          let imgPath3 = args `tryIndex` 3 <|> imgPath2
          mapM_ exampleImageManipulation imgPath3
          mapM_ exampleImageHistograms imgPath3

          -- custom images
          exampleCustomImages
