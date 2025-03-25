{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )
import Safe (atMay)

import Examples.Animations
import Examples.Images

main :: IO ()
main = do args <- getArgs

          -- functional animation
          let imgPath1 = args `atMay` 0
          mapM_ exampleAnimToGrey imgPath1
          exampleAnimRotateSpiral
          exampleAnimZoomSpiral
          exampleAnimBlurrySpiral
          example2Bodies
          example3Bodies
          exampleGameOfLife

          -- video manipulation
          let videoBasePath = args `atMay` 1
          mapM_ exampleManipulateVideo videoBasePath

          -- functional images
          let imgPath2 = args `atMay` 2 <|> imgPath1
          mapM_ exampleFImage imgPath2

          -- image manipulation + histograms
          let imgPath3 = args `atMay` 3 <|> imgPath2
          mapM_ exampleImageManipulation imgPath3
          mapM_ exampleImageHistograms imgPath3

          -- custom images
          exampleCustomImages
