{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )
import Safe (atMay)
import System.Directory ( removeFile )

import Examples.Animations
import Examples.Images
import Graphics.Phoskell.IO
import Graphics.Phoskell.Core
import Graphics.Phoskell.Processes
import Data.Time
import Data.List (sort)
import Data.Fixed (mod')
import System.Directory.Internal.Prelude (isDoesNotExistError)
import Control.Exception

timeBetweenMillis :: UTCTime -> UTCTime -> Int
timeBetweenMillis t t' = floor (diffUTCTime t' t * 1000)

test :: IO Int
test = do removeFile "output.jpg" `catch` handleExists
          t <- getCurrentTime
          img <- readImageRGB "input.jpg"
          let img' = img
                     :> gammaCorrect 0.5
                     :> PointProcess rgbToGrey
                     :> meanFilter 5
                     :> threshold 127
          -- print $ sum (toArray img)
          writeImageBinary "output.jpg" img'
          t' <- getCurrentTime
          let ms = timeBetweenMillis t t'
          putStrLn (show ms ++ "ms")
          return ms
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

percentile :: Int -> [Int] -> Int
percentile p ls
 | p < 0 || p > 100 = undefined
 | otherwise = val
    where
        ls' = sort ls
        len = fromIntegral (length ls) - 1
        p' = fromIntegral p / 100
        pos = p' * len
        m = pos `mod'` 1
        val
          | m == 0     = ls' !! floor pos
          | pos >= len = ls' !! floor pos
          | pos < 0    = ls' !! ceiling pos
          | otherwise  = round $ sum (zipWith (*) [1 - m, m] $ fromIntegral <$> drop (floor pos) ls')

main :: IO ()
main = do ts <- traverse (const test) $ replicate 11 ()
          putStrLn ""
          print ts
          print $ sort ts
          putStrLn (' ':unwords [replicate 4 (if or [(length ts - 1) * j `div` 100 == i || length ts * j `div` 100 == i | j <- [0,25,50,75,1]] then '-' else ' ') | i <- [1..length ts]])
          putStrLn ""
          putStrLn ("Mean: " ++ show (sum ts `div` length ts) ++ "ms")
          putStrLn ""
          putStrLn "Percentiles"
          putStrLn ("0th   : " ++ show (percentile   0 ts) ++ "ms")
          putStrLn ("25th  : " ++ show (percentile  25 ts) ++ "ms")
          putStrLn ("Median: " ++ show (percentile  50 ts) ++ "ms")
          putStrLn ("75th  : " ++ show (percentile  75 ts) ++ "ms")
          putStrLn ("100th : " ++ show (percentile 100 ts) ++ "ms")

-- main :: IO ()
-- main = do args <- getArgs

--           -- functional animation
--           let imgPath1 = args `atMay` 0
--           mapM_ exampleAnimToGrey imgPath1
--           exampleAnimRotateSpiral
--           exampleAnimZoomSpiral
--           exampleAnimBlurrySpiral
--           example2Bodies
--           example3Bodies
--           exampleGameOfLife

--           -- video manipulation
--           let videoBasePath = args `atMay` 1
--           mapM_ exampleManipulateVideo videoBasePath

--           -- functional images
--           let imgPath2 = args `atMay` 2 <|> imgPath1
--           mapM_ exampleFImage imgPath2

--           -- image manipulation + histograms
--           let imgPath3 = args `atMay` 3 <|> imgPath2
--           mapM_ exampleImageManipulation imgPath3
--           mapM_ exampleImageHistograms imgPath3

--           -- custom images
--           exampleCustomImages
