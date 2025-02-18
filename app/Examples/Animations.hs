{-# OPTIONS_GHC -Wno-type-defaults #-}
module Examples.Animations (
    exampleAnimRotateSpiral,
    exampleAnimToGray,
    exampleManipulateVideo,
) where

import Control.Monad
import Data.List
import Data.Time
import System.Directory
import System.IO

import Graphics.ImageProcessing.IO
import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Core.Color
import Graphics.ImageProcessing.Processes
import Graphics.ImageProcessing.Processes.Point
import Graphics.ImageProcessing.Processes.Convolution
import Graphics.ImageProcessing.Transformations.Rotation
import Graphics.ImageProcessing.Transformations.Scaling
import Graphics.ImageProcessing.Analysis

import FunctionalImages
import FunctionalAnimations

pad :: Show a => Int -> a -> String
pad n x = reverse $ take n x'
    where x' = reverse (show x) ++ repeat '0'

-- ANIMATION GENERATION --

animBaseFolder :: String
animBaseFolder = "outputs"

animOutPath :: String -> Integer -> FilePath
animOutPath folder i = intercalate "/" [animBaseFolder, folder, fname]
    where fname = pad 4 i ++ ".png"

mkAnimFolder :: String -> IO ()
mkAnimFolder folder = createDirectoryIfMissing True path
    where path = intercalate "/" [animBaseFolder, folder]

exampleAnimRotateSpiral :: IO ()
exampleAnimRotateSpiral = do let polar (r,t) = if even (round (r/10 + t*5/pi)) then 1 else 0
                             let img = fromPolarF polar
                             let rotF θ = applyTransformF (\(x,y) -> (
                                         x*cos θ + y*sin θ,
                                         y*cos θ - x*sin θ
                                     ))
                             let anim t = rotF t img
                             let frames = fAnimToImages anim 0 0.01 (100,100) 0.1
                             mkAnimFolder "rotate-spiral"
                             writeImages (animOutPath "rotate-spiral") (take 1000 frames)

exampleAnimToGray :: FilePath -> IO ()
exampleAnimToGray path = do imgIn <- readImageRGB path
                            let img = imgIn :> scaleBy 0.25
                            putStrLn "grayAnim: start"
                            let outPath0 = "outputs/grayAnim0.png"
                            writeImageRGB outPath0 img
                            let outPath1 = "outputs/grayAnim1.png"
                            writeImageGray outPath1 (img :> PointProcess rgbToGray)
                            putStrLn "grayAnim: base images done"
                            anim <- readFAnim [outPath0, outPath1]
                            let sz = imageSize' img
                            let frames = fAnimToImages anim (-1) 0.01 sz 1
                            mkAnimFolder "gray-anim"
                            writeImages (animOutPath "gray-anim") (take 301 frames)
                            putStrLn "grayAnim: done"

-- VIDEO MANIPULATION --

percent :: Int -> Int -> String
percent x y = show (fromIntegral (floor (1000 * x' / y')) / 10) ++ "%"
    where
        x' = fromIntegral x
        y' = fromIntegral y

getPathInput :: String -> Int -> String
getPathInput p x = p ++ "/input/output_" ++ pad 5 x ++ ".jpg"

getPathOutput :: String -> Int -> String
getPathOutput p x = p ++ "/output/output_" ++ pad 5 x ++ ".jpg"

alterVideo :: String -> UTCTime -> Int -> Int -> IO ()
alterVideo p t c n = do let inpPath = getPathInput p n
                        img <- readImageRGBA inpPath
                        let outPath = getPathOutput p n
                        let img' = img :> invertColorsNotAlpha :> meanFilter 5 :> rotate90 :> scaleYBy 0.5
                        writeImageRGBA outPath img'
                        t' <- getCurrentTime
                        let elapsed = fromIntegral (floor (diffUTCTime t' t * 10)) / 10
                        putStrLn ("\r" ++ inpPath ++ " --> " ++ outPath ++ " (" ++ show elapsed ++ "s)")
                        putStr (pad 4 n ++ "/" ++ show c ++ " | " ++ percent n c ++ " (" ++ show elapsed ++ "s)")
                        _ <- hFlush stdout
                        unless (n >= c) $ alterVideo p t c (n+1)

exampleManipulateVideo :: String -> IO ()
exampleManipulateVideo dir = do t <- getCurrentTime
                                inpDir <- listDirectory (dir ++ "/input/")
                                alterVideo dir t (length inpDir) 1
                                putStrLn ""
