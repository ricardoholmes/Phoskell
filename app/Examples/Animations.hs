{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE NumericUnderscores #-}
module Examples.Animations (
    exampleAnimRotateSpiral,
    exampleAnimToGray,
    exampleManipulateVideo,
    example2Bodies,
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
animBaseFolder = "output-frames"

animOutPath :: String -> Integer -> FilePath
animOutPath folder i = intercalate "/" [animBaseFolder, folder, fname]
    where fname = pad 4 i ++ ".png"

mkAnimFolder :: String -> IO ()
mkAnimFolder folder = createDirectoryIfMissing True path
    where path = intercalate "/" [animBaseFolder, folder]

exampleAnimToGray :: FilePath -> IO ()
exampleAnimToGray path = do mkAnimFolder "gray-anim"
                            imgIn <- readImageRGB path
                            let img = imgIn :> scaleBy 0.25
                            putStrLn "grayAnim: START"
                            let outPath0 = "output-frames/grayAnim0.png"
                            writeImageRGB outPath0 img
                            let outPath1 = "output-frames/grayAnim1.png"
                            writeImageGray outPath1 (img :> PointProcess rgbToGray)
                            putStrLn "grayAnim: BASE IMAGES DONE"
                            anim <- readFAnim [outPath0, outPath1]
                            let sz = imageSize' img
                            let frames = fAnimToImages anim (-1) 0.01 sz 1
                            writeImages (animOutPath "gray-anim") (take 301 frames)
                            putStrLn "grayAnim: DONE"

exampleAnimRotateSpiral :: IO ()
exampleAnimRotateSpiral = do mkAnimFolder "rotate-spiral"
                             let polar (r,t) = if even (round (r/10 + t*5/pi)) then 1 else 0
                             putStrLn "rotSpiral: START"
                             let img = fromPolarF polar
                             let rotF θ = applyTransformF (\(x,y) -> (
                                         x*cos θ + y*sin θ,
                                         y*cos θ - x*sin θ
                                     ))
                             let anim t = rotF t img
                             let frames = fAnimToImages anim 0 0.01 (100,100) 0.1
                             writeImages (animOutPath "rotate-spiral") (take 1000 frames)
                             putStrLn "rotSpiral: DONE"

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

-- N-Body Simulation --

type Vec2 = (Double,Double)

-- color, radius, mass, position, velocity, acceleration
type Bodies = [(Color,Double,Double,Vec2,Vec2,Vec2)]

nBodyStep :: Color -> Bodies -> Double -> (FImage,Bodies)
nBodyStep bg b dt = (\pos -> getColor $ head $ dropWhile (\(_,r,_,d,_,_) -> dist pos d > r) b' ++ [(bg,0,0,(0,0),(0,0),(0,0))], b')
    where
        getColor (c,_,_,_,_,_) = c
        b' = do ((c,r,m1,(dx,dy),(vx,vy),(ax,ay)),rest) <- bSplits
                let d' = (dx + vx*dt, dy + vy*dt)
                let v' = (vx + ax*dt, vy + ay*dt)
                let as = [(f m2 (dist (dx,dy) (d2x,d2y)), atan2 (d2y-dy) (d2x-dx)) | (_,_,m2,(d2x,d2y),_,_) <- rest]
                let as' = [(f' * cos angle, f' * sin angle) | (f',angle) <- as]
                let a' = foldr (\(x,y) (a'x,a'y) -> (a'x + x, a'y + y)) (0,0) as'
                return (c,r,m1,d',v',a')
        bSplits = [(body, take i b ++ drop (i+1) b) | (i,body) <- zip [0..] b]
        f m2 r = g * m2 / (r*r)
        g = 6.6743 / 10^11 -- gravity constant
        dist (d1x,d1y) (d2x,d2y) = sqrt ((d2x-d1x)^2 + (d2y-d1y)^2)

-- background -> bodies -> time step (delta time)
runNBodies :: Color -> Bodies -> Double -> [FImage]
runNBodies bg b dt = img : runNBodies bg b' dt
    where (img,b') = nBodyStep bg b dt

-- animation showing a simulation of the moon orbiting around the earth
example2Bodies :: IO ()
example2Bodies = do mkAnimFolder "two-bodies"
                    let twoBodies = runNBodies 0 [
                                (Pixel4 1 1 1 0, 2_000_000, 7.3476 * 10^22, (384_400_000,0), (0,1000), (0,0)),
                                (Pixel4 0 0 1 0, 6_000_000, 5.972168 * 10^24, (0,0), (0,0), (0,0))
                            ] 3600
                    let twoBodies' = [fImageToImage fimg (1_000_000_000, 1_000_000_000) 1_000_000 | fimg <- twoBodies]
                    writeImages (animOutPath "two-bodies") (take 700 twoBodies')
