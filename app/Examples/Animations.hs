{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE NumericUnderscores #-}
module Examples.Animations (
    exampleAnimToGray,
    exampleAnimRotateSpiral,
    exampleAnimZoomSpiral,
    exampleAnimBlurrySpiral,
    exampleManipulateVideo,
    example2Bodies,
    example3Bodies,
    exampleGameOfLife
) where

import Control.Monad
import Data.List
import Data.Time
import System.Directory
import System.IO

import Graphics.ImageProcessing.IO
import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Core.Colour
import Graphics.ImageProcessing.Processes
import Graphics.ImageProcessing.Processes.Point
import Graphics.ImageProcessing.Processes.Convolution
import Graphics.ImageProcessing.Transformations.Rotation
import Graphics.ImageProcessing.Transformations.Scaling
import Graphics.ImageProcessing.Analysis

import Graphics.ImageProcessing.Functional
import Data.Fixed (mod')

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

exampleAnimZoomSpiral :: IO ()
exampleAnimZoomSpiral = do mkAnimFolder "zoom-spiral"
                           putStrLn "zoomSpiral: START"
                           let fimg (r,t) = pure $ mkSmallDouble (1 - (sin (5 * t + r) + 1) / 2)
                           let spiralZoom time = applyTransformF (\(x,y) -> (x*time, y*time)) $ fromPolarF fimg
                           let frames = fAnimToImages spiralZoom 0.01 0.01 (50,50) 0.1
                           writeImages (animOutPath "zoom-spiral") $ take 500 frames
                           putStrLn "zoomSpiral: DONE"

fanimTest :: FAnim
fanimTest t = fromPolarF polarFAnim
    where
        polarFAnim (r,t') = mkSmallDouble <$> setAlpha1 (colour (2 * pi * (t `mod'` 1)) `multScalar` sin' (t*t' + r))
        setAlpha1 (Pixel4 r g b _) = Pixel4 r g b 1
        colour t' = Pixel4 (sin' t') (sin' (-t')) 0 1
        sin' x = (sin x + 1) / 2

exampleAnimBlurrySpiral :: IO ()
exampleAnimBlurrySpiral = do mkAnimFolder "blurry-spiral"
                             putStrLn "blurrySpiral: START"
                             let frames = fAnimToImages fanimTest 0 0.01 (5,5) 0.01
                             writeImages (animOutPath "blurry-spiral") $ take 500 frames
                             putStrLn "blurrySpiral: DONE"

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
                        let img' = img :> invertColoursNotAlpha :> meanFilter 5 :> rotate90 :> scaleYBy 0.5
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

-- colour, radius, mass, position, velocity, acceleration
type Bodies = [(Colour,Double,Double,Vec2,Vec2,Vec2)]

nBodyStep :: Colour -> Bodies -> Double -> (FImage,Bodies)
nBodyStep bg b dt = (\pos -> getColour $ head $ dropWhile (\(_,r,_,d,_,_) -> dist pos d > r) b' ++ [(bg,0,0,(0,0),(0,0),(0,0))], b')
    where
        getColour (c,_,_,_,_,_) = c
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
runNBodies :: Colour -> Bodies -> Double -> [FImage]
runNBodies bg b dt = img : runNBodies bg b' dt
    where (img,b') = nBodyStep bg b dt

-- animation showing a simulation of the moon orbiting around the earth
example2Bodies :: IO ()
example2Bodies = do mkAnimFolder "two-bodies"
                    let twoBodies = runNBodies 0 [
                                (Pixel4 1 1 1 1, 2_000_000, 7.3476 * 10^22, (384_400_000,0), (0,1000), (0,0)),
                                (Pixel4 0 0 1 1, 6_000_000, 5.972168 * 10^24, (0,0), (0,0), (0,0))
                            ] 3600
                    let twoBodies' = [fImageToImage fimg (1_000_000_000, 1_000_000_000) 1_000_000 | fimg <- twoBodies]
                    writeImages (animOutPath "two-bodies") (take 700 twoBodies')

example3Bodies :: IO ()
example3Bodies = do mkAnimFolder "three-bodies"
                    let threeBodies = runNBodies 0 [
                                (Pixel4 1 0 0 1, 10, 10^10, (30,0), (0,0), (0,0)),
                                (Pixel4 0 1 0 1, 10, 10^10, (0,0), (0,0), (0,0)),
                                (Pixel4 0 0 1 1, 10, 10^10, (0,40), (0,0), (0,0))
                            ] 1
                    let threeBodies' = [fImageToImage fimg (300, 300) 1 | fimg <- threeBodies]
                    writeImages (animOutPath "three-bodies") (take 1000 threeBodies')


-- Game of Life --

-- | given image size and map state
gameOfLife' :: (Double,Double) -> [[Bool]] -> [FImage]
gameOfLife' (w,h) m = fimg : gameOfLife' (w,h) m'
    where
        fimg (x,y) = if m !! floor (y + (h/2)) !! floor (x + (w/2))
                            then 1
                            else 0
        neighbours (x,y) = sum [if m !! y' !! x' then 1 else 0 | y' <- [y-1, y, y+1], x' <- [x-1, x, x+1],
                                                   x' /= x || y' /= y,
                                                   x' >= 0 && y' >= 0,
                                                   y' < length m && x' < length (m !! y')]
        m' = [[if c then ns == 2 || ns == 3 else ns == 3 | (x,c) <- zip [0..] r, let ns = neighbours (x,y)] | (y,r) <- zip [0..] m]

-- | given image size and list of coords of living cells
gameOfLife :: (Double,Double) -> [(Int,Int)] -> [Image RGBA]
gameOfLife sz@(w,h) cs = (\fimg -> fImageToImage fimg sz 0.1) <$> gameOfLife' sz m
    where
        w' = round w `div` 2
        h' = round h `div` 2
        m = [[(x,y) `elem` cs | x <- [-w'..w']] | y <- [-h'..h']]

exampleGameOfLife :: IO ()
exampleGameOfLife = do
        mkAnimFolder "game-of-life"
        putStrLn "gameOfLife: START"
        let frames = gameOfLife (100,100) [(0,0),(0,-1),(-1,0),(0,1),(1,1)]
        writeImages (animOutPath "game-of-life") (take 500 frames)
        putStrLn "gameOfLife: DONE"
