{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )
import Safe (atMay)

import Examples.Animations
import Examples.Images
import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Functional
import Graphics.ImageProcessing.IO ( writeImageRGBA )
import System.Directory (createDirectoryIfMissing)
import Data.Fixed (mod')

fimgTest :: FImage
fimgTest = fromPolarF polarFImg
    where polarFImg (r,t) = pure $ mkSmallDouble $ (sin (t + r) + 1) / 2

fanimTest :: FAnim
fanimTest t = fromPolarF polarFAnim
    where
        polarFAnim (r,t') = mkSmallDouble <$> setAlpha1 (color (2 * pi * (t `mod'` 1)) `multScalar` sin' (t*t' + r))
        setAlpha1 (Pixel4 r g b _) = Pixel4 r g b 1
        color :: Double -> Pixel4 Double
        color t' = Pixel4 (sin' t') (sin' (-t')) 0 1
        sin' x = (sin x + 1) / 2

-- given image size and map
gameOfLife :: (Double,Double) -> [[Bool]] -> [FImage]
gameOfLife (w,h) m = fimg : gameOfLife (w,h) m'
    where
        fimg (x,y) = if m !! floor (y + (h/2)) !! floor (x + (w/2))
                            then 1
                            else 0
        neighbours (x,y) = sum [if m !! y' !! x' then 1 else 0 | y' <- [y-1, y, y+1], x' <- [x-1, x, x+1],
                                                   x' /= x || y' /= y,
                                                   x' >= 0 && y' >= 0,
                                                   y' < length m && x' < length (m !! y')]
        m' = [[if c then ns == 2 || ns == 3 else ns == 3 | (x,c) <- zip [0..] r, let ns = neighbours (x,y)] | (y,r) <- zip [0..] m]

-- given image size and list of coords of living cells
gameOfLife' :: (Double,Double) -> [(Int,Int)] -> [Image RGBA]
gameOfLife' sz@(w,h) cs = (\fimg -> fImageToImage fimg sz 0.1) <$> gameOfLife sz m
    where
        w' = round w `div` 2
        h' = round h `div` 2
        m = [[(x,y) `elem` cs | x <- [-w'..w']] | y <- [-h'..h']]

spiralZoom :: FAnim
spiralZoom time = applyTransformF (\(x,y) -> (x*time, y*time)) $ fromPolarF fimg
    where fimg (r,t) = pure $ mkSmallDouble (1 - (sin (5 * t + r) + 1) / 2)

main :: IO ()
main = do args <- getArgs

          createDirectoryIfMissing True "output-images/"
          writeImageRGBA "output-images/test.png" $ fImageToImage fimgTest (5,5) 0.01

          createDirectoryIfMissing True "output-frames/test/"
          writeImages (\i -> "output-frames/test/" ++ show i ++ ".png") $ take 500 (fAnimToImages fanimTest 0 0.01 (5,5) 0.01)

          createDirectoryIfMissing True "output-frames/game-of-life/"
          writeImages (\i -> "output-frames/game-of-life/" ++ show i ++ ".png") $ take 300 (gameOfLife' (100,100) [(0,0),(0,-1),(-1,0),(0,1),(1,1)])

          createDirectoryIfMissing True "output-frames/zoom-spiral/"
          writeImages (\i -> "output-frames/zoom-spiral/" ++ show i ++ ".png") $ take 500 (fAnimToImages spiralZoom 0.01 0.01 (50,50) 0.1)

          -- functional animation
          let imgPath1 = args `atMay` 0
          mapM_ exampleAnimToGray imgPath1
          exampleAnimRotateSpiral
          example2Bodies

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
