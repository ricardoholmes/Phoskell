{-# OPTIONS_GHC -Wno-type-defaults #-}
module FunctionalAnimations (
    fAnimToFImages,
    fAnimToImages,
    writeImages,
    readImages,
    readFImages,
    readFAnimN,
    readFAnim,
) where

import FunctionalImages
import Graphics.ImageProcessing.Core ( RGBA, Pixel(multScalar) )
import Graphics.ImageProcessing (Image)
import Graphics.ImageProcessing.IO (writeImageRGBA, readImageRGBA)
import Control.Monad (zipWithM_)
import Safe (atMay)
import Data.Maybe (fromMaybe)
import Data.Fixed (mod')

-- | Given some point in time, return a functional image.
type FAnim = Double -> FImage

-- | Render a functional animation as a list of functional images.
--
-- Parameters:
-- - Functional animation.
-- - Start time.
-- - Time step per frame.
fAnimToFImages :: FAnim -> Double -> Double -> [FImage]
fAnimToFImages f s step = [f i | i <- [s,s+step..]]

-- | Render a functional animation as a list of RGBA images.
--
-- Parameters:
-- - Functional animation.
-- - Start time.
-- - Time step per frame.
-- - Size of region to be covered in terms @(width, height)@.
-- - Step size.
fAnimToImages :: FAnim -> Double -> Double -> (Double, Double) -> Double -> [Image RGBA]
fAnimToImages f t t' wh s = [fImageToImage (f i) wh s | i <- [t,t+t'..]]

-- | Write a list of images to paths determined by the function given.
--
-- Parameters:
-- - Function that takes a frame's index (starting at 0) and returns that frame's filepath.
-- - List of images to output.
writeImages :: (Integer -> FilePath) -> [Image RGBA] -> IO ()
writeImages pathF = zipWithM_ writeImageRGBA paths
    where paths = map pathF [0..]

readImages :: [FilePath] -> IO [Image RGBA]
readImages = mapM readImageRGBA

readFImages :: [FilePath] -> IO [FImage]
readFImages = mapM (fmap imageToFImage . readImageRGBA)

fImagesToFAnimN :: [FImage] -> FAnim
fImagesToFAnimN fImgs t = fromMaybe (const 0) (fImgs `atMay` floor t)

-- | Read images as a functional animation, rounding to the previous frame if needed.
readFAnimN :: [FilePath] -> IO FAnim
readFAnimN paths = fImagesToFAnimN <$> readFImages paths

fImagesToFAnim :: [FImage] -> FAnim
fImagesToFAnim fImgs t = interpolate lower upper
    where
        lower = getFrame (floor t)
        upper = getFrame (floor t + 1)
        getFrame f = fromMaybe (const 0) (fImgs `atMay` f)
        p = mkSmallDouble $ t `mod'` 1
        interpolate l u xy = (l xy `multScalar` (1 - p)) + (u xy `multScalar` p)

-- | Read images as a functional animation, using interpolation for points between frames.
readFAnim :: [FilePath] -> IO FAnim
readFAnim paths = fImagesToFAnim <$> readFImages paths
