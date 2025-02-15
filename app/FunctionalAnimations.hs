module FunctionalAnimations (
    fAnimToFImages,
    fAnimToImages,
    writeImages,
) where

import FunctionalImages
import Graphics.ImageProcessing.Core (RGBA)
import Graphics.ImageProcessing (Image)
import Graphics.ImageProcessing.IO (writeImageRGBA)

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
-- - Function that takes the frame's index (starting at 1) and returns a filename.
-- - List of images to output.
writeImages :: (Integer -> String) -> [Image RGBA] -> IO ()
writeImages pathF imgs = sequence_ [writeImageRGBA (pathF i) img | (i,img) <- zip [1..] imgs]
