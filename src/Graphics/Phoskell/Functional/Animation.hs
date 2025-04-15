-- | Functional animation type and associated functions.
module Graphics.Phoskell.Functional.Animation (
    FAnim,
    fAnimToFImages,
    fAnimToImages,
    writeImages,
    readImages,
    readFImages,
    fImagesToFAnimN,
    fImagesToFAnim,
    readFAnimN,
    readFAnim,
) where

import Control.Monad (zipWithM_)
import Safe (atMay)
import Data.Maybe (fromMaybe)
import Data.Fixed (mod')

import Graphics.Phoskell.Core.Colour ( RGBA )
import Graphics.Phoskell.Core.Image ( Image )
import Graphics.Phoskell.Core.Pixel ( multScalar )
import Graphics.Phoskell.IO.Input ( readImageRGBA )
import Graphics.Phoskell.IO.Output ( writeImageRGBA )
import Graphics.Phoskell.Functional.Image

-- | Given some point in time, return a functional image.
type FAnim = Double -> FImage

-- | Render a functional animation as a list of functional images.
fAnimToFImages :: FAnim -- ^ Functional animation.
               -> Double -- ^ Start time.
               -> Double -- ^ Time step per frame.
               -> [FImage]
fAnimToFImages f s step = [f i | i <- [s,s+step..]]

-- | Render a functional animation as a list of RGBA images.
fAnimToImages :: FAnim -- ^ Functional animation.
              -> Double -- ^ Start time.
              -> Double -- ^ Time step per frame.
              -> (Double, Double) -- ^ Size of region to be covered in terms @(width, height)@.
              -> Double -- ^ Step size.
              -> [Image RGBA]
fAnimToImages f t t' wh s = [fImageToImage (f i) wh s | i <- [t,t+t'..]]

-- | Write a list of images to paths determined by the function given.
writeImages :: (Integer -> FilePath) -- ^ Function that takes a frame's index (starting at 0) and returns that frame's filepath.
            -> [Image RGBA] -- ^ List of images to output.
            -> IO ()
writeImages pathF = zipWithM_ writeImageRGBA paths
    where paths = map pathF [0..]

-- | Read the list of image paths as RGBA images.
readImages :: [FilePath] -> IO [Image RGBA]
readImages = mapM readImageRGBA

-- | Read the list of image paths as functional.
readFImages :: [FilePath] -> IO [FImage]
readFImages = mapM (fmap imageToFImage . readImageRGBA)

-- | Convert list of functional images to a functional animation.
--
-- Uses nearest neighbour interpolation for non-integer frames.
fImagesToFAnimN :: [FImage] -> FAnim
fImagesToFAnimN fImgs t = fromMaybe (const 0) (fImgs `atMay` floor t)

-- | Convert list of functional images to a functional animation.
--
-- Uses pointwise linear interpolation for non-integer frames.
fImagesToFAnim :: [FImage] -> FAnim
fImagesToFAnim fImgs t = interpolate lower upper
    where
        lower = getFrame (floor t)
        upper = getFrame (floor t + 1)
        getFrame f = fromMaybe (const 0) (fImgs `atMay` f)
        p = mkSmallDouble $ t `mod'` 1
        interpolate l u xy = (l xy `multScalar` (1 - p)) + (u xy `multScalar` p)

-- | Read images as a functional animation, rounding to the previous frame if needed.
readFAnimN :: [FilePath] -> IO FAnim
readFAnimN paths = fImagesToFAnimN <$> readFImages paths

-- | Read images as a functional animation, using interpolation for points between frames.
readFAnim :: [FilePath] -> IO FAnim
readFAnim paths = fImagesToFAnim <$> readFImages paths
