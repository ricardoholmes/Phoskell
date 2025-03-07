module Spec.Image (
    prop_arrayImageUnchanged,
    prop_imageArrayUnchanged,
    propsLawsImage,
) where

import Graphics.ImageProcessing.Core
import Test.QuickCheck.Classes
import Data.Proxy (Proxy (..))
import Arbitrary ()

-- | Converting an array to an image and back does nothing
prop_arrayImageUnchanged :: ImageArray RGBA -> Bool
prop_arrayImageUnchanged arr = arr == arr'
    where arr' = toArray (BaseImage arr)

-- | Converting an image to an array and back does nothing
prop_imageArrayUnchanged :: Image RGBA -> Bool
prop_imageArrayUnchanged img = img == img'
    where img' = BaseImage (toArray img)

-- | Laws for image's typeclasses
--
-- Note: Doesn't test @Applicative@ and @Num@ because doing so would test images
-- with different sizes, which is not supported.
propsLawsImage :: IO ()
propsLawsImage = do
        putStrLn "Image laws"
        lawsCheck (eqLaws pa)
        lawsCheck (functorLaws p)
    where
        pa = Proxy :: Proxy (Image Int)
        p = Proxy :: Proxy Image
