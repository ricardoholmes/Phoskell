module Main where

import Test.QuickCheck

import Arbitrary ()
import Spec.Pixel
import Spec.Color
import Spec.Image
import Spec.Transformations

-- collections of tests --

propsConversionsGray :: IO ()
propsConversionsGray = do
        putStrLn "Gray conversions"
        quickCheck prop_gray_rgb
        quickCheck prop_gray_hsv
        quickCheck prop_gray_hsl
        quickCheck prop_gray_rgba

propsConversionsRGB :: IO ()
propsConversionsRGB = do
        putStrLn "RGB conversions"
        quickCheck prop_rgb_hsv
        quickCheck prop_rgb_hsl
        quickCheck prop_rgb_rgba

propsConversionsHSV :: IO ()
propsConversionsHSV = do
        putStrLn "HSV conversions"
        quickCheck prop_hsv_rgb
        quickCheck prop_hsv_hsl
        quickCheck prop_hsv_rgba

propsConversionsHSL :: IO ()
propsConversionsHSL = do
        putStrLn "HSL conversions"
        quickCheck prop_hsl_hsv
        quickCheck prop_hsl_rgb
        quickCheck prop_hsl_rgba

propsImageType :: IO ()
propsImageType = do
        putStrLn "Image Type"
        quickCheck prop_arrayImageUnchanged
        quickCheck prop_imageArrayUnchanged

propsImageTransformations :: IO ()
propsImageTransformations = do
        putStrLn "Image transformations"
        quickCheck prop_transpose
        quickCheck prop_mirrorX
        quickCheck prop_mirrorY
        quickCheck prop_rot90x4
        quickCheck prop_rot180x2
        quickCheck prop_rot270x4
        quickCheck prop_zoomOutInEven
        quickCheck prop_zoomOutInOdd

main :: IO ()
main = do propsLawsPixel1
          propsLawsPixel2
          propsLawsPixel3
          propsLawsPixel4
          propsConversionsGray
          propsConversionsRGB
          propsConversionsHSV
          propsConversionsHSL
          propsLawsImage
          propsImageType
          propsImageTransformations
