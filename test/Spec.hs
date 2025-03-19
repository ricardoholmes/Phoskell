module Main where

import Common ()
import Spec.Pixel
import Spec.Colour
import Spec.Image
import Spec.Transformations

import Test.Tasty
import Test.Tasty.QuickCheck

-- collections of tests --

pixelProps :: TestTree
pixelProps = testGroup "Pixel" [
        propsLawsPixel1,
        propsLawsPixel2,
        propsLawsPixel3,
        propsLawsPixel4
    ]

grayProps :: TestTree
grayProps = testGroup "Gray Conversions" [
        testProperty "id == rgbToGray . grayToRGB" prop_gray_rgb,
        testProperty "id == hsvToGray . grayToHSV" prop_gray_hsv,
        testProperty "id == hslToGray . grayToHSL" prop_gray_hsl,
        testProperty "id == rgbaToGray . grayToRGBA" prop_gray_rgba
    ]

rgbProps :: TestTree
rgbProps = testGroup "RGB Conversions" [
        testProperty "id == hsvToRGB . rgbToHSV" prop_rgb_hsv,
        testProperty "id == hslToRGB . rgbToHSL" prop_rgb_hsl,
        testProperty "id == rgbaToRGB . rgbToRGBA" prop_rgb_rgba
    ]

hsvProps :: TestTree
hsvProps = testGroup "HSV Conversions" [
        testProperty "id == rgbToHSV . hsvToRGB" prop_hsv_rgb,
        testProperty "id == hslToHSV . hsvToHSL" prop_hsv_hsl,
        testProperty "id == rgbaToHSV . hsvToRGBA" prop_hsv_rgba
    ]

hslProps :: TestTree
hslProps = testGroup "HSL Conversions" [
        testProperty "id == rgbToHSL . hslToRGB" prop_hsl_rgb,
        testProperty "id == hsvToHSL . hslToHSV" prop_hsl_hsv,
        testProperty "id == rgbaToHSL . hslToRGBA" prop_hsl_rgba
    ]

colourProps :: TestTree
colourProps = testGroup "Colour" [
        grayProps,
        rgbProps,
        hsvProps,
        hslProps
    ]

imageArrayProps :: TestTree
imageArrayProps = testGroup "Image Array Conversion" [
        testProperty "id == toArray . BaseImage" prop_arrayImageUnchanged,
        testProperty "id == BaseImage . toArray" prop_imageArrayUnchanged
    ]

imageProps :: TestTree
imageProps = testGroup "Image" [
        imageArrayProps,
        imageLawsProps
    ]

transformationProps :: TestTree
transformationProps = testGroup "Transformation" [
        testProperty "img == img :> transpose :> transpose" prop_transpose,
        testProperty "img == img :> mirrorX :> mirrorX" prop_mirrorX,
        testProperty "img == img :> mirrorY :> mirrorY" prop_mirrorY,
        testProperty "img == img :> rotate90 :> rotate90 :> rotate90 :> rotate90" prop_rot90x4,
        testProperty "img == img :> rotate180 :> rotate180" prop_rot180x2,
        testProperty "img == img :> rotate270 :> rotate270 :> rotate270 :> rotate270" prop_rot270x4,
        testProperty "img == img :> zoom 0.5 0 :> zoom 2 0" prop_zoomOutInOdd,
        testProperty "img == img :> zoom 0.2 0 :> zoom 5 0" prop_zoomOutInEven
    ]

main :: IO ()
main = defaultMain $ testGroup "Properties" [
            pixelProps,
            colourProps,
            imageProps,
            transformationProps
        ]
