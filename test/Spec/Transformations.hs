module Spec.Transformations (
    prop_transpose,
    prop_mirrorX,
    prop_mirrorY,
    prop_rot90x4,
    prop_rot180x2,
    prop_rot270x4,
    prop_zoomOutInOdd,
    prop_zoomOutInEven,
) where

import Graphics.ImageProcessing.Core
import Graphics.ImageProcessing.Analysis
import Graphics.ImageProcessing.Transformations
import Graphics.ImageProcessing.Transformations.Rotation

-- lossless image transformations --

prop_transpose :: Image RGBA -> Bool
prop_transpose img = img == img :> transpose :> transpose

prop_mirrorX :: Image RGBA -> Bool
prop_mirrorX img = img == img :> mirrorX :> mirrorX

prop_mirrorY :: Image RGBA -> Bool
prop_mirrorY img = img == img :> mirrorY :> mirrorY

prop_rot90x4 :: Image RGBA -> Bool
prop_rot90x4 img = img == rot90x4 img
    where rot90x4 x = x :> rotate90 :> rotate90 :> rotate90 :> rotate90

prop_rot180x2 :: Image RGBA -> Bool
prop_rot180x2 img = img == rot180x2 img
    where rot180x2 x = x :> rotate180 :> rotate180

prop_rot270x4 :: Image RGBA -> Bool
prop_rot270x4 img = img == rot270x4 img
    where rot270x4 x = x :> rotate270 :> rotate270 :> rotate270 :> rotate270

-- zooming --

prop_zoomOutInOdd :: Image RGBA -> Bool
prop_zoomOutInOdd img = img == img'
    where img' = img :> zoom 0.2 0 :> zoom 5 0

-- zooming with even multiplier can result in pixels being lost due to rounding
-- when calculating the image center, but image size should still be accurate
-- so that'll be checked instead
prop_zoomOutInEven :: Image RGBA -> Bool
prop_zoomOutInEven img = imageSize img == imageSize img'
    where img' = img :> zoom 0.5 0 :> zoom 2 0
