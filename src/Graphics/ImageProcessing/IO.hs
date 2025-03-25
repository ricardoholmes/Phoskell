{-# LANGUAGE DataKinds #-}
module Graphics.ImageProcessing.IO (
    readImageBinary,
    readImageGrey,
    readImageRGB,
    readImageRGBA,
    readImageHSV,
    readImageHSL,

    writeImageBinary,
    writeImageGrey,
    writeImageRGB,
    writeImageRGBA,
    writeImageHSV,
    writeImageHSL,
) where

import Graphics.ImageProcessing.IO.Input
import Graphics.ImageProcessing.IO.Output
