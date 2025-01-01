{-# LANGUAGE DataKinds #-}
module Graphics.ImageProcessing.IO (
    readImageBinary,
    readImageGray,
    readImageRGB,
    readImageRGBA,
    readImageHSV,
    readImageHSL,

    writeImageBinary,
    writeImageGray,
    writeImageRGB,
    writeImageRGBA,
    writeImageHSV,
    writeImageHSL,
) where

import Graphics.ImageProcessing.IO.Input
import Graphics.ImageProcessing.IO.Output
