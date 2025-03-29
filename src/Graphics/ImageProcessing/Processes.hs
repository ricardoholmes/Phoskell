module Graphics.ImageProcessing.Processes (
    -- Image process type-class
    ImageProcess(..),

    -- Image process types.
    PointProcess(..),
    IPointProcess(..),
    ArrayProcess(..),

    -- | Alpha channel processes
    module Graphics.ImageProcessing.Processes.Alpha,

    -- | Convolution
    module Graphics.ImageProcessing.Processes.Convolution,

    -- | Histogram manipulation
    module Graphics.ImageProcessing.Processes.Histogram,

    -- | Point processes
    module Graphics.ImageProcessing.Processes.Point,

    -- | Thresholding processes
    module Graphics.ImageProcessing.Processes.Threshold,

) where

import Graphics.ImageProcessing.Core.Image

import Graphics.ImageProcessing.Processes.Alpha
import Graphics.ImageProcessing.Processes.Convolution
import Graphics.ImageProcessing.Processes.Histogram
import Graphics.ImageProcessing.Processes.Point
import Graphics.ImageProcessing.Processes.Threshold
