-- | Variety of image processes.
module Graphics.Phoskell.Processes (
    -- * Image Process re-exports
    -- ** Image process type-class
    ImageProcess(..),

    -- ** Image process types.
    PointProcess(..),
    IPointProcess(..),
    ArrayProcess(..),

    -- * Alpha channel processes
    module Graphics.Phoskell.Processes.Alpha,

    -- * Convolution
    module Graphics.Phoskell.Processes.Convolution,

    -- * Histogram manipulation
    module Graphics.Phoskell.Processes.Histogram,

    -- * Point processes
    module Graphics.Phoskell.Processes.Point,

    -- * Thresholding processes
    module Graphics.Phoskell.Processes.Threshold,

) where

import Graphics.Phoskell.Core.Image

import Graphics.Phoskell.Processes.Alpha
import Graphics.Phoskell.Processes.Convolution
import Graphics.Phoskell.Processes.Histogram
import Graphics.Phoskell.Processes.Point
import Graphics.Phoskell.Processes.Threshold
