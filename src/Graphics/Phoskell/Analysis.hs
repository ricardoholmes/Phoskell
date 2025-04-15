-- | Analysis functions and processes.
module Graphics.Phoskell.Analysis (
    imageSize,
    imageSize',

    -- * Histogram functions
    module Graphics.Phoskell.Analysis.Histogram,
) where

import Graphics.Phoskell.Analysis.Histogram
import Graphics.Phoskell.Core.Image ( Image, toArray )
import Data.Massiv.Array ( Size (size), Sz (Sz2) )

-- | Gets the dimensions of the image given.
imageSize :: Image a -> (Int,Int)
imageSize img = (w,h)
    where Sz2 h w = size (toArray img)

-- | Gets the dimensions of the image given, in any Num type.
--
-- More general version of 'imageSize'.
imageSize' :: Num b => Image a -> (b,b)
imageSize' img = (fromIntegral w, fromIntegral h)
    where Sz2 h w = size (toArray img)
