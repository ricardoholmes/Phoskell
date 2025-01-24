module Graphics.ImageProcessing (
    Ix2(..),
    Sz(..),
    Sz2,
    Image(..),
    (!),
    generateImage,
) where

import Graphics.ImageProcessing.Core.Image
import qualified Data.Massiv.Array as MA
import Data.Massiv.Array (Ix2 (..), D (D), Comp (Par), Sz2, Sz (..))

generateImage :: Sz2 -> (Ix2 -> a) -> Image a
generateImage sz f = BaseImage (MA.makeArrayR D Par sz f)
