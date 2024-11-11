module Graphics.IO (
    readImage
) where

import qualified Data.Massiv.Array.IO as MIO
import Data.Massiv.Array
import Graphics.Image
import Graphics.Pixel

readImage :: (ColorModel cs e) => FilePath -> IO (Image cs e)
readImage path = do img <- MIO.readImage path
                    return $ massivToImage img

massivToImage :: MIO.Image S cs e -> Image cs e
massivToImage = Image
