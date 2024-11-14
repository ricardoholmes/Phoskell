module Graphics.Image.IO (
    readImage,
    writeImage,
) where

import qualified Data.Massiv.Array.IO as MIO
import Graphics.Image
import Graphics.Pixel

readImage :: (ColorModel cs e) => FilePath -> IO (Image cs e)
readImage path = do img <- MIO.readImage path
                    return (Image img)

writeImage :: (ColorModel cs e) => FilePath -> Image cs e -> IO ()
writeImage fp = MIO.writeImage fp . toArray
