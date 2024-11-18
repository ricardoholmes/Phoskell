module Graphics.Image.IO (
    readImageRGB,
    writeImageRGB,
) where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.IO as MIO
import qualified Graphics.Color.Model as C
import Graphics.Image
import Graphics.Image.Color
import Graphics.Image.Pixel
import Graphics.Image.Internal
    
readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB fp = do img <- MIO.readImage fp
                     let img' = M.map (\p ->
                            let (C.ColorRGB r g b) = MIO.pixelColor p
                            in Pixel3 r g b) img
                     return $ BaseImage img'

writeImageRGB :: FilePath -> Image RGB -> IO ()
writeImageRGB fp = MIO.writeImage fp .
        M.map (\(Pixel3 r g b) -> MIO.Pixel $ C.ColorRGB r g b) .
        toArray
