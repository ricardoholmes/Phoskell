{-# LANGUAGE DataKinds #-}
-- | Functions for writing images to files.
module Graphics.Phoskell.IO.Output (
    writeImageBinary,
    writeImageGrey,
    writeImageRGB,
    writeImageRGBA,
    writeImageHSV,
    writeImageHSL,
) where

import qualified Codec.Picture as JP
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Massiv.Array as M
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Codec.Picture.Jpg (encodeDirectJpegAtQualityWithMetadata)
import Codec.Picture.Saving (imageToTga)
import Codec.Picture.Types (ColorConvertible(promoteImage))
import Data.Bool (bool)
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Data.Massiv.Array (toUnboxedVector, toStorableVector, Ix2 ((:.)))
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import System.FilePath (takeExtension)

import Graphics.Phoskell.Core
import Graphics.Phoskell.Analysis
import Graphics.Phoskell.Processes.Threshold

writeDynamicImageAuto :: FilePath -> JP.DynamicImage -> IO ()
writeDynamicImageAuto fp = case takeExtension fp of
        ".bmp" -> JP.saveBmpImage fp
        ".jpg" -> JP.saveJpgImage 100 fp
        ".jpeg" -> JP.saveJpgImage 100 fp
        ".gif" -> either error id . JP.saveGifImage fp
        ".png" -> JP.savePngImage fp
        ".tga" -> BL.writeFile fp . imageToTga
        ".tiff" -> JP.saveTiffImage fp
        ".hdr" -> JP.saveRadianceImage fp
        _ -> const (error "Invalid image format")

rgbToDynamicImage :: Image RGB -> JP.DynamicImage
rgbToDynamicImage img =
        let (w,h) = imageSize img
            imgVec = toUnboxedVector $ toArrayUnboxed (img :> splitPixel)
            channels = tuple3ToList $ VU.unzip3 imgVec
            imgData = VS.generate (w*h*3) (\i -> (channels !! (i `mod` 3)) VU.! (i `div` 3))
        in JP.ImageRGB8 $ JP.Image {
                JP.imageWidth = w,
                JP.imageHeight = h,
                JP.imageData = imgData
            }
    where
        splitPixel :: RGB -> (Word8, Word8, Word8)
        splitPixel = VU.toURepr
        {-# INLINE splitPixel #-}
        tuple3ToList :: (a,a,a) -> [a]
        tuple3ToList (r,g,b) = [r,g,b]
        {-# INLINE tuple3ToList #-}

rgbaToDynamicImage :: Image RGBA -> JP.DynamicImage
rgbaToDynamicImage img =
        let (w,h) = imageSize img
            imgVec = toUnboxedVector $ toArrayUnboxed (img :> splitPixel)
            channels = tuple4ToList $ VU.unzip4 imgVec
            imgData = VS.generate (w*h*4) (\i -> (channels !! (i `mod` 4)) VU.! (i `div` 4))
        in JP.ImageRGBA8 $ JP.Image {
                JP.imageWidth = w,
                JP.imageHeight = h,
                JP.imageData = imgData
            }
    where
        splitPixel :: RGBA -> (Word8, Word8, Word8, Word8)
        splitPixel = VU.toURepr
        {-# INLINE splitPixel #-}
        tuple4ToList :: (a,a,a,a) -> [a]
        tuple4ToList (r,g,b,a) = [r,g,b,a]
        {-# INLINE tuple4ToList #-}

writeJpgGrey :: FilePath -> JP.Image JP.Pixel8 -> IO ()
writeJpgGrey fp = BL.writeFile fp . encode 50 metadata
    where
        encode = encodeDirectJpegAtQualityWithMetadata
        {-# INLINE encode #-}
        metadata = mempty
        {-# INLINE metadata #-}
{-# INLINE writeJpgGrey #-}

-- encode in netpbm formats --

toByteStringPBM :: Image Binary -> B.ByteString
toByteStringPBM img = fromString "P4\n"
                    <> fromString (show w ++ " " ++ show h ++ "\n")
                    <> imgByteString
    where
        (w,h) = imageSize img
        w' = ceiling (fromIntegral w / 8 :: Double) * 8
        bytes = ceiling (fromIntegral w / 8 :: Double) * h
        imgArr = toArrayUnboxed $ img :> (\(Pixel1 b) -> bool 1 0 b)
        imgBytes = M.generate M.Par (M.Sz1 bytes) (\i ->
                    let xStart = (8 * i) `mod` w'
                        y = (8 * i) `div` w'
                    in foldl (\t x ->
                            t * 2 + fromMaybe 0 (imgArr M.!? (y:.x))
                        ) 0 [xStart..xStart+7])
        {-# INLINE imgBytes #-}
        imgByteString = M.castToByteString $ M.computeAs M.S imgBytes
        {-# INLINE imgByteString #-}
{-# INLINE toByteStringPBM #-}

toByteStringPGM :: Image Grey -> B.ByteString
toByteStringPGM img = fromString "P5\n"
                    <> fromString (show w ++ " " ++ show h ++ "\n")
                    <> fromString "255\n" -- max value
                    <> imgByteString
    where
        (w,h) = imageSize img
        imgByteString = M.castToByteString $ toArrayStorable (img :> (\(Pixel1 x) -> x))
        {-# INLINE imgByteString #-}
{-# INLINE toByteStringPGM #-}

toByteStringPPM :: Image RGB -> BL.ByteString
toByteStringPPM img = fromString "P6\n"
                    <> fromString (show w ++ " " ++ show h ++ "\n")
                    <> fromString "255\n" -- max value
                    <> BB.toLazyByteString imgByteString
    where
        (w,h) = imageSize img
        imgVec = M.toStorableVector $ toArrayStorable img
        imgByteString = VS.foldMap (foldMap BB.word8) imgVec
        {-# INLINE imgByteString #-}
{-# INLINE toByteStringPPM #-}

-- | Read a binary image to the path given.
writeImageBinary :: FilePath -> Image Binary -> IO ()
writeImageBinary fp = if ".pbm" `isSuffixOf` map toLower fp
                            then B.writeFile fp . toByteStringPBM
                            else writeImageGrey fp . fmap toGrey
    where
        toGrey (Pixel1 False) = 0
        toGrey (Pixel1 True) = 255
        {-# INLINE toGrey #-}

-- | Read a greyscale image to the path given.
writeImageGrey :: FilePath -> Image Grey -> IO ()
writeImageGrey fp img = case takeExtension fp of
        ".bmp" -> JP.writeBitmap fp imgJP
        ".jpg" ->  writeJpgGrey fp imgJP
        ".jpeg" -> writeJpgGrey fp imgJP
        ".gif" -> JP.writeGifImage fp imgJP
        ".png" -> JP.writePng fp imgJP
        ".tga" -> JP.writeTga fp imgJP
        ".tiff" -> JP.writeTiff fp imgJP
        ".hdr" -> JP.writeHDR fp $ toRadianceEncodable imgJP
        ".pbm" -> B.writeFile fp $ toByteStringPBM (img :> threshold 127)
        ".pgm" -> B.writeFile fp $ toByteStringPGM img
        ".ppm" -> BL.writeFile fp $ toByteStringPPM (img :> greyToRGB)
        _ -> fail "Invalid image format"
    where
        unwrapToPixel8 :: Grey -> JP.Pixel8
        unwrapToPixel8 (Pixel1 y) = y
        {-# INLINE unwrapToPixel8 #-}
        (width,height) = imageSize img
        imageData = toStorableVector $ toArrayStorable (img :> unwrapToPixel8)
        imgJP = JP.Image width height imageData :: JP.Image JP.Pixel8
        -- make image able to be saved to an HDR file
        toRadianceEncodable :: JP.Image JP.Pixel8 -> JP.Image JP.PixelRGBF
        toRadianceEncodable im = promoteImage (promoteImage im :: JP.Image JP.PixelRGB8)
        {-# INLINE toRadianceEncodable #-}

-- | Read an RGB image to the path given.
writeImageRGB :: FilePath -> Image RGB -> IO ()
writeImageRGB fp = case takeExtension fp of
        ".pbm" -> B.writeFile fp . toByteStringPBM . (:> threshold 127) . (:> rgbToGrey)
        ".pgm" -> B.writeFile fp . toByteStringPGM . (:> rgbToGrey)
        ".ppm" -> BL.writeFile fp . toByteStringPPM
        _ -> writeDynamicImageAuto fp . rgbToDynamicImage

-- | Read an RGBA image to the path given.
writeImageRGBA :: FilePath -> Image RGBA -> IO ()
writeImageRGBA fp = case takeExtension fp of
        ".pbm" -> B.writeFile fp . toByteStringPBM . (:> threshold 127) . (:> rgbaToGrey)
        ".pgm" -> B.writeFile fp . toByteStringPGM . (:> rgbaToGrey)
        ".ppm" -> BL.writeFile fp . toByteStringPPM . (:> rgbaToRGB)
        _ -> writeDynamicImageAuto fp . rgbaToDynamicImage

-- | Read an HSV image to the path given.
writeImageHSV :: FilePath -> Image HSV -> IO ()
writeImageHSV fp = writeImageRGB fp . fmap hsvToRGB

-- | Read an HSL image to the path given.
writeImageHSL :: FilePath -> Image HSL -> IO ()
writeImageHSL fp = writeImageRGB fp . fmap hslToRGB
