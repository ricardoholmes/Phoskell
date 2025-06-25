{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | Functions for reading images from files.
module Graphics.Phoskell.IO.Input (
    readImageBinary,
    readImageGrey,
    readImageRGB,
    readImageRGBA,
    readImageHSV,
    readImageHSL,
) where

import Foreign (Storable)
import Data.Bits
import Codec.Picture.Types hiding (Image, Pixel)

import qualified Codec.Picture as JP -- JuicyPixels
import qualified Data.Massiv.Array as M
import qualified Data.Vector.Storable as V

import Graphics.Phoskell.Core
import Graphics.Phoskell.Processes (threshold)

-- HELPER FUNCTIONS --

class Decimable px1 px2 where
    decimateBitDepth :: JP.Image px1 -> JP.Image px2

decimateWord16 :: ( JP.Pixel px1, JP.Pixel px2
                  , PixelBaseComponent px1 ~ Pixel16
                  , PixelBaseComponent px2 ~ Pixel8
                  ) => JP.Image px1 -> JP.Image px2
decimateWord16 (JP.Image w h da) =
  JP.Image w h $ V.map (\v -> fromIntegral $ v `unsafeShiftR` 8) da

decimateWord3216 :: ( JP.Pixel px1, JP.Pixel px2
                  , PixelBaseComponent px1 ~ Pixel32
                  , PixelBaseComponent px2 ~ Pixel16
                  ) => JP.Image px1 -> JP.Image px2
decimateWord3216 (JP.Image w h da) =
  JP.Image w h $ V.map (\v -> fromIntegral $ v `unsafeShiftR` 16) da
  
decimateWord32 :: ( JP.Pixel px1, JP.Pixel px2
                  , PixelBaseComponent px1 ~ Pixel32
                  , PixelBaseComponent px2 ~ Pixel8
                  ) => JP.Image px1 -> JP.Image px2
decimateWord32 (JP.Image w h da) =
  JP.Image w h $ V.map (\v -> fromIntegral $ v `unsafeShiftR` 24) da

decimateFloat :: ( JP.Pixel px1, JP.Pixel px2
                 , PixelBaseComponent px1 ~ PixelF
                 , PixelBaseComponent px2 ~ Pixel8
                 ) => JP.Image px1 -> JP.Image px2
decimateFloat (JP.Image w h da) =
  JP.Image w h $ V.map (floor . (255*) . max 0 . min 1) da

decimateFloat16 :: ( JP.Pixel px1, JP.Pixel px2
                 , PixelBaseComponent px1 ~ PixelF
                 , PixelBaseComponent px2 ~ Pixel16
                 ) => JP.Image px1 -> JP.Image px2
decimateFloat16 (JP.Image w h da) =
  JP.Image w h $ V.map (floor . (65535*) . max 0 . min 1) da

instance Decimable Pixel16 Pixel8 where
    decimateBitDepth = decimateWord16
    {-# INLINE decimateBitDepth #-}

instance Decimable Pixel32 Pixel16 where
    decimateBitDepth = decimateWord3216
    {-# INLINE decimateBitDepth #-}

instance Decimable Pixel32 Pixel8 where
    decimateBitDepth = decimateWord32
    {-# INLINE decimateBitDepth #-}

instance Decimable PixelYA16 PixelYA8 where
    decimateBitDepth = decimateWord16
    {-# INLINE decimateBitDepth #-}

instance Decimable PixelRGB16 PixelRGB8 where
    decimateBitDepth = decimateWord16
    {-# INLINE decimateBitDepth #-}

instance Decimable PixelRGBA16 PixelRGBA8 where
    decimateBitDepth = decimateWord16
    {-# INLINE decimateBitDepth #-}

instance Decimable PixelCMYK16 PixelCMYK8 where
    decimateBitDepth = decimateWord16
    {-# INLINE decimateBitDepth #-}

instance Decimable PixelF Pixel8 where
    decimateBitDepth = decimateFloat
    {-# INLINE decimateBitDepth #-}

instance Decimable PixelF Pixel16 where
    decimateBitDepth = decimateFloat16
    {-# INLINE decimateBitDepth #-}

instance Decimable PixelRGBF PixelRGB8 where
    decimateBitDepth = decimateFloat
    {-# INLINE decimateBitDepth #-}

instance Decimable PixelRGBF PixelRGB16 where
    decimateBitDepth = decimateFloat16
    {-# INLINE decimateBitDepth #-}

readWithConversion :: (JP.DynamicImage -> JP.Image a) -> FilePath -> IO (JP.Image a)
readWithConversion conv fp = do img <- JP.readImage fp
                                return $ case img of
                                    (Left err) -> error err
                                    (Right i) -> conv i

jpToImageIO :: (Storable (JP.PixelBaseComponent a), Storable b) => JP.Image a -> IO (Image b)
jpToImageIO JP.Image{
            JP.imageData=img,
            JP.imageWidth=w,
            JP.imageHeight=h
        } = BaseImage . M.delay <$> M.resizeM sz vec
    where
        sz = M.Sz2 h w
        vec = M.fromStorableVector M.Par $ V.unsafeCast img

convertY8 :: DynamicImage -> JP.Image JP.Pixel8
convertY8 dynImage = case dynImage of
    ImageY8     img -> img
    ImageY16    img -> decimateBitDepth img
    ImageY32    img -> decimateBitDepth img
    ImageYF     img -> decimateBitDepth img
    ImageYA8    img -> dropAlphaLayer img
    ImageYA16   img -> dropAlphaLayer (decimateBitDepth img :: JP.Image PixelYA8)
    ImageRGB8   img -> extractLumaPlane img
    ImageRGB16  img -> extractLumaPlane (decimateBitDepth img :: JP.Image PixelRGB8)
    ImageRGBF   img -> extractLumaPlane (decimateBitDepth img :: JP.Image PixelRGB8)
    ImageRGBA8  img -> extractLumaPlane img
    ImageRGBA16 img -> extractLumaPlane (decimateBitDepth img :: JP.Image PixelRGBA8)
    ImageYCbCr8 img -> extractLumaPlane img
    ImageCMYK8  img -> extractLumaPlane (convertImage img :: JP.Image PixelRGB8)
    ImageCMYK16 img -> extractLumaPlane (convertImage (decimateBitDepth img :: JP.Image PixelCMYK8) :: JP.Image PixelRGB8)

-- IMAGE READING FUNCTIONS --

-- | Read an image as binary, given its path.
readImageBinary :: FilePath -> IO (Image Binary)
readImageBinary fp = do img <- readWithConversion convertY8 fp
                        img' <- jpToImageIO img :: IO (Image Grey)
                        return (img' :> threshold 127)

-- | Read an image as greyscale, given its path.
readImageGrey :: FilePath -> IO (Image Grey)
readImageGrey fp = do img <- readWithConversion convertY8 fp
                      jpToImageIO img

-- | Read an image as RGB, given its path.
readImageRGB :: FilePath -> IO (Image RGB)
readImageRGB fp = do img <- readWithConversion JP.convertRGB8 fp
                     jpToImageIO img

-- | Read an image as RGBA, given its path.
readImageRGBA :: FilePath -> IO (Image RGBA)
readImageRGBA fp = do img <- readWithConversion JP.convertRGBA8 fp
                      jpToImageIO img

-- | Read an image as HSV, given its path.
readImageHSV :: FilePath -> IO (Image HSV)
readImageHSV fp = do img <- readWithConversion JP.convertRGB8 fp
                     img' <- jpToImageIO img
                     return (img' :> PointProcess rgbToHSV)

-- | Read an image as HSL, given its path.
readImageHSL :: FilePath -> IO (Image HSL)
readImageHSL fp = do img <- readWithConversion JP.convertRGB8 fp
                     img' <- jpToImageIO img
                     return (img' :> PointProcess rgbToHSL)
