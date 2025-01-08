{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.ImageProcessing.Processes (
    ImageProcess(..),

    PointProcess(..),
    IPointProcess(..),
    MiscProcess(..),
) where

import Graphics.ImageProcessing.Core.Image
import qualified Data.Massiv.Array as M

newtype PointProcess a b = PointProcess (a -> b)
newtype IPointProcess a b = IPointProcess (M.Ix2 -> a -> b)
newtype MiscProcess a b = MiscProcess (ImageArray a -> ImageArray b)

instance ImageProcess PointProcess where
    applyProcess :: PointProcess a b -> ImageArray a -> ImageArray b
    applyProcess (PointProcess f) = M.map f

instance ImageProcess IPointProcess where
    applyProcess :: IPointProcess a b -> ImageArray a -> ImageArray b
    applyProcess (IPointProcess f) = M.imap f

instance ImageProcess MiscProcess where
    applyProcess :: MiscProcess a b -> ImageArray a -> ImageArray b
    applyProcess (MiscProcess f) = f
