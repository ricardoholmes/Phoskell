{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Image.ImageProcess (
    ImageProcess(..),

    PointProcess(..),
    IPointProcess(..),
    MiscProcess(..),
) where

import Graphics.Image.Internal
import qualified Data.Massiv.Array as M

newtype PointProcess a b = PointProcess (a -> b)
newtype IPointProcess a b = IPointProcess (M.Ix2 -> a -> b)
newtype MiscProcess a b = MiscProcess (M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b)

instance ImageProcess PointProcess where
    applyProcess :: PointProcess a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
    applyProcess (PointProcess f) = M.map f

instance ImageProcess IPointProcess where
    applyProcess :: IPointProcess a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
    applyProcess (IPointProcess f) = M.imap f

instance ImageProcess MiscProcess where
    applyProcess :: MiscProcess a b -> M.Array M.D M.Ix2 a -> M.Array M.D M.Ix2 b
    applyProcess (MiscProcess f) = f
