{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Image (
    Image(..),
    ImageProcess(..),
) where

import qualified Data.Massiv.Array as M

data Image a where
    BaseImage :: M.Array M.P M.Ix3 a -> Image a
    (:>) :: Image x -> ImageProcess x a -> Image a

data ImageProcess a b = PointProcess (a -> b)
                      | FragmentProcess ((Int, Int, a) -> b)
                      | GeneralProcess (Image a -> Image b)
