module Spec.Pixel (
    propsLawsPixel1,
    propsLawsPixel2,
    propsLawsPixel3,
    propsLawsPixel4,
) where

import Data.Proxy
import Test.QuickCheck.Classes
import Graphics.ImageProcessing.Core

import Arbitrary ()

-- typeclass laws for pixels

propsLawsPixel1 :: IO ()
propsLawsPixel1 = do
        putStrLn "Pixel1 laws"
        lawsCheck (eqLaws pa)
        lawsCheck (ordLaws pa)
        lawsCheck (functorLaws p)
        lawsCheck (applicativeLaws p)
        lawsCheck (foldableLaws p)
        lawsCheck (traversableLaws p)
        lawsCheck (numLaws pa)
        lawsCheck (enumLaws pa)
    where
        pa = Proxy :: Proxy (Pixel1 Int)
        p = Proxy :: Proxy Pixel1

propsLawsPixel2 :: IO ()
propsLawsPixel2 = do
        putStrLn "Pixel2 laws"
        lawsCheck (eqLaws pa)
        lawsCheck (functorLaws p)
        lawsCheck (applicativeLaws p)
        lawsCheck (foldableLaws p)
        lawsCheck (traversableLaws p)
        lawsCheck (numLaws pa)
    where
        pa = Proxy :: Proxy (Pixel2 Int)
        p = Proxy :: Proxy Pixel2

propsLawsPixel3 :: IO ()
propsLawsPixel3 = do
        putStrLn "Pixel3 laws"
        lawsCheck (eqLaws pa)
        lawsCheck (functorLaws p)
        lawsCheck (applicativeLaws p)
        lawsCheck (foldableLaws p)
        lawsCheck (traversableLaws p)
        lawsCheck (numLaws pa)
    where
        pa = Proxy :: Proxy (Pixel3 Int)
        p = Proxy :: Proxy Pixel3

propsLawsPixel4 :: IO ()
propsLawsPixel4 = do
        putStrLn "Pixel4 laws"
        lawsCheck (eqLaws pa)
        lawsCheck (functorLaws p)
        lawsCheck (applicativeLaws p)
        lawsCheck (foldableLaws p)
        lawsCheck (traversableLaws p)
        lawsCheck (numLaws pa)
    where
        pa = Proxy :: Proxy (Pixel4 Int)
        p = Proxy :: Proxy Pixel4
