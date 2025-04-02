module Spec.Pixel (
    propsLawsPixel1,
    propsLawsPixel2,
    propsLawsPixel3,
    propsLawsPixel4,
) where

import Common

import Data.Proxy
import Test.Tasty
import Test.QuickCheck.Classes
import Graphics.Phoskell.Core.Pixel

-- typeclass laws for pixels

propsLawsPixel1 :: TestTree
propsLawsPixel1 = testGroup "Pixel1 Laws" [
            lawsToTestTree (eqLaws pa),
            lawsToTestTree (ordLaws pa),
            lawsToTestTree (functorLaws p),
            lawsToTestTree (applicativeLaws p),
            lawsToTestTree (foldableLaws p),
            lawsToTestTree (traversableLaws p),
            lawsToTestTree (numLaws pa),
            lawsToTestTree (enumLaws pa)
        ]
    where
        pa = Proxy :: Proxy (Pixel1 Int)
        p = Proxy :: Proxy Pixel1

propsLawsPixel2 :: TestTree
propsLawsPixel2 = testGroup "Pixel2 Laws" [
            lawsToTestTree (eqLaws pa),
            lawsToTestTree (functorLaws p),
            lawsToTestTree (applicativeLaws p),
            lawsToTestTree (foldableLaws p),
            lawsToTestTree (traversableLaws p),
            lawsToTestTree (numLaws pa)
        ]
    where
        pa = Proxy :: Proxy (Pixel2 Int)
        p = Proxy :: Proxy Pixel2

propsLawsPixel3 :: TestTree
propsLawsPixel3 = testGroup "Pixel3 Laws" [
            lawsToTestTree (eqLaws pa),
            lawsToTestTree (functorLaws p),
            lawsToTestTree (applicativeLaws p),
            lawsToTestTree (foldableLaws p),
            lawsToTestTree (traversableLaws p),
            lawsToTestTree (numLaws pa)
        ]
    where
        pa = Proxy :: Proxy (Pixel3 Int)
        p = Proxy :: Proxy Pixel3

propsLawsPixel4 :: TestTree
propsLawsPixel4 = testGroup "Pixel4 Laws" [
            lawsToTestTree (eqLaws pa),
            lawsToTestTree (functorLaws p),
            lawsToTestTree (applicativeLaws p),
            lawsToTestTree (foldableLaws p),
            lawsToTestTree (traversableLaws p),
            lawsToTestTree (numLaws pa)
        ]
    where
        pa = Proxy :: Proxy (Pixel4 Int)
        p = Proxy :: Proxy Pixel4
