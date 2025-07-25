-- | Functional image type and associated functions.
module Graphics.Phoskell.Functional.Image (
    SmallDouble,
    Colour,
    Point,
    FImage,
    mkSmallDouble,
    fImageToImage,
    imageToFImage,
    imageToFImageN,
    applyTransformF,
    fromPolarF,
    toPolarF,
    overlayImageF,
    repeatProcessF,
) where

import Graphics.Phoskell.Core
import Graphics.Phoskell.Synthesis
import Graphics.Phoskell.Analysis
import Data.Maybe

-- | "Double" bounded between 0 and 1.
newtype SmallDouble = SD Double deriving (Eq, Ord)

-- | Convert "Double" to 'SmallDouble'.
mkSmallDouble :: Double -> SmallDouble
mkSmallDouble = SD . min 1 . max 0

-- | Convert 'SmallDouble' to "Double".
smallDoubleToDouble :: SmallDouble -> Double
smallDoubleToDouble (SD n) = n

instance Num SmallDouble where
    (SD x) + (SD y) = mkSmallDouble (x + y)
    (SD x) * (SD y) = mkSmallDouble (x * y)
    abs = id
    signum (SD n) = mkSmallDouble (signum n)
    fromInteger = mkSmallDouble . fromInteger
    (SD x) - (SD y) = mkSmallDouble (x - y)

instance Fractional SmallDouble where
    fromRational = mkSmallDouble . fromRational
    recip (SD n) = mkSmallDouble (1/n)

instance Floating SmallDouble where
    pi = mkSmallDouble pi
    exp (SD n) = mkSmallDouble (exp n)
    log (SD n) = mkSmallDouble (log n)
    sin (SD n) = mkSmallDouble (sin n)
    cos (SD n) = mkSmallDouble (cos n)
    asin (SD n) = mkSmallDouble (asin n)
    acos (SD n) = mkSmallDouble (acos n)
    atan (SD n) = mkSmallDouble (atan n)
    sinh (SD n) = mkSmallDouble (sinh n)
    cosh (SD n) = mkSmallDouble (cosh n)
    asinh (SD n) = mkSmallDouble (asinh n)
    acosh (SD n) = mkSmallDouble (acosh n)
    atanh (SD n) = mkSmallDouble (atanh n)

instance Real SmallDouble where
    toRational (SD n) = toRational n

instance RealFrac SmallDouble where
    properFraction (SD n) = (x, mkSmallDouble m)
        where (x,m) = properFraction n -- n should be in [0,1] but just in case

instance RealFloat SmallDouble where
    floatRadix (SD n) = floatRadix n
    floatDigits (SD n) = floatDigits n
    floatRange (SD n) = floatRange n
    decodeFloat (SD n) = decodeFloat n
    encodeFloat m n = mkSmallDouble (encodeFloat m n)
    isNaN (SD n) = isNaN n
    isInfinite (SD n) = isInfinite n
    isDenormalized (SD n) = isDenormalized n
    isNegativeZero (SD n) = isNegativeZero n
    isIEEE (SD n) = isIEEE n

instance Bounded SmallDouble where
    minBound = 0
    maxBound = 1

-- | RGBA colour, with values within [0..1].
type Colour = Pixel4 SmallDouble
-- | Point in space.
type Point = (Double, Double)
-- | Functional image.
type FImage = Point -> Colour

-- | Convert a functional image into a normal image
--
-- Image will cover region from @(-width\/2, -height\/2)@ to @(width\/2, height\/2)@.
--
-- The image will have size @(width\/step_size, height\/step_size)@.
fImageToImage :: FImage -- ^ Functional image to convert.
              -> (Double, Double) -- ^ Size of region to be covered in terms @(width, height)@.
              -> Double -- ^ Step size.
              -> Image RGBA
fImageToImage img (w,h) s = generateImage' xyMin xyMax f
    where
        w' = round (w/s)
        h' = round (h/s)
        xyMin = (-(w' `div` 2), -(h' `div` 2))
        xyMax = (w' `div` 2, h' `div` 2)
        f (x,y) = let x' = x * s
                      y' = y * s
                  in smallDoubleToDouble <$> img (x',y')

-- | Convert an 8 bit unsigned integer pixel value to a 'SmallDouble'.
word8ToSmallDouble :: Word8 -> SmallDouble
word8ToSmallDouble = mkSmallDouble . (/255) . fromIntegral

-- | Image to functional image, using interpolation for area between image's pixels
imageToFImage :: Image RGBA -> FImage
imageToFImage img (x,y) = v
    where
        v = a' + b' + c' + d'
        a' = a `multScalar` mkSmallDouble weightA
        b' = b `multScalar` mkSmallDouble weightB
        c' = c `multScalar` mkSmallDouble weightC
        d' = d `multScalar` mkSmallDouble weightD
        -- weights add to 1, each is in [0,1]
        -- closer => closer to 1
        -- further => closer to 0
        weightA = (1 - xFrac) * (1 - yFrac)
        weightB = xFrac * (1 - yFrac)
        weightC = (1 - xFrac) * yFrac
        weightD = xFrac * yFrac
        a = getIdx (xBase, yBase)
        b = getIdx (xBase + 1, yBase)
        c = getIdx (xBase, yBase + 1)
        d = getIdx (xBase + 1, yBase + 1)
        getIdx idx = word8ToSmallDouble <$> fromMaybe 0 (img !? idx)
        (xBase, xFrac) = properFraction x'
        (yBase, yFrac) = properFraction y'
        x' = x + (fromIntegral w / 2)
        y' = y + (fromIntegral h / 2)
        (w,h) = imageSize img

-- | Image to functional image, rounding points to the image's nearest pixel.
imageToFImageN :: Image RGBA -> FImage
imageToFImageN img (x,y) = fmap word8ToSmallDouble v
    where
        v = fromMaybe 0 (img !? idx)
        x' = round (x + (fromIntegral w / 2))
        y' = round (y + (fromIntegral h / 2))
        idx = (x', y')
        (w,h) = imageSize img

-- | Moves points to other points.
applyTransformF :: (Point -> Point) -> FImage -> FImage
applyTransformF f img = img . f

-- | Change from polar (r,θ) to Cartesian (x,y) coordinates.
--
-- Angle θ is in radians.
fromPolarF :: FImage -> FImage
fromPolarF img (x,y) = img (sqrt (x*x + y*y), atan2 y x)

-- | Change from Cartesian (x,y) to polar (r,θ) coordinates.
--
-- Angle θ is in radians.
toPolarF :: FImage -> FImage
toPolarF img (r,θ) = img (r * cos θ, r * sin θ)

-- | Overlays the second image given on the first.
overlayImageF :: FImage -> FImage -> FImage
overlayImageF img1 img2 idx =
    let Pixel4 r1 g1 b1 a1 = img1 idx
        Pixel4 r2 g2 b2 a2 = img2 idx
        c1 = smallDoubleToDouble <$> Pixel3 r1 g1 b1
        c2 = smallDoubleToDouble <$> Pixel3 r2 g2 b2
        a1' = smallDoubleToDouble a1
        a2' = smallDoubleToDouble a2
        a = a2' + a1'*(1 - a2')
        c2' = c2 `multScalar` a2'
        c1' = c1 `multScalar` (a1'*(1-a2'))
        Pixel3 r g b = fmap (/a) (c2' + c1')
    in mkSmallDouble <$> Pixel4 r g b a

-- | Apply a process to a functional image a given number of times.
repeatProcessF :: Int -> (FImage -> FImage) -> FImage -> FImage
repeatProcessF n f img
 | n <= 0 = img
 | otherwise = repeatProcessF (n-1) f (f img)
