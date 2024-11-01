# Dissertation
Functional image processing library in Haskell

## Example Code
Once this library is complete, given that there exists an RGB image named "flower.png",
the following code should run without errors:

```hs
brightness :: PixelRGB -> PixelGray
brightness = prod
-- hopefully doesnt have to be:
-- brightness p = clamp (0,255) floorToInt (prod (fromIntegral p / 255.0) * 255.0)
-- worrying about pixel depth should preferrably be handled by the library

redness :: PixelRGB -> PixelGray
redness = dot [-1, 0.5, 0.5]

colourSpace :: PixelRGB -> PixelGray
colourSpace p = redness p + brightness p

getBackground :: ImageGray -> ImageBinary
getBackground img = img
                    :> otsuThreshold
                    :> open (ellipse (13,13))
                    :> close (ellipse (35,35))
                    :> erode (ellipse (15,15))
                    :> open (ellipse (12,12))

getOutline :: ImageBinary -> ImageBinary
getOutline img = 1 - (noOutline - img)
    where noOutline = img :> dilate (ellipse (31,31))
-- if binary images are handled as booleans, then it may have to be:
-- not (noOutline `xor` img)

pipeline :: ImageRGB -> ImageRGB
pipeline img = fmap (* red) background + fmap (* green) outline
    where
        background = img :> fmap colourSpace :> getBackground
        outline = pipeline :> getOutline

main :: IO ()
main = do img <- readImage "flower.png"
          writeImage "segmented.png" (pipeline img)
```

Note: *Some minor changes to the syntax may be made (i.e. type names),
so those will have to be adjusted for.*
