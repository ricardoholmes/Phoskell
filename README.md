# Dissertation
Functional image processing library in Haskell

## Example Code
Once this library is complete, given that there exists an RGB image named "flower.png",
the following code should run without errors:

```hs
preProcess :: Image RGB -> Image Gray
preProcess img = img
               :> gaussianBlur (27,27) 0
               :> PointProcess (\p -> p `dot` Pixel3 1.75 (-0.5) (-0.5))

segment :: Image Gray -> Image Binary
segment img = img :> otsuThreshold

postProcess :: Image Binary -> Image Binary
postProcess img = img
                :> morphOpen (structuringElem Rect (7,7))
                :> morphClose (structuringElem Ellipse (16,16))

applyEdgeDetection :: Image Binary -> Image Binary
applyEdgeDetection img = ((img :> morphDilate k) - img)
                       :> PointProcess (1-)
        where
            k = structuringElem Ellipse (19,19)

applyPipeline :: Image RGB -> Image RGB
applyPipeline img = generateImage (size img) (\idx ->
                        Pixel3 (outline ! idx) (background ! idx) 0
                    )
    where
        background = img :> preProcess :> segment :> postProcess
        outline = background :> applyEdgeDetection

main :: IO ()
main = do img <- readImageRGB "flower.png"
          writeImage "segmented.png" (applyPipeline img)
```

Note: *Some minor changes to the syntax may be made (i.e. type names),
so those will have to be adjusted for.*
