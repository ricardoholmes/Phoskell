-- | Image stacking functions.
module Graphics.Phoskell.Synthesis.Stack (
    -- image stacking
    stackVertically,
    stackHorizontally,
    quadrants,
) where

import Data.Massiv.Array (Ix2(..))

import qualified Data.Massiv.Array as M

import Graphics.Phoskell.Core
import Graphics.Phoskell.Analysis (imageSize)
import Graphics.Phoskell.Transformations (zoomToSize)
import Graphics.Phoskell.Synthesis.Internal (generateImage)

-- | Stack two images vertically on top of one another to create a new image.
--
-- Given that the upper image's dimensions are @(w1,h1)@, and the lower image's
-- are @(w2,h2)@, the output image's dimensions are @(max(w1,w2), h1+h2)@.
--
-- If the images do not share the same width, the less wide image will be
-- centered horizontally and the empty space on its sides will be filled with
-- the colour given.
stackVertically :: Pixel p => p Word8 -- ^ Image that goes on the top.
                           -> Image (p Word8) -- ^ Image that goes on the bottom.
                           -> Image (p Word8) -- ^ Colour for the background, in case the images don't share the same width.
                           -> Image (p Word8)
stackVertically bg top bot = generateImage (0,-h1) (w-1,h2-1) (\(x,y) ->
                                if y < 0
                                    then M.evaluate' top' (y + h1:.x)
                                    else M.evaluate' bot' (y:.x)
                            )
    where
        (w1,h1) = imageSize top
        (w2,h2) = imageSize bot
        w = max w1 w2
        top' = toArray $ if w == w1 then top else top :> zoomToSize (w,h1) bg
        bot' = toArray $ if w == w2 then bot else bot :> zoomToSize (w,h2) bg

-- | Stack two images horizontally to create a new image.
--
-- Given that the upper image's dimensions are @(w1,h1)@, and the lower image's
-- are @(w2,h2)@, the output image's dimensions are @(max(w1,w2), h1+h2)@.
--
-- If the images do not share the same width, the less wide image will be
-- centered vertically and the empty space on its sides will be filled with
-- the colour given.
stackHorizontally :: Pixel p
                  => p Word8 -- ^ Image that goes on the left.
                  -> Image (p Word8) -- ^ Image that goes on the right.
                  -> Image (p Word8) -- ^ Colour for the background, in case the images don't share the same width.
                  -> Image (p Word8)
stackHorizontally bg top bot = generateImage (-w1,0) (w2-1,h-1) (\(x,y) ->
                                if x < 0
                                    then M.evaluate' top' (y:.x + w1)
                                    else M.evaluate' bot' (y:.x)
                            )
    where
        (w1,h1) = imageSize top
        (w2,h2) = imageSize bot
        h = max h1 h2
        top' = toArray $ if h == h1 then top else top :> zoomToSize (w1,h) bg
        bot' = toArray $ if h == h2 then bot else bot :> zoomToSize (w2,h) bg

-- | Place four images into quadrants.
--
-- Given image dimensions @(w1,h1)@, @(w2,h2)@, @(w3,h3)@, and @(w4,h4)@, in
-- order of the parameters, the output image will have dimensions
-- @(max(w1,w3)+max(w2,w4), max(h1,h2)+max(h3,h4))@.
quadrants :: Pixel p 
          => p Word8 -- ^ Background colour.
          -> Image (p Word8) -- ^ Top-left image.
          -> Image (p Word8) -- ^ Top-right image.
          -> Image (p Word8) -- ^ Bottom-left image.
          -> Image (p Word8) -- ^ Bottom-right image.
          -> Image (p Word8)
quadrants bg tl tr bl br = generateImage (-leftW, -topH) (rightW-1, botH-1) (\(x,y) ->
                                case (y<0,x<0) of
                                    (True,True) -> M.evaluate' tl' (y+topH:.x+leftW)
                                    (True,False) -> M.evaluate' tr' (y+topH:.x)
                                    (False,True) -> M.evaluate' bl' (y:.x+leftW)
                                    (False,False) -> M.evaluate' br' (y:.x)
                            )
    where
        (w1,h1) = imageSize tl
        (w2,h2) = imageSize tr
        (w3,h3) = imageSize bl
        (w4,h4) = imageSize br
        topH = max h1 h2
        botH = max h3 h4
        leftW = max w1 w3
        rightW = max w2 w4
        tl' = toArray $ if topH == h1 && leftW == w1 then tl else tl :> zoomToSize (leftW,topH) bg
        tr' = toArray $ if topH == h2 && rightW == w2 then tr else tr :> zoomToSize (rightW,topH) bg
        bl' = toArray $ if botH == h3 && leftW == w3 then bl else bl :> zoomToSize (leftW,botH) bg
        br' = toArray $ if botH == h4 && rightW == w4 then br else br :> zoomToSize (rightW,botH) bg
