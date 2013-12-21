> {-# LANGUAGE FlexibleContexts      #-}

> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Data.Colour (blend)

> locs   = [(x, y) | x <- [0.1, 0.3 .. 3.25], y <- [0.1, 0.3 .. 3.25]]

Create a list of points where the vectors will be placed.

> points = map p2 locs

The function to use to create the vector field.

> vectorField (x, y) = r2 (sin (y + 1), sin (x + 1))
>
> arrows = map arrowAtPoint locs
>
> arrowAtPoint (x, y) = arrowAt' opts (p2 (x, y)) (sL *^ vf) # alignTL # lc c
>   where
>     vf   = vectorField (x, y)
>     m    = magnitude $ vectorField (x, y)

Head size is a function of the length of the vector
as are tail size and shaft length.

>     hs   = 0.08 * m
>     sW   = 0.015 * m
>     sL   = 0.01 + 0.1 * m
>     c    = blend (m / sqrt 2 - 0.7) red midnightblue
>     opts = (with & arrowHead .~ spike
>                  & headSize .~ hs
>                  & shaftStyle %~ lw sW)
>
> field   = position $ zip points arrows
>
> example = ( field # translateY 0.05
>        <> ( square 3.5 # fc ghostwhite # lw 0.02 # alignBL))
>         # scaleX 2
>
