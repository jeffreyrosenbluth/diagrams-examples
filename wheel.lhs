> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE MultiParamTypeClasses          #-}
> {-# LANGUAGE FlexibleContexts               #-}
>
> import Diagrams.Prelude
> import Diagrams.Backend.Cairo.CmdLine
>
> bullseye = circle 0.2 # fc orangered
>                       # lw 0
>                       # named "bullseye"
>
> target = circle 1 # fc gold # named "target"
>
> d = bullseye <> target
>
> shaft = arc 0 (1/6 :: Turn)
>
> connectTarget :: (Angle a, Renderable (Path R2) b)
>               =>  a -> (Diagram b R2 -> Diagram b R2)
> connectTarget a = connectPerim' (with & arrowHead .~ thorn & shaftStyle %~  lw 0.01
>                                       & arrowShaft .~ shaft & headSize .~ 0.18
>                                       & arrowTail .~ thorn'
>                                      ) "target" "bullseye" a a
>
> angles :: [Turn]
> angles = [0, 1/16 .. 15/16]
>
> example = foldr connectTarget d angles
> main = defaultMain $ example # pad 1.1