{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts      #-}

import Diagrams.Prelude
import Diagrams.TwoD.Transform.ScaleInv
import Diagrams.Backend.SVG.CmdLine

sideWidth = 0.01
t :: Located (Trail R2)
t = polygon (with & polyType .~ PolySides [(1/3::Turn)] [1,2])
t' :: [Located (Trail R2)]
t' = explodeTrail t
ds :: [Turn]
ds = map ((1/4 +) . direction) (trailOffsets . unLoc $ t)
pts = zipWith atParam t' (repeat 0.5)
dot' :: Path R2
dot' = square sideWidth # scaleX 5
dot = (scaleInvPrim  dot' unitX)  # lw 0
dots = zipWith rotate ds [dot # fc red, dot # fc lightblue, dot # fc yellow]
x = mconcat (zipWith place dots pts) <> strokeLocTrail t # lw sideWidth # fc blue # opacity 0
x' = reflectX x
x'' = x' # scale 0.4

main =defaultMain $ (x' ||| strutX 0.25 ||| x'' # rotateBy (1/3)) # centerXY # pad 1.1 # bg lightgray