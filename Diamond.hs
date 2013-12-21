{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude hiding (tan, gray)
import Diagrams.Backend.SVG.CmdLine

import Data.List.Split      (chunksOf)
import Data.Maybe           (catMaybes)
import Control.Applicative
import Data.Monoid          (mconcat)
import Data.List            (transpose)

makeFlake :: Diagram B R2 -> Diagram B R2
makeFlake d = d4 <> d3
  where d0 = d  # rotateBy (1/8)
                # scaleX (1 / tan (pi / 8))
                # rotateBy (3/16)
        d1 = reflectX d0 # alignR <> d0 # alignL
        d2 = d1 # reflectY # alignT
        d3 = d2 <> d1 # alignB
        d4 = d3 # rotateBy (1/4)

main = defaultMain $ makeFlake example # centerXY # pad 1.1

-- First create a fucntion to make a circle of size linear in n/m
-- and stroke width decreasing exponentially with n.
circ n m = unitCircle # scale (0.05 + n / m) # lw (0.06 / exp (0.2 * n))

-- Create m - n + 1 concentric circles, starting with n.
-- Decreasing stroke width as circles get larger.
ccirc n m = mconcat [circ i m | i <- [m, (m-1)..n]]

-- Constant for translation along x = y.
d = sqrt 2 / 2

cc21 = ccirc 1 21
pic = cc21 <> cc21 # translate (r2 (d, d)) <> cc21 # translate (r2 (-d, d))

gray 0 = [[]]
gray n = map (False:) g ++ map (True:) (reverse g)
  where g = gray (n-1)

rings n = mkRingsDia . map ringOffsets . transpose . gray $ n
  where ringOffsets :: [Bool] -> [(CircleFrac, CircleFrac)]
        ringOffsets = map l2t . chunksOf 2 . findEdges . zip [0,1/(2^n)..1]
        l2t [x,y] = (x,y)
        l2t [x]   = (x,1)
findEdges :: Eq a => [(CircleFrac, a)] -> [CircleFrac]
findEdges = catMaybes . (zipWith edge <*> tail)
  where edge (_,c1) (a,c2) | c1 /= c2  = Just a
                           | otherwise = Nothing

mkRingsDia = freeze . mconcat . zipWith mkRingDia [2,3..]
  where mkRingDia r = lw 1.05 . mconcat . map (stroke . scale r . uncurry arc)

hilbert = iterate expand mempty where
  expand t = alignBL $ hcat [u, hrule 1, reflectX u] where
             u = vcat [t, alignT $ vrule 1, rotateBy (3/4) t]

example = pad 1.1 . centerXY . lw 0.1 $ hilbert!!5