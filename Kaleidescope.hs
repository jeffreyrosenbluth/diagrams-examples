{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Colour.Palette.ColorSet
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

type Dia = Diagram B R2

mkTriangle :: Dia -> Dia
mkTriangle = clipTo (triangle 1)

clipTo :: Path R2 -> Dia -> Dia
clipTo p = (withTrace p) . (withEnvelope p) . (clipBy p)

-- Take any diagram and cut out a equilateral triangle of side 1 from the
-- center. This is the traingle inside of the three mirrors
kaleid :: Dia -> Dia
kaleid d = rotateBy (1/12) $ appends hex hexs
  where
    hexs = zip dirs (replicate 6 hex)
    dirs = iterate (rotateBy (1/6)) unitX
    hex = rotateBy (1/12) $
            (ts !! 2 # centerXY)
         <> (ts !! 1 # rotateBy (1/2) # snugT)
    ts = iterate flipTurn (mkTriangle d)
    flipTurn tri = (tri === tri # reflectY) # rotateBy (1/6)

-------------------------------------------------------------------------------
-- Create a skewed 4 X 10 grid of colors called quads to use as an example
-- diagram.
-------------------------------------------------------------------------------
gr :: Double
gr = (1 + sqrt 5) / 2 -- golden ratio

bar :: [Kolor] -> Dia
bar [] = centerXY $ square gr # fc black
bar cs = centerXY $ hcat [square gr # scaleX s # fc k # lw 0 | k <- cs]
  where s = gr / (fromIntegral (length cs))

grid :: [[Kolor]] -> Dia
grid [] = centerXY $ square gr # fc black
grid cs = centerXY $ vcat [bar c # scaleY s | c <- cs]
  where s = 1 / (fromIntegral (length cs))

d4 :: [[Kolor]]
d4 = [[d3Colors4 b n | n <- [0..9]] | b <- [Darkest, Dark, Light, Lightest]]

quads :: Dia
quads = grid d4 # rotateBy (1/7) # scale 0.76
-------------------------------------------------------------------------------

main = mainWith $ kaleid quads # centerXY
   <> (circle 2.75 # fc black)  #  pad 1.1