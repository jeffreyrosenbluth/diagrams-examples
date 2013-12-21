{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad                 (replicateM)
import           Control.Monad.Random
import           Data.Colour.Palette.ColorSet
import           Data.List                     (zipWith, zipWith3)

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

import           System.Random

type Dia = Diagram B R2

sizeValue :: (RandomGen g) => Rand g Double
sizeValue = getRandomR (0.05, 0.25)

coordValue :: (RandomGen g) => Rand g Double
coordValue = getRandomR (-0.5, 0.5)

-- Generate a random set of confetti to use as the object in the
-- kaleidescope.
confetti :: Int -> Rand StdGen Dia
confetti n = do
  ss <- replicateM n sizeValue   -- radius
  cs <- replicateM n getRandom   -- color index
  as <- replicateM n getRandom   -- opacity
  xs <- replicateM n coordValue  -- x coordinate
  ys <- replicateM n coordValue  -- y coordinate
  let mkCirc :: Double -> Int -> Double -> Dia
      mkCirc s c a = circle s # fc (webColors c) # opacity a
      pos = zipWith mkP2 xs ys
      conf = zipWith3 mkCirc ss cs as
  return $ position (zip pos conf)

-- Make a confetting diagram and extract it from the monad.
mkConfetti :: Int -> (StdGen -> Dia)
mkConfetti n = evalRand $ confetti n

-- Clip a diagram to an equilateral traingle.
mkTriangle :: Dia -> Dia
mkTriangle = clipTo (triangle 1)

-- Version of clipBy that take on the trace and envelope of the clippoing
-- pathe.
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

kaleidescope :: Int -> Int -> Dia
kaleidescope n r
  = kaleid (mkConfetti n (mkStdGen r))
          # centerXY <> (circle 2.75 # fc black)
          # pad 1.1

main = mainWith $ kaleidescope