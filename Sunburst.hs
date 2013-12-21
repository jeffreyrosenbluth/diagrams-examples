{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Sunburst
-- Copyright   :  (c) 2013 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Generation of Sunburst Partitions. A radial view of a Treemap.
--
-- See John Stasko, Richard Catrambone, \"An evaluation of space-filling
-- information visualizations for depicting hierarchical structures\", 2000.
-- <http://www.cc.gatech.edu/~john.stasko/papers/ijhcs00.pdf>.
--
-- Example:
--
-- > import Data.Tree
-- > import Diagrams.TwoD.Sunburst
-- > aTree = unfoldTree (\n -> (0, replicate n (n-1))) 6
-- > colors = [ lightgray, lightcoral, lightseagreen, paleturquoise
-- >          , lightsteelblue, plum]
-- > sunburstEx = sunburst aTree colors
-- <<#diagram=sunburstEx&width=500>>
-----------------------------------------------------------------------------

module Diagrams.TwoD.Sunburst
  ( --  * Sunburst
    sunburst
  ) where

import           Data.Tree
import           Data.Foldable

-- Section data: Will be stored in nodes of a new rose tree and used to
-- make each section of the sunburst partition.
-- radius, ring width, start angle, end angle, number of sections, color.
data SData = SData Double Double Turn Turn Int (Colour Double)

colors = [ lightgray, lightcoral, lightseagreen, paleturquoise, lightsteelblue
         , plum, violet, coral, honeydew]

-- Make n sections (annular wedges) spanning a1 to a2.
sections :: Renderable (Path R2) b
        => Double -> Double -> Turn -> Turn -> Int -> (Colour Double)
        -> Diagram b R2
sections r s a1 a2 n c = mconcat $ iterateN n (rotate theta) w
  where
    theta = (a2 - a1) / (fromIntegral n)
    w = annularWedge (s + r) r a1 (a1 + theta)
      # lc white # lw 0.008 # fc c

toTree :: Double -> Double-> [(Colour Double)] -> Tree a -> Turn -> Turn  -> Tree SData
toTree r s (c:cs) (Node _ ts) q1 q2
  = Node (SData r s q1 q2 n c) ts'
      where
        n = length ts
        d =  (q2 - q1) / (fromIntegral n)
        qs = [q1 + ((fromIntegral i) * d ) | i <- [0..n]]
        fs = toTree (r + s) s cs
        ts' = zipWith3 fs ts (take (n-1) qs) (drop 1 qs)

-- | Take any @Tree a@ and a list of colors and make a sunburst partition.
--   Basically a treemap with a radial layout.
--   The root is the center of the sunburst and its circumference is divided
--   evenly according to the nuber of child nodes it has. The each of those
--   sections is treated the same way.
sunburst :: Renderable (Path R2) b => Tree a -> [(Colour Double)] -> Diagram b R2
sunburst t cs
  = sunB $ toTree 1 0.3 cs t 0 1
      where
        sunB (Node (SData r m a1 a2 n c) ts')
          = sections r m a1 a2 n c <> (foldMap sunB ts')

t = unfoldTree (\n -> (0, replicate n (n-1))) 6

example = sunburst t colors

main = defaultMain $ example # centerXY # pad 1.1