{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

triEx = arrowAt' with {arrowHead=tri, headSize=0.5}
        origin (r2 (0.01, 0))
    <> square 1 # alignL # lw 0


main = defaultMain $ triEx