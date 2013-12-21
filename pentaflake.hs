{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


hexacircle' 0 = regPoly 5 1 # lw 0

hexacircle' n = appends
                  pCenter
                  (zip vs (repeat (rotateBy (1/2) pOutside)))
  where vs = iterateN 5 (rotateBy (1/5))
           . (if odd n then negateV else id)
           $ unitY
        pCenter  = hexacircle' (n-1)
        pOutside = pCenter # opacity 0.8
hexacircle n = hexacircle' n # fc blue # bg white

main = defaultMain $ hexacircle 4 # pad 1.1