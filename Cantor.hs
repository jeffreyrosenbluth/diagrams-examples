{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List (foldl')

-- Identical to juxtapose from Diagrams.core except a2 is placed inside of a1
juxtaposeNeg :: (Enveloped a, HasOrigin a) => V a -> a -> a -> a
juxtaposeNeg v a1 a2 =
    case (mv1, mv2) of
        (Just v1, Just v2) -> moveOriginBy (v1 ^-^ v2) a2
        _                  -> a2
    where mv1 = negateV <$> envelopeVMay v a1
          mv2 = envelopeVMay (negateV v) a2

-- Same as appends but using juxtaposeNeg instead of juxtapose
appendsNeg d1 apps = d <> d1
  where d = mconcat . reverse $ (map (\(v,d) -> juxtaposeNeg v d1 d) apps)

colors = [darkgreen, darkred]
c = circle 1 # lw 0

button n r = button' n n r

button' 1 _ r = circle r # lw 0
button' n m r = appendsNeg (button' (n-1) m r) cdirs
  where
    dirs  = [unit_X, unitX]
    c     = button' (n-1) m (r/2) # fc (colors !! ((m-n-1) `mod` 2)) # lw 0
    cdirs = zip dirs [c, c]

cheese :: Int -> Int -> Diagram B R2
cheese n m = wedge 1 (0 :: Turn) (fromIntegral (48-m)/48) # fc black # lw 0
          <> button n 1 <> circle 1 # lw 0 # fc (colors !! 0) # pad 1.1

main = mainWith cheese