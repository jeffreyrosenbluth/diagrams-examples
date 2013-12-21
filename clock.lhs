> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
>
> quads      = vcat $ map (hcat . colorBar) [0..3]
> colorBar k = [square 1 # fc (d3Quad k i) | i <- [0..9]]
>
> main =defaultMain $ quads