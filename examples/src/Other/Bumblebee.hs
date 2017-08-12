
module Bumblebee where

import Mezzo

sco = score setRuleSet free setTempo 180 withMusic

four = scale 4 (-1)
eight = scale 2 (-2) . four
twelve = scale 3 (-2) . four

passage = twelve n +++ transpose (-8) (four n)
    where n = sco $ e'' sn

reps = reprise 4 $ eight (sco $ e sn)

thm vs =  reprise 2 (scale 5 (-1) (sco $ e sn) +++ scale 3 (-1) (sco $ f sn)
               +++ scale 4 (-1) (sco $ e sn) +++ scale 4 1 (sco $ c sn))
    +++ volta (eight (sco $ e sn) +++ scale 5 1 (sco $ c sn)) vs

octs b = b +++ reprise 3 (scale 2 1 (sco $ af_ sn))
    +++ reprise 4 (sco $ (bf_ sn :-: af_ sn) :|: (bf sn :-: a_ sn))

bumblebee = flatten
    [ passage
    , transpose (-12) passage
    , reps
    , thm [scale 3 (-1) (sco $ f sn), scale 3 1 (sco $ fs sn)]
    , transpose 5 (thm (replicate 2 (scale 3 (-1) (sco $ f sn))))
    , octs (scale 2 (-12) (sco $ a sn))
    , transpose 12 $ octs (reprise 2 (sco $ a_ sn))
    , sco $ a oct hc
    ]
