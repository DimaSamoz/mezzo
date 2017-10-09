{-# LANGUAGE GADTs #-}

module Other.ClappingMusic where

import Mezzo


sco = score setTempo 200 withMusic

-- African bell pattern
patt =
    [True, True, True, False, True, True, False, True, False, True, True, False]

-- Rotate list
rotate (v : vs) = vs ++ [v]

-- Make claps from pattern
claps = flatten . map (\n -> if n then sco $ c en else sco $ r er)

claps1 = reprise 52 (claps patt)

claps2 p = reprise 4 (claps p) : claps2 (rotate p)

clappingMusic = claps1 >< transpose 7 (flatten $ take 13 (claps2 patt))
