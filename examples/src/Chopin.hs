{-# LANGUAGE NoImplicitPrelude #-}

module Chopin where

import Mezzo

import Chopin.Part1
import Chopin.Part2

end = score section "end"
            setKeySig c_min
            setTempo 30
            withMusic (hom (c minD wc) (c_ fifth wc))

main = renderScores "rendered/Chopin.mid"
                    "Frederic Chopin - Prelude in C Minor"
                    [part1, part2, part2, end]
