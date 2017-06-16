{-# LANGUAGE NoImplicitPrelude #-}

module FurElise where

import Mezzo

import FurElise.Theme
import FurElise.Episode1
import FurElise.Episode2
import FurElise.Episode3
import FurElise.Episode4

-------------------------------------------------------------------------------
-- End theme
-------------------------------------------------------------------------------

endChord = score section "end chord"
                 setTempo 90
                 setKeySig a_min
                 withMusic (hom (a qn) (a_3 oct qc))

main = renderScores "rendered/FurElise.mid"
        "Ludwig van Beethoven - Fur Elise"
        [ theme
        , episode1, episode2
        , theme
        , episode3, episode4
        , theme
        , endChord
        ]
