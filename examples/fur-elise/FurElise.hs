{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Mezzo

import FurElise.Theme
import FurElise.Episode1
import FurElise.Episode2
import FurElise.Episode3
import FurElise.Episode4

-------------------------------------------------------------------------------
-- End theme
-------------------------------------------------------------------------------

endChord = section "end chord" $
            score
                setTempo 90
                setKeySig a_min
                withMusic (hom (a qn) (a_3 oct qc))

main = renderSections "rendered/FurElise.mid"
        "Ludwig van Beethoven - Fur Elise"
        [ theme
        , episode1, episode2
        , theme
        , episode3, episode4
        , theme
        , endChord
        ]
