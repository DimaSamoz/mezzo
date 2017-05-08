{-# LANGUAGE NoImplicitPrelude #-}

module Chopin.Part2 (part2) where

import Mezzo

part2Rh1 = ef maj3D qc :|: ef fourthD qc :|: (d' qn :-: (af en' :|: gf sn) :-: d qn) :|: d fourthD qc
part2Lh1 = c__ oct qc :|: c_ oct qc :|: b__ oct qc :|: bf__ oct qc

part2Rh2 = c fifthD qc :|: d maj3D qc {-:-: c qn -} :|: g_ maj' i2 ec' :|: pad (a_ min3 inv sc) :|: g_ maj inv qc
part2Lh2 = a__ oct qc :|: af__ oct qc :|: g__ oct qc :|: f__ oct qc

part2Rh3 = c fifthD qc :|: af_ maj3D qc :|: ((g en' :|: f sn) :-: g_ fifth qc) :|: c_ min' i2 qc
part2Lh3 = ef__ oct qc :|: f__ oct qc :|: b_3 oct qc :|: c__ oct qc

part2Rh4 = af__ majD' i2 qc :|: df_ majD inv qc :|: ((ef en' :|: d sn) :-: g_ maj3 qc :-: f_ qn) :|: pad (c_ min inv qc)
part2Lh4 = af_3 oct qc :|: df_3 oct qc :|: g_3 oct qc :|: c_3 oct qc

part2Rh = pad (part2Rh1 :|: part2Rh2 :|: part2Rh3) :|: part2Rh4
part2Lh = part2Lh1 :|: part2Lh2 :|: part2Lh3 :|: part2Lh4

part2 = section "Second part" $
            score setKeySig c_min
                  setTempo 30
                  withMusic (hom part2Rh part2Lh)
