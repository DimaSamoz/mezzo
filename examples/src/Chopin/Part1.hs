{-# LANGUAGE NoImplicitPrelude #-}

module Chopin.Part1 (part1) where

import Mezzo

part1Rh1 = c_ minD inv inv qc :|: af_ majD qc :|: ((ef maj3 ec' :|: d min3 sc) :-: g_ maj3 qc) :|: c_ minD inv qc
part1Lh1 = c__ oct qc :|: f_3 oct qc :|: g_3 oct qc :|: c__ oct qc {- :-: g__ qn -}

part1Rh2 = af__ majD' i2 qc :|: cs_ majD inv qc :|: ((c min3 ec' :|: bf_ min3 sc) :-: g_ qn :-: ef_ qn {- :-: df_ hn -}) :|: af__ majD inv qc
part1Lh2 = af_3 oct qc :|: df_3 oct qc :|: ef_3 oct qc :|: af_3 oct qc

part1Rh3 = b__ dimD inv qc :|: c_ majD inv qc :|: pad2 ((g en' :|: f sn) :-: c qn {- :-: af_ qn -}) :|: pad (c_ min inv inv qc)
part1Lh3 = g_3 oct qc :|: c_3 oct qc :|: f_3 oct qc :|: c__ oct qc

part1Rh4 = d_ dom7 inv qc :|: g_ majD qc :|: pad (((b en' :|: a sn) :-: fs qn :-: d qn {- :-: c qn -}) :|: g_ maj inv qc)
part1Lh4 = d__ oct qc :|: g_3 oct qc :|: d_3 oct qc :|: g_3 oct qc

part1Rh = part1Rh1 :|: part1Rh2 :|: part1Rh3 :|: part1Rh4
part1Lh = part1Lh1 :|: part1Lh2 :|: part1Lh3 :|: part1Lh4

part1 = score section "first part"
              setKeySig c_min
              setTempo 30
              withMusic (hom part1Rh part1Lh)
