{-# LANGUAGE NoImplicitPrelude #-}

import Mezzo

rh1 = c_ minD inv inv qc :|: af_ majD qc :|: ((ef maj3 ec' :|: d min3 sc) :-: g_ maj3 qc) :|: c_ minD inv qc
lh1 = c__ oct qc :|: f_3 oct qc :|: g_3 oct qc :|: c__ oct qc {- :-: g__ qn -}

rh2 = af__ majD' i2 qc :|: cs_ majD inv qc :|: ((c min3 ec' :|: bf_ min3 sc) :-: g_ qn :-: ef_ qn {- :-: df_ hn -}) :|: af__ majD inv qc
lh2 = af_3 oct qc :|: df_3 oct qc :|: ef_3 oct qc :|: af_3 oct qc

rh3 = b__ dimD inv qc :|: c_ majD inv qc :|: pad2 ((g en' :|: f sn) :-: c qn {- :-: af_ qn -}) :|: pad (c_ min inv inv qc)
lh3 = g_3 oct qc :|: c_3 oct qc :|: f_3 oct qc :|: c__ oct qc

rh4 = d_ dom7 inv qc :|: g_ majD qc :|: pad (((b en' :|: a sn) :-: fs qn :-: d qn {- :-: c qn -}) :|: g_ maj inv qc)
lh4 = d__ oct qc :|: g_3 oct qc :|: d_3 oct qc :|: g_3 oct qc


rh5 = ef maj3D qc :|: ef fourthD qc :|: (d' qn :-: (af en' :|: gf sn) :-: d qn) :|: d fourthD qc
lh5 = c__ oct qc :|: c_ oct qc :|: b__ oct qc :|: bf__ oct qc

rh6 = c fifthD qc :|: d maj3D qc {-:-: c qn -} :|: g_ maj' i2 ec' :|: pad (a_ min3 inv sc) :|: g_ maj inv qc
lh6 = a__ oct qc :|: af__ oct qc :|: g__ oct qc :|: f__ oct qc

rh7 = c fifthD qc :|: af_ maj3D qc :|: ((g en' :|: f sn) :-: g_ fifth qc) :|: c_ min' i2 qc
lh7 = ef__ oct qc :|: f__ oct qc :|: b_3 oct qc :|: c__ oct qc

rh8 = af__ majD' i2 qc :|: df_ majD inv qc :|: ((ef en' :|: d sn) :-: g_ maj3 qc :-: f_ qn) :|: pad (c_ min inv qc)
lh8 = af_3 oct qc :|: df_3 oct qc :|: g_3 oct qc :|: c_3 oct qc

rh9 = c minD wc
lh9 = c_ fifth wc


rh_p1 = rh1 :|: rh2 :|: rh3 :|: rh4
lh_p1 = lh1 :|: lh2 :|: lh3 :|: lh4

rh_p2 = pad (rh5 :|: rh6 :|: rh7) :|: rh8
lh_p2 = lh5 :|: lh6 :|: lh7 :|: lh8

rh = rh_p1 :|: rh_p2 :|: rh9
lh = lh_p1 :|: lh_p2 :|: lh9

-- comp = hom rh lh
