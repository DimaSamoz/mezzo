
import Mezzo

rh1 = c_ minD inv inv qc :|: af_ majD qc :|: (((g en' :-: ef en') :|: (f sn :-: d sn)) :-: (b_ qn :-: g_ qn)) :|: c_ minD inv qc
lh1 = (c_ qn :-: c__ qn) :|: (f__ qn :-: f_3 qn) :|: (g__ qn :-: g_3 qn) :|: (c_ qn {- :-: g__ qn -} :-: c__ qn)

rh2 = af__ majD' i2 qc :|: cs_ majD inv qc :|: (((ef en' :-: c en') :|: (df sn :-: bf_ sn)) :-: (g_ qn :-: ef_ qn {- :-: df_ hn -})) :|: af__ majD inv qc
lh2 = (af__ qn :-: af_3 qn) :|: (df__ qn :-: df_3 qn) :|: (ef__ qn :-: ef_3 qn) :|: (af__ qn :-: af_3 qn)

rh3 = b__ dimD inv qc :|: c_ majD inv qc :|: pad (((g en' :|: f sn) :-: c qn :-: af_ qn) :|: c_ Mezzo.min inv inv qc)

rh = rh1 :|: rh2
lh = lh1 :|: lh2

comp = hom rh lh
