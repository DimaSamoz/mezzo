
import Mezzo.Model
import Mezzo.Compose.Basic
import Mezzo.Compose.Combine
import Mezzo.Compose.Types

import Mezzo.Render.MIDI

v1_1 = play $ Melody :| g :| a :| b :| g
v2_1 = play $ Melody :| b_ :| c :| g :| b_
v3_1 = play $ Melody :| d :| d :| d :| d
v4_1 = play $ Melody :| g_ :| fs_ :| e_ :| d_

fj_verse1 = v1_1 :-: v2_1 :-: v3_1  :-: v4_1

v1_2 = b qn :|: c' qn :|: d' hn
v2_2 = d qn :|: e qn :|: fs hn
v3_2 = g_ qn :|: d qn :|: b_ hn
v4_2 = b_ qn :|: a_ qn :|: d_ hn

fj_verse2 = v1_2 :-: v2_2 :-: v3_2 :-: v4_2

fj = fj_verse1 :|: fj_verse1 :|: fj_verse2 :|: fj_verse2
