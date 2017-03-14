
-- Based on http://decipheringmusictheory.com/?page_id=46

import Mezzo.Model
import Mezzo.Compose.Basic
import Mezzo.Compose.Combine
import Mezzo.Compose.Types

import Mezzo.Render.MIDI

v1 = d qn :|: g qn :|: fs qn :|: g en :|: a en :|: bf qn :|: a qn :|: g hn

v2 = d qn :|: ef qn :|: d qn :|: bf_ en :|: a_ en :|: g_ qn :|: fs_ qn :|: g_ hn

comp = v1 :-: v2
