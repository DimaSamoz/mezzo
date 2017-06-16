{-# LANGUAGE NoImplicitPrelude, GADTs #-}

module FurElise.Episode3 (episode3) where

import Mezzo

-------------------------------------------------------------------------------
-- Third episode
-------------------------------------------------------------------------------

-- Repeat a composition 6 times.
repeat6 n = n :|: n :|: n :|: n :|: n :|: n

ep3Rh1 = pad3 (a en :|: r qr)
     :|:       cs dim7 inv qc'
     :|: pad  (d min inv qc)
     :|: pad2 (cs' min3 sc
     :|:       d' min3 sc)
     :|: pad  (d dim' i2 qc
     :|:       d dim' i2 ec
     :|:       a min qc')

ep3Lh1 = rep :|: rep :|: rep :|: rep :|: rep
    where rep = repeat6 (a__ sn)

ep3Rh2 = pad2 (d min3 inv qc
     :|:       c maj3 inv sc
     :|:       b_ min3 inv sc)
     :|: pad  (fs_ dim' i2 qc)
     :|: pad2 (a_ min3 inv ec
     :|:       a_ min3 inv ec
     :|:       c maj3 inv ec
     :|:       b_ min3 inv ec
     :|:       a_ min3 inv qc')

ep3Lh2 = repeat6 (d__ fifth sc)
     :|: repeat6 (ds__ sn :-: a__ sn)
     :|: e__ fourth sc :|: e__ fourth sc :|: e__ fourth sc
     :|: e__ fourth sc :|: e__ maj3 sc :|: e__ maj3 sc
     :|: a_3 oct sc :|: pad (play $ melody :<< a__ :| a__ :| a__ :| a__ :| a__)

ep3Rh3 =       cs dim7 inv qc'
     :|: pad  (d min inv qc)
     :|: pad2 (cs' min3 sc
     :|:       d' min3 sc
     :|:       d' min3 qc
     :|:       d' min3 ec
     :|:       d' min3 qc'
     :|:       ef maj3 inv qc
     :|:       d min3 inv sc
     :|:       c min3 inv sc)
     :|: pad  (bf_ maj inv qc
     :|:       d min ec
     :|:       d dim qc
     :|:       d dim ec
     :|:       a_ min inv qc)
     :|: pad3 (r er)
     :|: pad  (b_ fourth inv ec :-: gs_ en)
     :|: pad3 (r qr)

ep3Lh3 = aNotes :|: aNotes :|: aNotes :|: bfNotes :|: bfNotes :|: bfNotes :|: bNotes
     :|: c_ qn :|: r er :|: e_ en :|: r qr
    where aNotes = repeat6 (a__ sn)
          bfNotes = repeat6 (bf__ sn)
          bNotes = repeat6 (b__ sn)

ep3part sname = score section sname
                setTempo 90
                setKeySig d_min
                withMusic

ep3p1 = ep3part "3rd episode, part 1" (hom ep3Rh1 ep3Lh1)

ep3p2 = ep3part "3rd episode, part 2" (hom ep3Rh2 ep3Lh2)

ep3p3 = ep3part "3rd episode, part 3" (hom ep3Rh3 ep3Lh3)

episode3 = ep3p1 ++ ep3p2 ++ ep3p3
