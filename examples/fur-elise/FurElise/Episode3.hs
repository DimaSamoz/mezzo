{-# LANGUAGE NoImplicitPrelude #-}

module FurElise.Episode3 (episode3) where

import Mezzo

-------------------------------------------------------------------------------
-- Third episode
-------------------------------------------------------------------------------

ep3Rh1 = pad3 (a en :|: r qr)
     :|:       cs dim7 inv qc'
     :|: pad  (d min inv qc)
     :|: pad2 (cs' min3 sc
     :|:       d' min3 sc)
     :|: pad  (d dim' i2 qc
     :|:       d dim' i2 ec
     :|:       a min qc')

ep3Lh1 = notes :|: notes :|: notes :|: notes :|: notes
    where notes = play $ melody :<< a__ :| a__ :| a__ :| a__ :| a__ :| a__

ep3Rh2 = pad2 (d min3 inv qc
     :|:       c maj3 inv sc
     :|:       b_ min3 inv sc)
     :|: pad  (fs_ dim' i2 qc)
     :|: pad2 (a_ min3 inv ec
     :|:       a_ min3 inv ec
     :|:       c maj3 inv ec
     :|:       b_ min3 inv ec
     :|:       a_ min3 inv qc')

ep3Lh2 = d__ fifth sc :|: d__ fifth sc :|: d__ fifth sc
     :|: d__ fifth sc :|: d__ fifth sc :|: d__ fifth sc
     :|: ds__ sn :-: a__ sn :|: ds__ sn :-: a__ sn :|: ds__ sn :-: a__ sn
     :|: ds__ sn :-: a__ sn :|: ds__ sn :-: a__ sn :|: ds__ sn :-: a__ sn
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
    where aNotes = play $ melody :<< a__ :| a__ :| a__ :| a__ :| a__ :| a__
          bfNotes = play $ melody :<< bf__ :| bf__ :| bf__ :| bf__ :| bf__ :| bf__
          bNotes = play $ melody :<< b__ :| b__ :| b__ :| b__ :| b__ :| b__

ep3part = score setTempo 90
                setKeySig d_min
                withMusic

ep3p1 = section "3rd episode, part 1" $
            ep3part (hom ep3Rh1 ep3Lh1)

ep3p2 = section "3rd episode, part 2" $
            ep3part (hom ep3Rh2 ep3Lh2)

ep3p3 = section "3rd episode, part 3" $
            ep3part (hom ep3Rh3 ep3Lh3)

episode3 = ep3p1 ++ ep3p2 ++ ep3p3
