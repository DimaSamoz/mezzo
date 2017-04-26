
module FurElise.Episode2 (episode2) where

import Mezzo

-------------------------------------------------------------------------------
-- Second episode
-------------------------------------------------------------------------------

ep2Rh1 = play $ melody
            :<<< c' :| g' :| g :| g' :| a :| g' :| b :| g' :| c' :| g' :| d' :| g'
            :| e' :| g' :| c'' :| b' :| a' :| g' :| f' :| e' :| d' :| g' :| f' :| d'

ep2Lh1 = pad (c maj3 ec :|: pad (r sr)
     :|:      g sn :-: f sn
     :|:      e min3 sc)
     :|:      g sn :-: f sn :-: d sn
     :|:      c maj ec
     :|: pad (f_ maj3 ec
     :|:      g_ maj3 ec)

ep2Rh2 = play $ melody
            :<<< e' :| f' :| e :| ds' :| e' :| b :| e' :| ds' :| e' :| b :| e' :| ds'
            :<. e' :<< b :| e' :| ds' :<. e' :<< b :| e' :| ds' :| e' :| ds' :| e' :| ds'

ep2Lh2 = gs_ min3 ec :|: pad (r qr :|: r wr)

ep2part = score setTempo 90
                setKeySig c_maj
                withMusic

ep2p1 = section "2nd episode, part 1" $
            ep2part (hom ep2Rh1 ep2Lh1)

ep2p2 = section "2nd episode, part 2" $
            ep2part (hom ep2Rh2 ep2Lh2)

episode2 = ep2p1 ++ ep2p1 ++ ep2p2
