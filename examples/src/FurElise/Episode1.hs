
module FurElise.Episode1 (episode1) where

import Mezzo

-------------------------------------------------------------------------------
-- First episode
-------------------------------------------------------------------------------

ep1Rh1 = pad2 (a en :|: r sr)
     :|: pad (c maj3 inv sc :|: c fourth inv sc) :|: c maj inv sc

ep1Rh2 = play $ melody :| c' :<<. f' :<<< e' :< e' :| d'
                       :<<. bf' :<<< a' :<< a' :| g'

ep1Rh3 = play $ melody :<< f' :| e' :| d' :| c'

ep1Rh4 = play $ melody :< bf :| a :<<< a :| g :| a :| bf :^ c'
                       :<< d' :| ds' :<. e' :<< e' :| f' :| a :^ c'
                       :<<. d' :<<< b

ep1Lh1 = pad2 (a__ sn :|: e_ sn :|: a_ sn)
    :|: pad (c sn :-: bf_ sn :|: c sn :-: a_ sn)
     :|: (c sn :-: bf_ sn :-: g_ sn)

ep1Lh2 = play $ melody :<< f_ :| a_ :| c :| a_ :| c :| a_
                       :| f_ :| bf_ :| d :| bf_ :| d :| bf_ :| f_ :| e

ep1Lh3 = bf_ sn :-: g_ sn :-: f_ sn :|: pad2 (e sn)
     :|: bf_ sn :-: g_ sn :-: f_ sn :|: pad2 (e sn)

ep1Lh4 = pad (play $ melody :<< f_ :| a_ :| c :| a_ :| c :| a_
                            :<< f_ :| a_ :| c :| a_ :| c :| a_
                            :<< e_ :| a_ :| c :| a_)
     :|: d_ oct sc
     :|: pad (play $ melody :<< f_ :| g_ :| e :| g_ :| e :| g_ :| f)

ep1part sname = score section sname
                      setTempo 90
                      setKeySig c_maj
                      setRuleSet free
                      withMusic

ep1p1 = ep1part "1st episode, part 1" (hom ep1Rh1 ep1Lh1)

ep1p2 = ep1part "1st episode, part 2" (hom ep1Rh2 ep1Lh2)

ep1p3 = ep1part "1st episode, part 3" (hom ep1Rh3 ep1Lh3)

ep1p4 = score section "1st episode, part 4"
              setTempo 90
              setRuleSet free
              setKeySig c_maj
              withMusic (hom ep1Rh4 ep1Lh4)

episode1 = ep1p1 ++ ep1p2 ++ ep1p3 ++ ep1p4
