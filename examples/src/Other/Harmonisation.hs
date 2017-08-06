
-- From http://decipheringmusictheory.com/?page_id=46

module Other.Harmonisation where

import Mezzo

v1 = start $ melody :| d   :| g  :| fs :< g    :| a   :^ bf  :| a   :| a   :| a   :| d'   :| c' :| bf :| a :>> g

v2 = start $ melody :| d   :| ef :| d  :| d           :| d   :| d   :| cs  :| d   :| d    :| ef :| d  :| d :>> bf_

v3 = start $ melody :| bf_ :| g_ :| a_ :< g_   :| fs_ :^ g_  :| a_  :| a_  :| fs_ :| g_   :| g_ :| g_ :| fs_ :>> g_

v4 = start $ melody :| g__ :| c_ :| c_ :< bf__ :| a__ :^ g__ :| f__ :| a__ :| d__ :| bf__ :| c_ :| d_ :| d_  :>> g__
--                                                                           ^ The above tutorial used 'd_' which gave a concealed octave

sco = score setKeySig g_min
            setRuleSet strict
            withMusic (v1 :-: v2 :-: v3 :-: v4)

main = renderScore "rendered/Harmonisation.mid"
                   "4-voice chorale"
                   sco
