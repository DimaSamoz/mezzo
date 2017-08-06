
module FurElise.Theme (theme) where

import Mezzo


-------------------------------------------------------------------------------
-- Für Elise theme
-------------------------------------------------------------------------------

-- Refrain

refrCoreRh = start $ melody :<< e' :| ds' :| e' :| ds' :| e'
                           :| b :| d' :| c' :^ a :~< r :< b :~<< r

refrCoreLh = start $ melody :~> r
            :<< a__ :| e_ :| a_ :| c :| e :| a
            :<< e__ :| e_ :| gs_ :~<. r

refrRh = refrCoreRh :|: start (melody :<< e :| gs :| b :< c' :~<< r :<< e)
     :|: refrCoreRh :|: start (melody :<< e :| c' :| b)

refrLh = refrCoreLh :|: (a__ sn :|: e_ sn :|: a_ sn :|: r sr)
     :|: refrCoreLh

refrBh = hom refrRh refrLh
     :|: hom (a qn) (a__ sn :|: e_ sn :|: a_ sn :|: r sr)
     :|: hom refrRh refrLh

refrain = score section "refrain"
                setTempo 90
                setKeySig a_min
                setRuleSet free
                withMusic refrBh

-- Variation

varRh = start $ melody
    :< a :~<< r
    :<< b :| c' :| d' :<. e'
    :<< g :| f' :| e' :<. d'
    :<< f :| e' :| d' :<. c'
    :<< e :| d' :| c' :< b
    :<< e :| e :| e' :| e :| e' :| e' :| e''
    :<< ds' :| e' :| ds' :| e' :| ds' :| e' :| ds'

varLh = start $ melody
    :<< a__ :| e_ :| a_ :~<. r
    :<< c_ :| g_ :| c :~<. r
    :<< g__ :| g_ :| b_ :~<. r
    :<< a__ :| e_ :| a_ :~<. r
    :<< e__ :| e_ :~>. r :~< r

variation = score section "variation"
                  setTempo 90
                  setKeySig a_min
                  setRuleSet free
                  withMusic (hom varRh varLh)

theme = refrain ++ variation ++ refrain
