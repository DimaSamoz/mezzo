{-# LANGUAGE NoImplicitPrelude #-}

module FurElise.Episode4 (episode4) where

import Mezzo

-------------------------------------------------------------------------------
-- Fourth episode
-------------------------------------------------------------------------------

ep4Rh1 = tripletE a_ c e :|: tripletE a c' e' :|: tripletE d' c' b
     :|: tripletE a c' e' :|: tripletE a' c'' e'' :|: tripletE d'' c'' b'
     :|: tripletE a' c'' e'' :|: tripletE a'' c'3 e'3 :|: tripletE d'3 c'3 b''
     :|: tripletE bf'' a'' gs''

ep4Lh1 = pad2 (a_3 en)
    :|: pad2 (r er) :|: a_ min ec :|: a_ min ec
    :|: pad2 (r er) :|: a_ min ec :|: a_ min ec
    :|: pad2 (r er) :|: a_ min ec :|: a_ min ec

ep4Rh2 = tripletE g'' fs'' f'' :|: tripletE e'' ds'' d''
     :|: tripletE cs'' c'' b' :|: tripletE bf' a' gs' :|: tripletE g' fs' f'

ep4p1 = section "4th episode, part 1" $
            score setTempo 90
                  setKeySig a_min
                  setRuleSet free
                  withMusic (hom ep4Rh1 ep4Lh1)

ep4p2 = section "4th episode, part 2" $
            score setTempo 90
                  setKeySig a_min
                  withMusic ep4Rh2

episode4 = ep4p1 ++ ep4p2
