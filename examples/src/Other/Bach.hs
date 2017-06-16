{-# LANGUAGE GADTs, FlexibleContexts #-}

module Other.Bach where

import Mezzo

-----------------------------------------------------------------------
-- Bar repetitions
-----------------------------------------------------------------------

-- One bar in the top voice
v1b p1 p2 p3 = r er :|: notes :|: notes :|: r er :|: notes :|: notes
    where notes = p1 sn :|: p2 sn :|: p3 sn

-- One bar in the middle voice
v2b p = r sr :|: p qn' :|: r er :|: p qn' :|: r sr

-- One bar in the bottom voice
v3b p = p hn :|: p hn

-- Render out a single bar given five pitches
bar p1 p2 p3 p4 p5 = score section "bar"
                           setTempo 100
                           setKeySig c_maj
                           setRuleSet free
                           withMusic music
    where music = v1b p3 p4 p5 `hom` v2b p2 `hom` v3b p1

bars =
    [ bar c e g c' e',    bar c d a d' f'
    , bar b_ d g d' f',   bar c e g c' e',     bar c e a e' a'
    , bar c d fs a d',    bar b_ d g d' g',    bar b_ c e g c'
    , bar a_ c e g c',    bar d_ a_ d fs c',   bar g_ b_ d g b
    , bar g_ bf_ e g cs', bar f_ a_ d a d',    bar f_ af_ d f b
    , bar e_ g_ c g c',   bar e_ f_ a_ c f,    bar d_ f_ a_ c f
    , bar g__ d_ g_ b_ f, bar c_ e_ g_ c e,    bar c_ g_ bf_ c e
    , bar f__ f_ a_ c e,  bar fs__ c_ a_ c ef, bar af__ f_ b_ c d
    , bar g__ f_ g_ b_ d, bar g__ e_ g_ c e,   bar g__ d_ g_ c f
    , bar g__ d_ g_ b_ f, bar g__ ef_ a_ c fs, bar g__ e_ g_ c g
    , bar g__ d_ g_ c f,  bar g__ d_ g_ b_ f,  bar c__ c_ g_ bf_ e
    ]

-----------------------------------------------------------------------
-- Conclusion and end chord
-----------------------------------------------------------------------

concV1 = play $ melody :~< r
            :<< f_ :| a_ :| c :| f :| c :| a_ :| c
            :| a_ :| f_ :| a_ :| f_ :| d_ :| f_ :| d_ :~< r
            :<< g :| b :| d' :| f' :| d' :| b :| d'
            :| b :| g :| b :| d :| f :| e :| d

concV2 = r sr :|: c_ wn :|: b_ hn' :|: r er'

concV3 = c__ wn :|: c__ wn

conc = score section "conclusion"
             setTempo 100
             setKeySig c_maj
             withMusic (concV1 `hom` concV2 `hom` concV3)

endChord = score section "end chord"
                 setTempo 100
                 setKeySig c_maj
                 withMusic (c maj inv wc `hom` c__ oct wc)

main = renderScores "rendered/Bach.mid"
        "Johann Sebastian Bach - Prelude in C Major" $
        bars ++ [conc, endChord]
