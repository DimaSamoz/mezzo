{-# LANGUAGE NoImplicitPrelude #-}

import Mezzo

feTempo = 90

-------------------------------------------------------------------------------
-- Theme
-------------------------------------------------------------------------------

thRhCo = play $ melody :<< e' :| ds' :| e' :| ds' :| e'
                       :| b :| d' :| c' :^ a :~< r :< b :~<< r

thLhCo = play $ melody :~> r
            :<< a__ :| e_ :| a_ :| c :| e :| a
            :<< e__ :| e_ :| gs_ :~<. r
            :<< a__ :| e_ :| a_

themeRh = thRhCo :|: play (melody :<< e :| gs :| b :< c' :~<< r :<< e)
      :|: thRhCo :|: play (melody :<< e :| c' :| b :< a :~<< r)

themeLh = thLhCo :|: r sr :|: thLhCo

themeBh = hom themeRh themeLh
    :|: pad (r sr)
    :|: hom themeRh themeLh

theme = section "theme" $
            score setTempo feTempo
                setKeySig a_min
                withMusic themeBh

-------------------------------------------------------------------------------
-- First episode
-------------------------------------------------------------------------------

ep1Rh = play $ melody
    :<< b :| c' :| d' :<. e'
    :<< g :| f' :| e' :<. d'
    :<< f :| e' :| d' :<. c'
    :<< e :| d' :| c' :< b
    :<< e :| e :| e' :| e :| e' :| e' :| e''
    :<< ds' :| e' :| ds' :| e' :| ds' :| e' :| ds'

ep1Lh = play $ melody :~<. r
    :<< c_ :| g_ :| c :~<. r
    :<< g__ :| g_ :| b_ :~<. r
    :<< a__ :| e_ :| a_ :~<. r
    :<< e__ :| e_ :~>. r :~< r

episode1 = section "1st episode" $
            score setTempo feTempo
                setKeySig a_min
                setRuleSet free
                withMusic (hom ep1Rh ep1Lh)

-------------------------------------------------------------------------------
-- Second episode
-------------------------------------------------------------------------------

ep2Rh1 = pad (c maj3 inv sc :|: c fourth inv sc) :|: c maj inv sc

ep2Rh2 = play $ melody :| c' :<<. f' :<<< e' :< e' :| d'
                       :<<. bf' :<<< a' :<< a' :| g'

ep2Rh3 = play $ melody :<< f' :| e' :| d' :| c'

ep2Rh4 = play $ melody :< bf :| a :<<< a :| g :| a :| bf :^ c'
                       :<< d' :| ds' :<. e' :<< e' :| f' :| a :^ c'
                       :<<. d' :<<< b

ep2Lh1 = pad (c sn :-: bf_ sn :|: c sn :-: a_ sn)
     :|: (c sn :-: bf_ sn :-: g_ sn)

ep2Lh2 = play $ melody :<< f_ :| a_ :| c :| a_ :| c :| a_
                       :| f_ :| bf_ :| d :| bf_ :| d :| bf_ :| f_ :| e

ep2Lh3 = bf_ sn :-: g_ sn :-: f_ sn :|: pad2 (e sn)
     :|: bf_ sn :-: g_ sn :-: f_ sn :|: pad2 (e sn)

ep2Lh4 = pad (play $ melody :<< f_ :| a_ :| c :| a_ :| c :| a_
                            :<< f_ :| a_ :| c :| a_ :| c :| a_
                            :<< e_ :| a_ :| c :| a_)
     :|: d_ oct sc
     :|: pad (play $ melody :<< f_ :| g_ :| e :| g_ :| e :| g_ :| f)

epPart k = score setTempo 90
            setKeySig k
            withMusic

ep2p1 = section "2nd episode, part 1" $
            epPart c_maj (hom ep2Rh1 ep2Lh1)

ep2p2 = section "2nd episode, part 2" $
            epPart c_maj (hom ep2Rh2 ep2Lh2)

ep2p3 = section "2nd episode, part 3" $
            epPart c_maj (hom ep2Rh3 ep2Lh3)

ep2p4 = section "2nd episode, part 4" $
            epPart c_maj (hom ep2Rh4 ep2Lh4)

episode2 = ep2p1 ++ ep2p2 ++ ep2p3 ++ ep2p4

-------------------------------------------------------------------------------
-- Third episode
-------------------------------------------------------------------------------

ep3Rh1 = play $ melody
            :<<< c' :| g' :| g :| g' :| a :| g' :| b :| g' :| c' :| g' :| d' :| g'
            :| e' :| g' :| c'' :| b' :| a' :| g' :| f' :| e' :| d' :| g' :| f' :| d'

ep3Lh1 = pad (c maj3 ec :|: pad (r sr)
     :|:      g sn :-: f sn
     :|:      e min3 sc)
     :|:      g sn :-: f sn :-: d sn
     :|:      c maj ec
     :|: pad (f_ maj3 ec
     :|:      g_ maj3 ec)

ep3Rh2 = play $ melody
            :<<< e' :| f' :| e :| ds' :| e' :| b :| e' :| ds' :| e' :| b :| e' :| ds'
            :<. e' :<< b :| e' :| ds' :<. e' :<< b :| e' :| ds' :| e' :| ds' :| e' :| ds'

ep3Lh2 = gs_ min3 ec :|: pad (r qr :|: r wr)

ep3p1 = section "3rd episode, part 1" $
            epPart c_maj (hom ep3Rh1 ep3Lh1)

ep3p2 = section "3rd episode, part 2" $
            epPart c_maj (hom ep3Rh2 ep3Lh2)

episode3 = ep3p1 ++ ep3p1 ++ ep3p2

-------------------------------------------------------------------------------
-- Fourth episode
-------------------------------------------------------------------------------


thLhCoShort = play $ melody :~> r
            :<< a__ :| e_ :| a_ :| c :| e :| a
            :<< e__ :| e_ :| gs_ :~<. r

themeRhShort = thRhCo :|: play (melody :<< e :| gs :| b :< c' :~<< r :<< e)
           :|: thRhCo :|: play (melody :<< e :| c' :| b)

themeLhShort = thLhCo :|: r sr :|: thLhCoShort

themeBhShort = hom themeRh themeLh
    :|: pad (r sr)
    :|: hom themeRhShort themeLhShort

themeShort = section "theme (short)" $
            epPart a_min themeBhShort

ep4start = hom (a en :|: r sr) (a__ sn :|: a__ sn :|: a__ sn)

theme' = themeShort ++ section ""  (epPart a_min ep4start)

ep4Rh1 = pad3 (r sr :|: r er)
     :|:       cs dim7 inv qc'
     :|: pad  (d min inv qc)
     :|: pad2 (cs' min3 sc
     :|:       d' min3 sc)
     :|: pad  (d dim' i2 qc
     :|:       d dim' i2 ec
     :|:       a min qc')

ep4Lh1 = (a__ sn :|: a__ sn :|: a__ sn) :|: notes :|: notes :|: notes :|: notes
    where notes = play $ melody :<< a__ :| a__ :| a__ :| a__ :| a__ :| a__

ep4Rh2 = pad2 (d min3 inv qc
     :|:       c maj3 inv sc
     :|:       b_ min3 inv sc)
     :|: pad  (fs_ dim' i2 qc)
     :|: pad2 (a_ min3 inv ec
     :|:       a_ min3 inv ec
     :|:       c maj3 inv ec
     :|:       b_ min3 inv ec
     :|:       a_ min3 inv qc')

ep4Lh2 = d__ fifth sc :|: d__ fifth sc :|: d__ fifth sc
     :|: d__ fifth sc :|: d__ fifth sc :|: d__ fifth sc
     :|: ds__ sn :-: a__ sn :|: ds__ sn :-: a__ sn :|: ds__ sn :-: a__ sn
     :|: ds__ sn :-: a__ sn :|: ds__ sn :-: a__ sn :|: ds__ sn :-: a__ sn
     :|: e__ fourth sc :|: e__ fourth sc :|: e__ fourth sc
     :|: e__ fourth sc :|: e__ maj3 sc :|: e__ maj3 sc
     :|: a_3 oct sc :|: pad (play $ melody :<< a__ :| a__ :| a__ :| a__ :| a__)

ep4Rh3 =       cs dim7 inv qc'
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

ep4Lh3 = aNotes :|: aNotes :|: aNotes :|: bfNotes :|: bfNotes :|: bfNotes :|: bNotes
     :|: c_ qn :|: r er :|: e_ en :|: r qr
    where aNotes = play $ melody :<< a__ :| a__ :| a__ :| a__ :| a__ :| a__
          bfNotes = play $ melody :<< bf__ :| bf__ :| bf__ :| bf__ :| bf__ :| bf__
          bNotes = play $ melody :<< b__ :| b__ :| b__ :| b__ :| b__ :| b__

ep4p1 = section "4th episode, part 1" $
            epPart d_min (hom ep4Rh1 ep4Lh1)

ep4p2 = section "4th episode, part 2" $
            epPart d_min (hom ep4Rh2 ep4Lh2)

ep4p3 = section "4th episode, part 3" $
            epPart d_min (hom ep4Rh3 ep4Lh3)

episode4 = ep4p1 ++ ep4p2 ++ ep4p3

-------------------------------------------------------------------------------
-- Fifth episode
-------------------------------------------------------------------------------

ep5Rh1 = tripletE a_ c e :|: tripletE a c' e' :|: tripletE d' c' b
     :|: tripletE a c' e' :|: tripletE a' c'' e'' :|: tripletE d'' c'' b'
     :|: tripletE a' c'' e'' :|: tripletE a'' c'3 e'3 :|: tripletE d'3 c'3 b''
     :|: tripletE bf'' a'' gs''

ep5Lh1 = pad2 (a_3 en)
    :|: pad2 (r er) :|: a_ min ec :|: a_ min ec
    :|: pad2 (r er) :|: a_ min ec :|: a_ min ec
    :|: pad2 (r er) :|: a_ min ec :|: a_ min ec

ep5Rh2 = tripletE g'' fs'' f'' :|: tripletE e'' ds'' d''
     :|: tripletE cs'' c'' b' :|: tripletE bf' a' gs' :|: tripletE g' fs' f'

ep5p1 = section "5th episode, part 1" $
            score setTempo feTempo
                setKeySig a_min
                setRuleSet free
                withMusic (hom ep5Rh1 ep5Lh1)

ep5p2 = section "5th episode, part 2" $
            epPart d_min ep5Rh2

episode5 = ep5p1 ++ ep5p2

-------------------------------------------------------------------------------
-- End theme
-------------------------------------------------------------------------------

endChord = hom (a en :|: r er) (a_3 oct ec :|: pad (r er))

endTheme = themeShort ++ section "end chord" (epPart a_min endChord)


comp = renderSections "examples/FurElise.mid"
        "Ludwig van Beethoven - Fur Elise"
        [ theme, episode1, theme
        , episode2, episode3
        , theme, episode1, theme'
        , episode4, episode5
        , theme, episode1, endTheme]
