{-# LANGUAGE TypeInType #-}

-- Testing of the composition functions.
-- These examples should compile correctly. The values aren't actually tested here -
-- the example compositions provide fairly good evidence that the literals work correctly.

module CompTestsCompile where

import Mezzo

-- Testing function that builds a score with the strictest rule set.
test = score setRuleSet strict withMusic

---- Notes & rests
scC = test $ c qn
scFf = test $ ff en
scGs'' = test $ gs'' sn
scBff_2 = test $ bf flat wn'
scAsfsf'4 = test $ as'4 flat sharp flat hn
scMin = test $ c_5 qn
scMax = test $ b_4 qn
scR1 = test $ r qr
scR2 = test $ r sr'

notes = renderSections
        "demos/noteTests.mid"
        "Note and rest tests" $
        map (section "")[scC, scFf, scGs'', scBff_2, scAsfsf'4, scMin, scMax, scR1, scR2]

---- Chords
scCmaj3 = test $ c maj3 qc
scDfmin3D = test $ df min3D ec
scEs'fourth = test $ e' sharp fourth sc
scFsf__fifthDI = test $ fs__ flat fifthD inv sc'
scGs'3octII = test $ gs'3 oct inv inv wc
scAfmaji2 = test $ af maj' i2 hc
scBsaugD = test $ b sharp augD qc'
scCf_dim = test $ cf_ dim ec
scGf'minD = test $ gf' minD ec'
scFf_3min7i3 = test $ ff_3 min7' i3 qc
scAsfsdom7 = test $ as_ flat sharp dom7 ec
scDssdim7inv = test $ ds sharp dim7 inv hc
scEs'hdim7i2 = test $ es' hdim7' i2 wc'

chords = renderSections
        "demos/chords.mid"
        "Chord tests" $
        map (section "")
            [ scCmaj3, scDfmin3D, scEs'fourth, scFsf__fifthDI
            , scGs'3octII, scAfmaji2, scBsaugD, scCf_dim, scGf'minD
            , scFf_3min7i3, scAsfsdom7, scDssdim7inv, scEs'hdim7i2]
