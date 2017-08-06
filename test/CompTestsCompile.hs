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

---- Progressions
scMajSubdomII = test $ prog $ cadence (full subdom_ii auth_V)
scMajSubdomIII_IV = test $ prog $ cadence (full subdom_iii_IV auth_V)


---- Melodic intervals
scMCF = test $ c qn :|: f en'
scMBfD' = test $ bf hn :|: d' qn
scMAAs = test $ a_ sn :|: as_ qn'
scMEE = test $ e'' en :|: e'' en
scMF__Ds'' = test $ f__ tn :|: d'' wn
scMel = test $ start $ melody :< c :| d :| e :| f :| g :| a :| b :| c' :| bf
                             :| af :| fs sharp :| e :| cs :| c :| b_ :| fs :| a
                             :| d' :| e' :| g' :| c'' :| cs'' :| cs'' sharp

---- Harmonic intervals

scHCF = test $ f en :-: c en
scHBG = test $ b__ en :-: g__ en
scHFfEss = test $ ff qn' :-: es sharp qn'
scHAGf = test $ gf sn :-: a sn
scHGsE = test $ gs' qn :-: e qn

---- Motion
scMoCCFG = test $ f qn :-: c qn :|: g qn :-: c qn
scMoCCFG' = test $ (f qn :|: g qn) :-: (c qn :|: c qn)
scMoCDCB = test $ c' en :-: c en :|: b qn :-: d qn
scMoCDCB' = test $ (c' en :|: b qn) :-: (c en :|: d qn)
scMoEfEFFs = test $ f hn :-: ef hn :|: fs en :-: e en
scMoEfEFFs' = test $ (f hn :|: fs en) :-: (ef hn :|: e en)

allcomps = renderSections
        "demos/testing.mid"
        "All tests" $
        map (section "")
            [ scC, scFf, scGs'', scBff_2, scAsfsf'4, scMin, scMax, scR1, scR2
            , scCmaj3, scDfmin3D, scEs'fourth, scFsf__fifthDI
            , scGs'3octII, scAfmaji2, scBsaugD, scCf_dim, scGf'minD
            , scFf_3min7i3, scAsfsdom7, scDssdim7inv, scEs'hdim7i2
            , scMajSubdomII, scMajSubdomIII_IV
            , scMCF, scMBfD', scMAAs, scMEE, scMF__Ds'', scMel
            , scHCF, scHBG, scHFfEss, scHAGf, scHGsE
            , scMoCCFG, scMoCCFG', scMoCDCB, scMoCDCB', scMoEfEFFs, scMoEfEFFs'
            ]
