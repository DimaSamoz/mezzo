-- {-# OPTIONS_GHC -fdefer-type-errors #-}
-- {-# LANGUAGE TypeInType #-}

-- Testing of the composition functions.
-- These examples should all fail. The name of the testing value should give an
-- indication of the nature of the error.

module CompTestsFail where

import Mezzo
-- import Control.Exception

-- Testing function that builds a score with the strictest rule set.
test s = score setRuleSet strict withMusic

{-

---- Notes & rests
sfCf_5 = test "too low" $ cf_5 qn
-- Can't create custom errors for octaves lower than 0, as those are not natural numbers.
sfBs'4 = test "too high" $ bs'4 qn
sfBsss'4 = test "invalid octave" $ bs'4 sharp sharp qn

---- Chords
sfCmaj7 = test "major seventh" $ c maj7 qc
-- Nested TypeError not encountered automatically
sfG'4maj = test "invalid octave chord" `seq` b'4 maj qc
sfGmin3Di3iii = test "invalid octave after inversions" `seq` g'3 min3D' i3 inv inv inv qc

---- Progressions
sfSubdomii = const "no ii subdominant in minor key"
                    (score setKeySig a_min withMusic (prog $ cadence (full subdom_ii auth_V)))
sfSubdomiii = const "no iii-IV subdominant in minor key"
                    (score setKeySig a_min withMusic (prog $ cadence (full subdom_iii_IV auth_V)))
-- -}

---- Melodic composition
sfCFf = test  "diminished melodic C-Ff" $ c qn :|: ff qn
sfGf = test  "diminished melodic G-Gf" $ g qn :|: gf' qn
sfEAs = test  "augmented melodic E-As" $ e qn :|: a sharp qn
sfCCs = test  "augmented melodic C-Cs" $ c qn :|: cs' qn
sfCB = test "major seventh C-B" $ c qn :|: b qn
sfCBf = test "minor seventh C-B" $ c qn :|: bf qn
sfFB = test "tritone" $ f qn :|: b qn
sfMelody = test "melody" $ play $ melody :| c :| gs :| df' :| b :| c
