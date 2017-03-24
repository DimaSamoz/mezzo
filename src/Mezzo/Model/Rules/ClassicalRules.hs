
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Rules.ClassicalRules
-- Description :  MIDI exporting
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and constraints encoding the rules of classical music.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Rules.ClassicalRules
    ( ValidMelConcat
    , ValidHarmConcat
    , Align
    ) where

import Mezzo.Model.Types
import Mezzo.Model.Prim
import Mezzo.Model.Harmony
import Mezzo.Model.Errors

import GHC.TypeLits
import Data.Kind

-------------------------------------------------------------------------------
-- Melodic constraints
-------------------------------------------------------------------------------

-- | Ensures that melodic intervals are valid.
--
-- A melodic interval is invalid if it is
--
--  * any augmented interval or
--  * any diminished interval or
--  * any seventh interval.
class ValidMelInterval (e :: PitchPair) (i :: IntervalType)
instance {-# OVERLAPPING #-}       ValidMelInterval e (Interval Aug Unison)
instance {-# OVERLAPS #-} PitchPairError "Augmented melodic intervals are not permitted: " e
                                => ValidMelInterval e (Interval Aug a)
instance {-# OVERLAPS #-} PitchPairError "Diminished melodic intervals are not permitted: " e
                                => ValidMelInterval e (Interval Dim a)
instance {-# OVERLAPPING #-} PitchPairError "Seventh intervals are not permitted in melody: " e
                                => ValidMelInterval e (Interval a Seventh)
instance {-# OVERLAPPABLE #-}      ValidMelInterval e i

-- | Ensures that two pitches form valid melodic leaps.
--
-- Two pitches form valid melodic leaps if
--
--  * at least one of them is silent (i.e. it is a rest) or
--  * they form a valid melodic interval.
class ValidMelLeap (p1 :: PitchType) (p2 :: PitchType)
instance {-# OVERLAPPING #-}  ValidMelLeap Silence Silence
instance {-# OVERLAPPING #-}  ValidMelLeap Silence (Pitch pc acc oct)
instance {-# OVERLAPPING #-}  ValidMelLeap (Pitch pc acc oct) Silence
instance {-# OVERLAPPABLE #-} ValidMelInterval '(a, b) (MakeInterval a b) => ValidMelLeap a b

-- | Ensures that two voices can be appended.
--
-- Two voices can be appended if
--
--  * at least one of them is empty or
--  * the last pitch of the first vector forms a valid melodic leap
--    with the first pitch of the second vector.
class ValidMelAppend (a :: Voice l1) (b :: Voice l2)
instance {-# OVERLAPPING #-}  ValidMelAppend End a
instance {-# OVERLAPPING #-}  ValidMelAppend a End
instance {-# OVERLAPPABLE #-} ValidMelLeap (Last vs1) (Head vs2) => ValidMelAppend vs1 vs2

-- | Ensures that two partitures can be horizontally concatenated.
--
-- Two part lists can be horizontally concatenated if
--
--  * both of them are empty or
--  * all of the voices can be appended.
class ValidMelConcat (ps1 :: Partiture n l1) (ps2 :: Partiture n l2)
instance {-# OVERLAPPING #-}       ValidMelConcat None None
instance {-# OVERLAPPABLE #-} (ValidMelAppend v1 v2, ValidMelConcat vs1 vs2)
                                => ValidMelConcat (v1 :-- vs1) (v2 :-- vs2)


-------------------------------------------------------------------------------
-- Harmonic constraints
-------------------------------------------------------------------------------

-- | Ensures that harmonic intervals are valid.
--
-- A harmonic interval is invalid if it is
--
--  * a minor second or
--  * a major seventh or
--  * an augmented octave.
class ValidHarmInterval (e :: PitchPair) (i :: IntervalType)
instance {-# OVERLAPPING #-} PitchPairError "Can't have minor seconds in chords: " e
                                => ValidHarmInterval e (Interval Aug Unison)
instance {-# OVERLAPPING #-} PitchPairError "Can't have minor seconds in chords: " e
                                => ValidHarmInterval e (Interval Min Second)
instance {-# OVERLAPPING #-} PitchPairError "Can't have major sevenths in chords: " e
                                => ValidHarmInterval e (Interval Maj Seventh)
instance {-# OVERLAPPING #-} PitchPairError "Can't have major sevenths in chords: " e
                                => ValidHarmInterval e (Interval Dim Octave)
instance {-# OVERLAPPING #-} PitchPairError "Can't have augmented octaves in chords: " e
                                => ValidHarmInterval e (Interval Aug Octave)
instance {-# OVERLAPPABLE #-}      ValidHarmInterval e i

-- | Ensures that two pitches form valid harmonic dyad (interval).
--
-- Two pitches form valid harmonic dyads if
--
--  * at least one of them is silent (i.e. it is a rest) or
--  * they form a valid harmonic interval.
class ValidHarmDyad (p1 :: PitchType) (p2 :: PitchType)
instance {-# OVERLAPPING #-}  ValidHarmDyad Silence Silence
instance {-# OVERLAPPING #-}  ValidHarmDyad (Pitch pc acc oct) Silence
instance {-# OVERLAPPING #-}  ValidHarmDyad Silence (Pitch pc acc oct)
instance {-# OVERLAPPABLE #-} ValidHarmInterval '(a, b) (MakeInterval a b) => ValidHarmDyad a b

-- | Ensures that two voices form pairwise valid harmonic dyads.
class ValidHarmDyadsInVectors (v1 :: Voice l) (v2 :: Voice l)
instance AllPairsSatisfy ValidHarmDyad v1 v2 => ValidHarmDyadsInVectors v1 v2

-- | Ensures that two partitures can be vertically concatenated.
--
-- Two partitures can be vertically concatenated if
--
--  * the top one is empty or
--  * all but the first voice can be concatenated, and the first voice
--    forms valid harmonic dyads with every other voice and follows the rules
--    of valid harmonic motion.
class ValidHarmConcat (ps :: (Partiture n1 l, Partiture n2 l))
instance {-# OVERLAPPING #-}       ValidHarmConcat '(None, vs)
instance {-# OVERLAPPABLE #-} ( ValidHarmConcat '(vs, us)
                              , AllSatisfyAll [ ValidHarmDyadsInVectors v
                                              , ValidHarmMotionInVectors v] us)
                                => ValidHarmConcat '((v :-- vs), us)


-------------------------------------------------------------------------------
-- Voice leading constraints
-------------------------------------------------------------------------------

-- | Ensures that four pitches (representing two consequent intervals) follow
-- the rules for valid harmonic motion.
--
-- Harmonic motion is not permitted if
--
--  * it is direct motion into a perfect interval (this covers parallel and
--    concealed fifths, octaves and unisons).
type family ValidMotion (p1 :: PitchType) (p2 :: PitchType)
                        (q1 :: PitchType) (q2 :: PitchType)
                            :: Constraint where
    ValidMotion Silence _ _ _ = Valid
    ValidMotion _ Silence _ _ = Valid
    ValidMotion _ _ Silence _ = Valid
    ValidMotion _ _ _ Silence = Valid
    ValidMotion p1 p2 q1 q2   =
            If ((p1 .~. q1) .||. (p2 .~. q2))
                (ObliqueMotion (MakeInterval p1 p2) (MakeInterval q1 q2))
                (If (p1 <<? q1)
                    (If (p2 <<? q2)
                        (DirectMotion (DyPair p1 p2 q1 q2) (MakeInterval p1 p2) (MakeInterval q1 q2))
                        (ContraryMotion (MakeInterval p1 p2) (MakeInterval q1 q2)))
                    (If (p2 <<? q2)
                        (ContraryMotion (MakeInterval p1 p2) (MakeInterval q1 q2))
                        (DirectMotion (DyPair p1 p2 q1 q2) (MakeInterval p1 p2) (MakeInterval q1 q2))))

-- | Ensures that the interval formed by the first pitch and the last element
-- of the first voice can move to the interval formed by the second
-- pitch and the first element of the second voice.
class ValidMelPitchVectorMotion (p1 :: PitchType) (p2 :: PitchType) (v1 :: Voice l1) (v2 :: Voice l2)
instance {-# OVERLAPPING #-}    ValidMelPitchVectorMotion p1 p2 End End
instance {-# OVERLAPPABLE #-} ValidMotion p1 (Last v1) p2 (Head v2)
                            =>  ValidMelPitchVectorMotion p1 p2 v1 v2
-- Can't have v1 be End and v2 be not End, since if v1 under p1 is not nil, there
-- must be an accompanying voice under p2

-- | Ensures that two partitures follow the rules of motion when
-- horizontally concatenated.
--
-- Two horizontally concatenated partitures follow the rules of harmonic motion if
--
--  * both are empty or
--  * their lower voices can be concatenated and the joining elements of the
--    top voice form intervals with the joining elements of the other voices
--    which follow the rules of harmonic motion.
class ValidMelMatrixMotion (ps1 :: Partiture n l1) (ps2 :: Partiture n l2)
instance {-# OVERLAPPING #-}       ValidMelMatrixMotion None None
instance {-# OVERLAPPABLE #-} ( ValidMelMatrixMotion vs1 vs2
                              , AllPairsSatisfy' (ValidMelPitchVectorMotion (Last v1) (Head v2)) vs1 vs2)
                                => ValidMelMatrixMotion (v1 :-- vs1) (v2 :-- vs2)

-- | Ensures that two voices form pairwise intervals which follow the
-- rules of harmonic motion.
class ValidHarmMotionInVectors (v1 :: Voice l) (v2 :: Voice p)
instance {-# OVERLAPPING #-}       ValidHarmMotionInVectors End End
instance {-# OVERLAPPING #-} ValidHarmMotionInVectors (p :* d1 :- End) (q :* d2 :- End)
instance {-# OVERLAPPABLE #-} ( ValidMotion p q (Head ps) (Head qs)
                              , ValidHarmMotionInVectors ps qs)
                                => ValidHarmMotionInVectors (p :* d1 :- ps) (q :* d2 :- qs)
