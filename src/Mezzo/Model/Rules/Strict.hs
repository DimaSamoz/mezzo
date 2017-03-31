
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Rules.Strict
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

module Mezzo.Model.Rules.Strict
    ( ValidMelConcatStrict
    , ValidMelIntervalStrict
    , ValidHarmConcat
    , ValidHomConcat
    , ValidMelMatrixMotion
    , ValidChordType
    ) where

import Mezzo.Model.Types
import Mezzo.Model.Prim
import Mezzo.Model.Harmony
import Mezzo.Model.Errors
import Mezzo.Model.Rules.Classical

import GHC.TypeLits
import Data.Kind


-- | Ensures that melodic intervals are valid.
--
-- A melodic interval is invalid if it is
--
--  * any augmented interval or
--  * any diminished interval or
--  * any seventh interval.
class ValidMelIntervalStrict (e :: PitchPair) (i :: IntervalType)
instance {-# OVERLAPPING #-}       ValidMelIntervalStrict e (Interval Aug Unison)
instance {-# OVERLAPS #-} PitchPairError "Augmented melodic intervals are not permitted: " e
                                => ValidMelIntervalStrict e (Interval Aug a)
instance {-# OVERLAPS #-} PitchPairError "Diminished melodic intervals are not permitted: " e
                                => ValidMelIntervalStrict e (Interval Dim a)
instance {-# OVERLAPPING #-} PitchPairError "Seventh intervals are not permitted in melody: " e
                                => ValidMelIntervalStrict e (Interval a Seventh)
instance {-# OVERLAPPABLE #-}      ValidMelIntervalStrict e i



-- | Ensures that two pitches form valid melodic leaps.
--
-- Two pitches form valid melodic leaps if
--
--  * at least one of them is silent (i.e. it is a rest) or
--  * they form a valid melodic interval.
class ValidMelLeapStrict (p1 :: PitchType) (p2 :: PitchType)
instance {-# OVERLAPPING #-}  ValidMelLeapStrict Silence Silence
instance {-# OVERLAPPING #-}  ValidMelLeapStrict Silence (Pitch pc acc oct)
instance {-# OVERLAPPING #-}  ValidMelLeapStrict (Pitch pc acc oct) Silence
instance {-# OVERLAPPABLE #-} ValidMelIntervalStrict '(a, b) (MakeInterval a b) => ValidMelLeapStrict a b

-- | Ensures that two voices can be appended.
--
-- Two voices can be appended if
--
--  * at least one of them is empty or
--  * the last pitch of the first vector forms a valid melodic leap
--    with the first pitch of the second vector.
class ValidMelAppendStrict (a :: Voice l1) (b :: Voice l2)
instance {-# OVERLAPPING #-}  ValidMelAppendStrict End a
instance {-# OVERLAPPING #-}  ValidMelAppendStrict a End
instance {-# OVERLAPPABLE #-} ValidMelLeapStrict (Last vs1) (Head vs2) => ValidMelAppendStrict vs1 vs2

-- | Ensures that two partitures can be horizontally concatenated.
--
-- Two part lists can be horizontally concatenated if
--
--  * both of them are empty or
--  * all of the voices can be appended.
class ValidMelConcatStrict (ps1 :: Partiture n l1) (ps2 :: Partiture n l2)
instance {-# OVERLAPPING #-}       ValidMelConcatStrict None None
instance {-# OVERLAPPABLE #-} (ValidMelAppendStrict v1 v2, ValidMelConcatStrict vs1 vs2)
                                => ValidMelConcatStrict (v1 :-- vs1) (v2 :-- vs2)

-------------------------------------------------------------------------------
-- Voice leading constraints
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Chord constraints
-------------------------------------------------------------------------------

-- | Ensures that the chord is not a major seventh chord.
class ValidChordType (c :: ChordType n)
instance ValidChordType (Triad r t i)
instance {-# OVERLAPPING #-} ChordError "Can't have major seventh chords: " r " Maj7"
                                => ValidChordType (Tetrad r MajSeventh i)
instance {-# OVERLAPPABLE #-} ValidChordType (Tetrad r t i)
