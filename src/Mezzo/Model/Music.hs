{-# LANGUAGE TypeInType, RankNTypes, TypeOperators, GADTs, ConstraintKinds,
    MultiParamTypeClasses, TypeFamilies, UndecidableInstances, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Music
-- Description :  Mezzo music algebra
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Algebraic description of music with type-level constraints.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Music
    (
    -- * Music
      Music (..)
    , Score (..)
    ) where

import Data.Kind
import GHC.TypeLits

import Mezzo.Model.Prim
import Mezzo.Model.Types

infixl 4 :|:
infixl 4 :-:

-------------------------------------------------------------------------------
-- The 'Music' datatype
-------------------------------------------------------------------------------

-- | A piece of music consisting of parallel and sequential composition of notes
-- and rests, subject to constraints.
--
-- Currently enforced constraints are:
--
--  * Height (number of voices) of two sequentially composed pieces must be equal.
--  * Width (number of temporal units) of two parallelly composed pieces must be equal.
--  * Sequentially composed voices cannot have any augmented, diminished or seventh leaps.
--  * Parallelly composed pieces cannot have any minor second or major seventh harmonic intervals.
--
data Music :: forall pt p q. Matrix pt p q -> Type where
    -- | A note specified by a pitch class, accidental, octave and duration.
    Note :: PC pc -> Acc acc -> Oct oct -> Dur d -> Music ((Pitch pc acc oct +*+ d) :- Nil)
    -- | A rest specified by a duration.
    Rest :: Dur d -> Music ((Silence +*+ d) :- Nil)
    -- | Sequential or melodic composition of music.
    (:|:) :: ValidMelComp m1 m2  => Music m1 -> Music m2 -> Music (m1 +|+ m2)
    -- | Parallel or harmonic composition of music.
    (:-:) :: ValidHarmComp m1 m2 => Music m1 -> Music m2 -> Music (m1 +-+ m2)

-- | A type encapsulating every 'Music' composition.
data Score = forall m. Score (Music m)

-------------------------------------------------------------------------------
-- Musical constraints
-- Specifications of the rules that valid musical terms have to follow.
-------------------------------------------------------------------------------

---- Melodic constraints

-- | Ensures that two pieces of music can be composed sequentially.
type ValidMelComp m1 m2 = (ValidMelConcat m1 m2)

-- | Ensures that melodic intervals are valid.
--
-- A melodic interval is invalid if it is
--
--  * any augmented interval or
--  * any diminished interval or
--  * any seventh interval.
class ValidMelInterval (i :: IntervalType)
instance {-# OVERLAPPING #-}  ValidMelInterval (Interval Aug Unison)
instance {-# OVERLAPS #-}     TypeError (Text "Augmented melodic intervals are not permitted.")
                                => ValidMelInterval (Interval Aug a)
instance {-# OVERLAPS #-}     TypeError (Text "Diminished melodic intervals are not permitted.")
                                => ValidMelInterval (Interval Dim a)
instance {-# OVERLAPPING #-}  TypeError (Text "Seventh intervals are not permitted in melody.")
                                => ValidMelInterval (Interval a Seventh)
instance {-# OVERLAPPABLE #-} ValidMelInterval i

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
instance {-# OVERLAPPABLE #-} ValidMelInterval (MakeInterval a b) => ValidMelLeap a b

-- | Ensures that two pitch vectors can be appended.
--
-- Two pitch vectors can be appended if
--
--  * at least one of them is empty or
--  * the last pitch of the first vector forms a valid melodic leap
--    with the first pitch of the second vector.
class ValidMelAppend (a :: Vector PitchType n) (b :: Vector PitchType m)
instance {-# OVERLAPPING #-}  ValidMelAppend Nil a
instance {-# OVERLAPPING #-}  ValidMelAppend a Nil
instance {-# OVERLAPPABLE #-} ValidMelLeap (Last vs1) v2 => ValidMelAppend vs1 (v2 :- vs2)

-- | Ensures that two pitch matrices can be horizontally concatenated.
--
-- Two pitch matrices can be horizontally concatenated if
--
--  * both of them are empty or
--  * all of the row vectors can be appended.
class ValidMelConcat (m1 :: Matrix t p q) (m2 :: Matrix t p r)
instance {-# OVERLAPPING #-}   ValidMelConcat Nil Nil
instance {-# OVERLAPPABLE #-} (ValidMelAppend row1 row2, ValidMelConcat rest1 rest2)
                                => ValidMelConcat (row1 :- rest1) (row2 :- rest2)


---- Harmonic constraints

-- | Ensures that two pieces of music can be composed in parallel.
type ValidHarmComp m1 m2 = (AllSatisfy OnlyValidHarmDyads (Transpose (m1 +-+ m2)))

-- | Ensures that harmonic intervals are valid.
--
-- A harmonic interval is invalid if it is
--
--  * a minor second or
--  * a major seventh or
--  * an augmented octave.
class ValidHarmInterval (i :: IntervalType)
instance {-# OVERLAPPING #-}  TypeError (Text "Can't have minor seconds in chords.")
                                => ValidHarmInterval (Interval Aug Unison)
instance {-# OVERLAPPING #-}  TypeError (Text "Can't have minor seconds in chords.")
                                => ValidHarmInterval (Interval Min Second)
instance {-# OVERLAPPING #-}  TypeError (Text "Can't have major sevenths in chords.")
                                => ValidHarmInterval (Interval Maj Seventh)
instance {-# OVERLAPPING #-}  TypeError (Text "Can't have major sevenths in chords.")
                                => ValidHarmInterval (Interval Dim Octave)
instance {-# OVERLAPPING #-}  TypeError (Text "Can't have augmented octaves in chords.")
                                => ValidHarmInterval (Interval Aug Octave)
instance {-# OVERLAPPABLE #-} ValidHarmInterval i

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
instance {-# OVERLAPPABLE #-} ValidHarmInterval (MakeInterval a b) => ValidHarmDyad a b

-- | Ensures that pitch vectors contain only valid harmonic dyads.
--
-- A vector contains only valid harmonic intervals if
--
--  * it is empty or a singleton vector or
--  * it contains two pitches that form valid harmonic dyads or
--  * the head forms valid harmonic dyads with all pitches in the tail and
--    the tail contains only valid harmonic dyad.
class OnlyValidHarmDyads (v :: Vector PitchType n)
instance {-# OVERLAPPING #-}   OnlyValidHarmDyads Nil
instance {-# OVERLAPPING #-}   OnlyValidHarmDyads (p :- Nil)
instance {-# OVERLAPPING #-}   ValidHarmDyad p1 p2 => OnlyValidHarmDyads (p1 :- p2 :- Nil)
instance {-# OVERLAPPABLE #-} (AllSatisfy (ValidHarmDyad p) ps, OnlyValidHarmDyads ps)
                                => OnlyValidHarmDyads (p :- ps)
