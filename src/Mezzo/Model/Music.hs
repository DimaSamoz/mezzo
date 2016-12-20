{-# LANGUAGE TypeInType, RankNTypes, TypeOperators, GADTs, ConstraintKinds,
    FlexibleContexts, MultiParamTypeClasses, TypeFamilies, UndecidableInstances,
    FlexibleInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

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
import Mezzo.Model.Harmony
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
--  * Music must not contain parallel or concealed unisons, fifths or octaves.
--
data Music :: forall pt p q. Matrix pt p q -> Type where
    -- | A note specified by a pitch class, accidental, octave and duration.
    Note :: PC pc -> Acc acc -> Oct oct -> Dur d -> Music (From (Pitch pc acc oct) d)
    -- | A rest specified by a duration.
    Rest :: Dur d -> Music (From Silence d)
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
type ValidMelComp m1 m2 = ( ValidMelConcat m1 m2
                          , ValidMelMatrixMotion m1 m2)

-- | Ensures that melodic intervals are valid.
--
-- A melodic interval is invalid if it is
--
--  * any augmented interval or
--  * any diminished interval or
--  * any seventh interval.
class ValidMelInterval (i :: IntervalType)
instance {-# OVERLAPPING #-}       ValidMelInterval (Interval Aug Unison)
instance {-# OVERLAPS #-} TypeError (Text "Augmented melodic intervals are not permitted.")
                                => ValidMelInterval (Interval Aug a)
instance {-# OVERLAPS #-} TypeError (Text "Diminished melodic intervals are not permitted.")
                                => ValidMelInterval (Interval Dim a)
instance {-# OVERLAPPING #-} TypeError (Text "Seventh intervals are not permitted in melody.")
                                => ValidMelInterval (Interval a Seventh)
instance {-# OVERLAPPABLE #-}      ValidMelInterval i

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
instance {-# OVERLAPPABLE #-} ValidMelLeap (Last vs1) v2 => ValidMelAppend vs1 (v2 :* d :- vs2)

-- | Ensures that two pitch matrices can be horizontally concatenated.
--
-- Two pitch matrices can be horizontally concatenated if
--
--  * both of them are empty or
--  * all of the row vectors can be appended.
class ValidMelConcat (m1 :: Matrix t p q) (m2 :: Matrix t p r)
instance {-# OVERLAPPING #-}       ValidMelConcat Nil Nil
instance {-# OVERLAPPABLE #-} (ValidMelAppend row1 row2, ValidMelConcat rest1 rest2)
                                => ValidMelConcat (row1 :* d1 :- rest1) (row2 :* d2  :- rest2)


---- Harmonic constraints

-- | Ensures that two pieces of music can be composed in parallel.
type ValidHarmComp m1 m2 = (ValidHarmConcat (Align m1 m2))

-- | Ensures that harmonic intervals are valid.
--
-- A harmonic interval is invalid if it is
--
--  * a minor second or
--  * a major seventh or
--  * an augmented octave.
class ValidHarmInterval (i :: IntervalType)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have minor seconds in chords.")
                                => ValidHarmInterval (Interval Aug Unison)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have minor seconds in chords.")
                                => ValidHarmInterval (Interval Min Second)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have major sevenths in chords.")
                                => ValidHarmInterval (Interval Maj Seventh)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have major sevenths in chords.")
                                => ValidHarmInterval (Interval Dim Octave)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have augmented octaves in chords.")
                                => ValidHarmInterval (Interval Aug Octave)
instance {-# OVERLAPPABLE #-}      ValidHarmInterval i

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

-- | Ensures that two pitch vectors form pairwise valid harmonic dyads.
class ValidHarmDyadsInVectors (v1 :: Vector PitchType l) (v2 :: Vector PitchType l)
instance AllPairsSatisfy ValidHarmDyad v1 v2 => ValidHarmDyadsInVectors v1 v2

-- | Ensures that two pitch matrices can be vertically concatenated.
--
-- Two pitch matrices can be vertically concatenated if
--
--  * the top one is empty or
--  * all but the first voice can be concatenated, and the first voice
--    forms valid harmonic dyads with every other voice and follows the rules
--    of valid harmonic motion.
class ValidHarmConcat (ms :: (Matrix t p q, Matrix t r q))
instance {-# OVERLAPPING #-}       ValidHarmConcat '(Nil, vs)
instance {-# OVERLAPPABLE #-} ( ValidHarmConcat '(vs, us)
                              , AllSatisfyAll [ ValidHarmDyadsInVectors v
                                              , ValidHarmMotionInVectors v] us)
                                => ValidHarmConcat '((v :* d :- vs), us)


---- Voice leading constraints

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
                        (DirectMotion (MakeInterval p1 p2) (MakeInterval q1 q2))
                        (ContraryMotion (MakeInterval p1 p2) (MakeInterval q1 q2)))
                    (If (p2 <<? q2)
                        (ContraryMotion (MakeInterval p1 p2) (MakeInterval q1 q2))
                        (DirectMotion (MakeInterval p1 p2) (MakeInterval q1 q2))))

-- | Ensures that the interval formed by the first pitch and the last element
-- of the first pitch vector can move to the interval formed by the second
-- pitch and the first element of the second pitch vector.
class ValidMelPitchVectorMotion (p1 :: PitchType) (p2 :: PitchType) (v1 :: Vector PitchType p) (v2 :: Vector PitchType q)
instance {-# OVERLAPPING #-}    ValidMelPitchVectorMotion p1 p2 Nil Nil
instance {-# OVERLAPPABLE #-} ValidMotion p1 (Last v1) p2 (Head v2)
                            =>  ValidMelPitchVectorMotion p1 p2 v1 v2
-- Can't have v1 be Nil and v2 be not Nil, since if v1 under p1 is not nil, there
-- must be an accompanying voice under p2

-- | Ensures that two pitch matrices follow the rules of motion when
-- horizontally concatenated.
--
-- Two horizontally concatenated pitch matrices follow the rules of harmonic motion if
--
--  * both are nil matrices or
--  * their lower voices can be concatenated and the joining elements of the
--    top voice form intervals with the joining elements of the other voices
--    which follow the rules of harmonic motion.
class ValidMelMatrixMotion (m1 :: Matrix PitchType p q) (m2 :: Matrix PitchType p r)
instance {-# OVERLAPPING #-}       ValidMelMatrixMotion Nil Nil
instance {-# OVERLAPPABLE #-} ( ValidMelMatrixMotion vs1 vs2
                              , AllPairsSatisfy (ValidMelPitchVectorMotion (Last v1) (Head v2)) vs1 vs2)
                                => ValidMelMatrixMotion (v1 :* d1 :- vs1) (v2 :* d2 :- vs2)

-- | Ensures that two pitch vectors form pairwise intervals which follow the
-- rules of harmonic motion.
class ValidHarmMotionInVectors (v :: Vector PitchType p) (u :: Vector PitchType p)
instance {-# OVERLAPPING #-}       ValidHarmMotionInVectors Nil Nil
-- Issues with pattern-matching and overlapping instances makes this necessary.
instance {-# OVERLAPPABLE #-} ( ValidMotion p q (If (IsEmpty ps) Silence (Head ps)) (If (IsEmpty qs) Silence (Head qs))
                              , ValidHarmMotionInVectors ps qs)
                                => ValidHarmMotionInVectors (p :* d1 :- ps) (q :* d2 :- qs)
