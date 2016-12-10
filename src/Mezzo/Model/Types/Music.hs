{-# LANGUAGE TypeInType, RankNTypes, TypeOperators, GADTs, ConstraintKinds,
    MultiParamTypeClasses, TypeFamilies, UndecidableInstances, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Types.Music
-- Description :  Mezzo music types
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types modeling musical constructs at the type level.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Types.Music
    (
    -- * Note properties
      PitchClass (..)
    , Accidental (..)
    , OctaveNum (..)
    , Duration (..)
    -- ** Singleton types for note properties
    , PC (..)
    , Acc (..)
    , Oct (..)
    , Dur (..)
    -- * Pitches
    , PitchType (..)
    -- * Intervals
    , IntervalSize (..)
    , IntervalClass (..)
    , IntervalType (..)
    , MakeInterval
    -- * Music
    , Music (..)
    , Score (..)
    ) where

import Data.Kind
import GHC.TypeLits

import Mezzo.Model.Types.Prim

-------------------------------------------------------------------------------
-- Note properties
-- The "minimum complete definition" for musical notes and rests.
-------------------------------------------------------------------------------

-- | The diatonic pitch class of the note.
data PitchClass = C | D | E | F | G | A | B

-- | The accidental applied to a note.
data Accidental = Natural | Flat | Sharp

-- | The octave where the note resides (middle C is Oct3).
data OctaveNum =
    Oct_1 | Oct0 | Oct1 | Oct2 | Oct3 | Oct4 | Oct5 | Oct6 | Oct7 | Oct8

-- | The duration of the note (a whole note has duration 32).
type Duration = Nat

---- Singleton types for note properties

-- | The singleton type for 'PitchClass'.
data PC (pc :: PitchClass) where
    PC :: PC pc

-- | The singleton type for 'Accidental'.
data Acc (acc :: Accidental) where
    Acc :: Acc acc

-- | The singleton type for 'Octave'.
data Oct (oct :: OctaveNum) where
    Oct :: Oct oct

-- | The singleton type for 'Duration'.
data Dur (dur :: Duration) where
    Dur :: Dur dur

-------------------------------------------------------------------------------
-- Pitches
-- Encapsulates the pitch class, accidental and octave of a note.
-------------------------------------------------------------------------------

-- | The type of pitches.
data PitchType where
    -- | A pitch made up of a pitch class, an accidental and an octave.
    Pitch :: PitchClass -> Accidental -> OctaveNum -> PitchType
    -- | Silence, the pitch of rests.
    Silence :: PitchType

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

-- | The size of the interval.
data IntervalSize =
    Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave

-- | The class of the interval.
data IntervalClass = Maj | Perf | Min | Aug | Dim

-- | The type of intervals.
data IntervalType where
    -- | An interval smaller than 13 semitones, where musical rules
    -- can still be enforced.
    Interval :: IntervalClass -> IntervalSize -> IntervalType
    -- | An interval larger than 13 semitones, which is large enough
    -- so that dissonance effects are not significant.
    Compound :: IntervalType

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
    -- | Parallel or harmotic composition of music.
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
instance {-# OVERLAPS #-}     ValidMelLeap v1 v2 => ValidMelAppend (v1 :- Nil) (v2 :- vs2)
instance {-# OVERLAPPABLE #-} ValidMelAppend vs1 vs2 => ValidMelAppend (v :- vs1) vs2

-- | Ensures that two pitch matrices can be horizontally concatenated.
--
-- Two pitch matrices can be horizontally concatenated if
--
--  * both of them are empty or
--  * all of the row vectors can be appended.
class ValidMelConcat (m1 :: Matrix t p q) (m2 :: Matrix t p r)
instance {-# OVERLAPPING #-} ValidMelConcat Nil Nil
instance {-# OVERLAPPABLE #-} (ValidMelAppend row1 row2, ValidMelConcat rest1 rest2)
                                => ValidMelConcat (row1 :- rest1) (row2 :- rest2)


---- Harmonic constraints

-- | Ensures that two pieces of music can be composed in parallel.
type ValidHarmComp m1 m2 = (AllSatisfy OnlyValidHarmDyads (Transpose (m1 +|+ m2)))

-- | Ensures that harmonic intervals are valid.
--
-- A harmonic interval is invalid if it is
--
--  * a minor second or
--  * a major seventh or
--  * an augmented octave.
class ValidHarmInterval (i :: IntervalType)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have minor seconds in chords.") => ValidHarmInterval (Interval Aug Unison)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have minor seconds in chords.") => ValidHarmInterval (Interval Min Second)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have major sevenths in chords.") => ValidHarmInterval (Interval Maj Seventh)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have major sevenths in chords.") => ValidHarmInterval (Interval Dim Octave)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have augmented octaves in chords.") => ValidHarmInterval (Interval Aug Octave)
instance {-# OVERLAPPABLE #-} ValidHarmInterval i

-- | Ensures that two pitches form valid harmonic dyad (interval).
--
-- Two pitches form valid harmonic dyads if
--
--  * at least one of them is silent (i.e. it is a rest) or
--  * they form a valid harmonic interval.
class ValidHarmDyad (p1 :: PitchType) (p2 :: PitchType)
instance {-# OVERLAPPING #-} ValidHarmDyad Silence Silence
instance {-# OVERLAPPING #-} ValidHarmDyad (Pitch pc acc oct) Silence
instance {-# OVERLAPPING #-} ValidHarmDyad Silence (Pitch pc acc oct)
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
instance {-# OVERLAPPING #-} OnlyValidHarmDyads Nil
instance {-# OVERLAPPING #-} OnlyValidHarmDyads (p :- Nil)
instance {-# OVERLAPPING #-} ValidHarmDyad p1 p2 => OnlyValidHarmDyads (p1 :- p2 :- Nil)
instance {-# OVERLAPPABLE #-} (AllSatisfy (ValidHarmDyad p) ps, OnlyValidHarmDyads ps) => OnlyValidHarmDyads (p :- ps)

-------------------------------------------------------------------------------
-- Interval construction
-------------------------------------------------------------------------------

-- | Make an interval from two arbitrary pitches.
type family MakeInterval (p1 :: PitchType) (p2 :: PitchType) :: IntervalType where
    MakeInterval Silence Silence = TypeError (Text "Can't make intervals from rests.")
    MakeInterval Silence p2      = TypeError (Text "Can't make intervals from rests.")
    MakeInterval p1      Silence = TypeError (Text "Can't make intervals from rests.")
    MakeInterval p1 p2 =
        If  (p1 <<=? p2)
            (MakeIntervalOrd p1 p2)
            (MakeIntervalOrd p2 p1)

-- | Make an interval from two ordered pitches.
type family MakeIntervalOrd (p1 :: PitchType) (p2 :: PitchType) :: IntervalType where
    -- Handling base cases.
    MakeIntervalOrd p p = Interval Perf Unison
    MakeIntervalOrd (Pitch C acc o) (Pitch D acc o) = Interval Maj Second
    MakeIntervalOrd (Pitch C acc o) (Pitch E acc o) = Interval Maj Third
    MakeIntervalOrd (Pitch C acc o) (Pitch F acc o) = Interval Perf Fourth
    MakeIntervalOrd (Pitch C acc o) (Pitch G acc o) = Interval Perf Fifth
    MakeIntervalOrd (Pitch C acc o) (Pitch A acc o) = Interval Maj Sixth
    MakeIntervalOrd (Pitch C acc o) (Pitch B acc o) = Interval Maj Seventh
    -- Handling perfect and augmented octaves.
    MakeIntervalOrd (Pitch C acc o1) (Pitch C acc o2) =
            If (OctSucc o1 .~. o2) (Interval Perf Octave) (Compound)
    MakeIntervalOrd (Pitch C Natural o1) (Pitch C Sharp o2) =
            If (OctSucc o1 .~. o2) (Interval Aug Octave) (Compound)
    MakeIntervalOrd (Pitch C Flat o1) (Pitch C Natural o2) =
            If (OctSucc o1 .~. o2) (Interval Aug Octave) (Compound)
    -- Handling accidental first pitch.
    MakeIntervalOrd (Pitch C Flat o) (Pitch pc2 acc o) =
            Expand (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 acc o))
    MakeIntervalOrd (Pitch C Sharp o) (Pitch pc2 acc o) =
            Shrink (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 acc o))
    -- Handling accidental second pitch.
    MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Sharp o) =
            Expand (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Natural o))
    MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Flat o) =
            Shrink (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Natural o))
    -- Handling the general case.
    MakeIntervalOrd (Pitch pc1 acc1 o1) (Pitch pc2 acc2 o2) =
            If  (o1 .~. o2 .||. OctSucc o1 .~. o2)
                (MakeIntervalOrd (HalfStepDown (Pitch pc1 acc1 o1)) (HalfStepDown (Pitch pc2 acc2 o2)))
                (Compound)
    -- Handling erroneous construction (shoudln't happen).
    MakeIntervalOrd _ _ = TypeError (Text "Invalid interval.")

-- | Shrink an interval.
type family Shrink (i :: IntervalType) :: IntervalType where
    Shrink (Interval Perf Unison) = TypeError (Text "Can't diminish unisons.")
    Shrink (Interval Perf is)     = Interval Dim is
    Shrink (Interval Min  is)     = Interval Dim is
    Shrink (Interval Maj  is)     = Interval Min is
    Shrink (Interval Aug  Unison) = Interval Perf Unison
    Shrink (Interval Aug  Fourth) = Interval Perf Fourth
    Shrink (Interval Aug  Fifth)  = Interval Perf Fifth
    Shrink (Interval Aug  Octave) = Interval Perf Octave
    Shrink (Interval Aug  is)     = Interval Maj is
    Shrink (Interval Dim  Unison) = TypeError (Text "Can't diminish unisons.")
    Shrink (Interval Dim  Second) = TypeError (Text "Can't diminish unisons.")
    Shrink (Interval Dim  Fifth)  = Interval Perf Fourth
    Shrink (Interval Dim  Sixth)  = Interval Dim Fifth
    Shrink (Interval Dim  is)     = Interval Min (IntSizePred is)
    Shrink (Compound)             = Compound

-- | Expand an interval.
type family Expand (i :: IntervalType) :: IntervalType where
    Expand (Interval Perf Octave)  = Interval Aug Octave
    Expand (Interval Perf is)      = Interval Aug is
    Expand (Interval Maj  is)      = Interval Aug is
    Expand (Interval Min  is)      = Interval Maj is
    Expand (Interval Dim  Unison)  = TypeError (Text "Can't diminish unisons.")
    Expand (Interval Dim  Fourth)  = Interval Perf Fourth
    Expand (Interval Dim  Fifth)   = Interval Perf Fifth
    Expand (Interval Dim  Octave)  = Interval Perf Octave
    Expand (Interval Dim  is)      = Interval Min is
    Expand (Interval Aug  Third)   = Interval Aug Fourth
    Expand (Interval Aug  Fourth)  = Interval Perf Fifth
    Expand (Interval Aug  Seventh) = Interval Aug Octave
    Expand (Interval Aug  Octave)  = Compound
    Expand (Interval Aug  is)      = Interval Maj (IntSizeSucc is)
    Expand (Compound)              = Compound

-------------------------------------------------------------------------------
-- Enumerations and orderings
-- Implementation of enumerators and ordering relations for applicable types.
-------------------------------------------------------------------------------

-- | Convert a pitch to a natural number (equal to its MIDI code).
type family PitchToNat (p :: PitchType) :: Nat where
    PitchToNat Silence = TypeError (Text "Can't convert a rest to a number.")
    PitchToNat (Pitch C Natural Oct_1) = 0
    PitchToNat (Pitch C Sharp Oct_1)   = 1
    PitchToNat (Pitch D Flat Oct_1)    = 1
    PitchToNat p                       = 1 + (PitchToNat (HalfStepDown p))

-- | Convert a natural number to a suitable pitch.
-- Not a functional relation, so usage is not recommended.
type family NatToPitch (n :: Nat) where
    NatToPitch 0 = Pitch C Natural Oct_1
    NatToPitch 1 = Pitch C Sharp Oct_1
    NatToPitch n = HalfStepUp (NatToPitch (n - 1))

-- | Comparison of pitches.
type family (p1 :: PitchType) <<=? (p2 :: PitchType) where
    p1 <<=? p2 = PitchToNat p1 <=? PitchToNat p2

-- | Increment an octave.
type family OctSucc (o :: OctaveNum) :: OctaveNum where
    OctSucc Oct_1 = Oct0
    OctSucc Oct0  = Oct1
    OctSucc Oct1  = Oct2
    OctSucc Oct2  = Oct3
    OctSucc Oct3  = Oct4
    OctSucc Oct4  = Oct5
    OctSucc Oct5  = Oct6
    OctSucc Oct6  = Oct7
    OctSucc Oct7  = Oct8
    OctSucc Oct8  = TypeError (Text "Octave is too high.")

-- | Decrement an octave.
type family OctPred (o :: OctaveNum) :: OctaveNum where
    OctPred Oct_1 = TypeError (Text "Octave is too low.")
    OctPred Oct0 = Oct_1
    OctPred Oct1 = Oct0
    OctPred Oct2 = Oct1
    OctPred Oct3 = Oct2
    OctPred Oct4 = Oct3
    OctPred Oct5 = Oct4
    OctPred Oct6 = Oct5
    OctPred Oct7 = Oct6
    OctPred Oct8 = Oct7

-- | Increment a pitch class.
type family ClassSucc (c :: PitchClass) :: PitchClass where
    ClassSucc C = D
    ClassSucc D = E
    ClassSucc E = F
    ClassSucc F = G
    ClassSucc G = A
    ClassSucc A = B
    ClassSucc B = C

-- | Decrement a pitch class.
type family ClassPred (c :: PitchClass) :: PitchClass where
    ClassPred C = B
    ClassPred D = C
    ClassPred E = D
    ClassPred F = E
    ClassPred G = F
    ClassPred A = G
    ClassPred B = A

-- | Increment an interval size.
type family IntSizeSucc (is :: IntervalSize) :: IntervalSize where
    IntSizeSucc Unison  = Second
    IntSizeSucc Second  = Third
    IntSizeSucc Third   = Fourth
    IntSizeSucc Fourth  = Fifth
    IntSizeSucc Fifth   = Sixth
    IntSizeSucc Sixth   = Seventh
    IntSizeSucc Seventh = Octave
    IntSizeSucc Octave  = Octave

-- | Decrement an interval size.
type family IntSizePred (is :: IntervalSize) :: IntervalSize where
    IntSizePred Unison  = Unison
    IntSizePred Second  = Unison
    IntSizePred Third   = Second
    IntSizePred Fourth  = Third
    IntSizePred Fifth   = Fourth
    IntSizePred Sixth   = Fifth
    IntSizePred Seventh = Sixth
    IntSizePred Octave  = Seventh

-- | Move a pitch up by a semitone.
type family HalfStepUp (p :: PitchType) :: PitchType where
    HalfStepUp Silence              = Silence
    HalfStepUp (Pitch B  acc     o) = Pitch C acc (OctSucc o)
    HalfStepUp (Pitch E  acc     o) = Pitch F acc o
    HalfStepUp (Pitch pc Flat    o) = Pitch pc Natural o
    HalfStepUp (Pitch pc Natural o) = Pitch pc Sharp o
    HalfStepUp (Pitch pc Sharp   o) = Pitch (ClassSucc pc) Natural o

-- | Move a pitch down by a semitone.
type family HalfStepDown (p :: PitchType) :: PitchType where
    HalfStepDown Silence              = Silence
    HalfStepDown (Pitch C  acc     o) = Pitch B acc (OctPred o)
    HalfStepDown (Pitch F  acc     o) = Pitch E acc o
    HalfStepDown (Pitch pc Flat    o) = Pitch (ClassPred pc) Natural o
    HalfStepDown (Pitch pc Natural o) = Pitch pc Flat o
    HalfStepDown (Pitch pc Sharp   o) = Pitch pc Natural o
