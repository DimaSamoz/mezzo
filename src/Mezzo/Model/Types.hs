{-# LANGUAGE TypeInType, GADTs, TypeOperators, TypeFamilies, UndecidableInstances,
    TypeApplications, ScopedTypeVariables, FlexibleInstances, StandaloneDeriving, ViewPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Types
-- Description :  Mezzo music types
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types modeling basic musical constructs at the type level.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Types
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
    , Pit (..)
    , type (=?=)
    , type (<<=?)
    , type (<<?)
    -- * Harmonic types
    , Mode (..)
    , ScaleDegree (..)
    , KeyType (..)
    , RootType (..)
    , Mod (..)
    , ScaDeg (..)
    , KeyS (..)
    , Root (..)
    , RootToPitch
    , PitchToNat
    , Sharpen
    , Flatten
    , FromRoot
    , FromSilence
    -- * Specialised musical vector types
    , Voice
    , Partiture
    -- * Intervals
    , IntervalSize (..)
    , IntervalClass (..)
    , IntervalType (..)
    , MakeInterval
    , HalfStepsUpBy
    , HalfStepsDownBy
    , RaiseBy
    , LowerBy
    , RaiseAllBy
    , LowerAllBy
    , RaiseByOct
    , LowerByOct
    ) where

import GHC.TypeLits
import Data.Proxy

import Mezzo.Model.Prim
import Mezzo.Model.Reify

infixl 3 <<=?
infixl 3 <<?

-------------------------------------------------------------------------------
-- Note properties
-- The "minimum complete definition" for musical notes and rests.
-------------------------------------------------------------------------------

-- | The diatonic pitch class of the note.
data PitchClass = C | D | E | F | G | A | B

-- | The accidental applied to a note.
data Accidental = Natural | Flat | Sharp

-- | The octave where the note resides (middle C is Oct4).
data OctaveNum =
    Oct_1 | Oct0 | Oct1 | Oct2 | Oct3 | Oct4 | Oct5 | Oct6 | Oct7 | Oct8

-- | The duration of the note (a whole note has duration 32).
type Duration = Nat

---- Singleton types for note properties

-- | The singleton type for 'PitchClass'.
data PC (pc :: PitchClass) where
    PC :: Primitive pc => PC pc

-- | The singleton type for 'Accidental'.
data Acc (acc :: Accidental) where
    Acc :: Primitive acc => Acc acc

-- | The singleton type for 'Octave'.
data Oct (oct :: OctaveNum) where
    Oct :: Primitive oct => Oct oct

-- | The singleton type for 'Duration'.
data Dur (dur :: Duration) where
    Dur :: Primitive dur => Dur dur

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

-- | The singleton type for pitches.
data Pit (p :: PitchType) where
    Pit :: Primitive p => Pit p

-------------------------------------------------------------------------------
-- Harmonic types
-------------------------------------------------------------------------------

-- | The mode of a key: major or minor.
data Mode = MajorMode | MinorMode

-- | The seven scale degrees.
data ScaleDegree = I | II | III | IV | V | VI | VII

-- | The of a scale, chord or piece.
data KeyType = Key PitchClass Accidental Mode

-- | The root of a chord.
data RootType where
    -- | A pitch constructs a diatonic root.
    PitchRoot :: PitchType -> RootType
    -- | A key and a scale degree constructs a scalar root.
    DegreeRoot :: KeyType -> ScaleDegree -> RootType

-- | The singleton type for 'Mode'.
data Mod (m :: Mode) = Mod

-- | The singleton type for 'ScaleDegree'
data ScaDeg (sd :: ScaleDegree) = ScaDeg

-- | The singleton type for 'KeyType'.
data KeyS (k :: KeyType) = KeyS

-- | The singleton type for 'Root'.
data Root (r :: RootType) where
    Root :: Primitive r => Root r

-- | Convert a root to a pitch.
--
-- Note: the default octave for scalar roots is 'Oct2'.
type family RootToPitch (dr :: RootType) :: PitchType where
    RootToPitch (PitchRoot p) = p
    RootToPitch (DegreeRoot (Key pc acc m) d) =
                    HalfStepsUpBy (Pitch pc acc Oct2) (DegreeOffset m d)

-- | Calculate the semitone offset of a scale degree in a given mode.
type family DegreeOffset (m :: Mode) (d :: ScaleDegree) where
    DegreeOffset MajorMode I   = 0
    DegreeOffset MajorMode II  = 2
    DegreeOffset MajorMode III = 4
    DegreeOffset MajorMode IV  = 5
    DegreeOffset MajorMode V   = 7
    DegreeOffset MajorMode VI  = 9
    DegreeOffset MajorMode VII = 11
    DegreeOffset MinorMode I   = 0
    DegreeOffset MinorMode II  = 2
    DegreeOffset MinorMode III = 3
    DegreeOffset MinorMode IV  = 5
    DegreeOffset MinorMode V   = 7
    DegreeOffset MinorMode VI  = 8
    DegreeOffset MinorMode VII = 10

-- | Sharpen a root.
type family Sharpen (r :: RootType) :: RootType where
    Sharpen r = PitchRoot (HalfStepUp (RootToPitch r))

-- | Flatten a root.
type family Flatten (r :: RootType) :: RootType where
    Flatten r = PitchRoot (HalfStepDown (RootToPitch r))

-- | Create a new partiture with one voice of the given pitch.
type family FromRoot (r :: RootType) (d :: Nat) :: Partiture 1 d where
    FromRoot r d = ((RootToPitch r) +*+ d) :-- None

-- | Create a new partiture with one voice of silence.
type family FromSilence (d :: Nat) :: Partiture 1 d where
    FromSilence d = (Silence +*+ d) :-- None

-------------------------------------------------------------------------------
-- Type specialisations
-------------------------------------------------------------------------------

-- | A 'Voice' is made up of a sequence of pitch repetitions.
type Voice l = OptVector PitchType l

-- | A 'Partiture' is made up of a fixed number of voices.
type Partiture n l = Matrix PitchType n l

-- type family PadFst (p :: Partiture n1 l1) (q :: Partiture nAim l2) :: Partiture nAim l1 where
--     PadFst None None = None
--     PadFst None ((y :: Voice l) :-- ys) = FromSilence l ++. (PadFst None ys)
--     PadFst (x :-- xs) (y :-- ys) = x :-- PadFst xs ys
--
-- type family SeqPad (p :: Partiture n1 l1) (q :: Partiture n2 l2) :: Partiture (MaxN n1 n2) (l1 + l2) where
--     SeqPad (p :: Partiture n l1) (q :: Partiture n l2) = p +|+ q
--     SeqPad (p :: Partiture n1 l1) (q :: Partiture n2 l2) =
--                     If (n1 <=? n2)  -- If p has fewer voices
--                     (PadFst p q +|+ q)  -- Then pad p and concatenate with q
--                     (p +|+ PadFst q p)  -- Else

-- durT :: Dur n -> Dur m -> Dur (MaxN n m)
-- durT = undefined

-- t2 :: (Pad None ((Silence ** 6 :- End) :-- (Silence ** 6 :- End) :-- None)) ~ ((Silence ** 6 :- End) :-- (Silence ** 6 :- End) :-- None) => Int
-- t2 = 5
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
    MakeIntervalOrd (Pitch C Natural o) (Pitch C Sharp o) = Interval Aug Unison
    MakeIntervalOrd (Pitch C Natural o) (Pitch D Flat o) = Interval Min Second
    MakeIntervalOrd (Pitch C acc o)     (Pitch D acc o)   = Interval Maj Second
    MakeIntervalOrd (Pitch C acc o)     (Pitch E acc o)   = Interval Maj Third
    MakeIntervalOrd (Pitch C acc o)     (Pitch F acc o)   = Interval Perf Fourth
    MakeIntervalOrd (Pitch C acc o)     (Pitch G acc o)   = Interval Perf Fifth
    MakeIntervalOrd (Pitch C acc o)     (Pitch A acc o)   = Interval Maj Sixth
    MakeIntervalOrd (Pitch C acc o)     (Pitch B acc o)   = Interval Maj Seventh
    -- Handling perfect and augmented octaves.
    MakeIntervalOrd (Pitch C acc o1) (Pitch C acc o2) =
            If (OctSucc o1 .~. o2) (Interval Perf Octave) Compound
    MakeIntervalOrd (Pitch C Natural o1) (Pitch C Sharp o2) =
            If (OctSucc o1 .~. o2) (Interval Aug Octave) Compound
    MakeIntervalOrd (Pitch C Flat o1) (Pitch C Natural o2) =
            If (OctSucc o1 .~. o2) (Interval Aug Octave) Compound
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
                Compound
    -- Handling erroneous construction (shouldn't happen).
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
    Shrink  Compound              = Compound

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
    Expand  Compound               = Compound

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
    PitchToNat (Pitch C Natural Oct1)  = 24
    PitchToNat (Pitch C Natural Oct2)  = 36
    PitchToNat (Pitch C Natural Oct3)  = 48
    PitchToNat (Pitch C Natural Oct4)  = 60
    PitchToNat (Pitch C Natural Oct5)  = 72
    PitchToNat (Pitch C Natural Oct6)  = 84
    PitchToNat p                       = 1 + PitchToNat (HalfStepDown p)

-- | Convert a natural number to a suitable pitch.
-- Not a functional relation, so usage is not recommended.
type family NatToPitch (n :: Nat) where
    NatToPitch 0 = Pitch C Natural Oct_1
    NatToPitch 1 = Pitch C Sharp Oct_1
    NatToPitch n = HalfStepUp (NatToPitch (n - 1))

-- | Greater than or equal to for pitches.
type family (p1 :: PitchType) <<=? (p2 :: PitchType) where
    p1 <<=? p2 = PitchToNat p1 <=? PitchToNat p2

-- | Greater than for pitches.
type family (p1 :: PitchType) <<? (p2 :: PitchType) where
    p1 <<? p2 = (p1 <<=? p2) .&&. Not (p1 .~. p2)

-- | Enharmonic equality of pitches.
type family (p :: PitchType) =?= (q :: PitchType) :: Bool where
    Silence             =?= Silence             = True
    Silence             =?= _                   = False
    _                   =?= Silence             = False
    Pitch pc acc oct    =?= Pitch pc acc oct    = True
    Pitch C Flat o1     =?= Pitch B Natural o2  = o1 .~. OctSucc o2
    Pitch C Natural o1  =?= Pitch B Sharp o2    = o1 .~. OctSucc o2
    Pitch E Natural oct =?= Pitch F Flat oct    = True
    Pitch E Sharp oct   =?= Pitch F Natural oct = True
    Pitch F Flat oct    =?= Pitch E Natural oct = True
    Pitch F Natural oct =?= Pitch E Sharp oct   = True
    Pitch B Natural o1  =?= Pitch C Flat o2     = OctSucc o1 .~. o2
    Pitch B Sharp o1    =?= Pitch C Natural o2  = OctSucc o1 .~. o2
    Pitch pc1 Sharp oct =?= Pitch pc2 Flat oct  = ClassSucc pc1 .~. pc2
    Pitch pc1 Flat oct  =?= Pitch pc2 Sharp oct = pc1 .~. ClassSucc pc2
    _                   =?= _                   = False

-- | Convert an octave to a natural number.
type family OctToNat (o :: OctaveNum) :: Nat where
    OctToNat Oct_1 = 0
    OctToNat Oct0  = 1
    OctToNat Oct1  = 2
    OctToNat Oct2  = 3
    OctToNat Oct3  = 4
    OctToNat Oct4  = 5
    OctToNat Oct5  = 6
    OctToNat Oct6  = 7
    OctToNat Oct7  = 8
    OctToNat Oct8  = 9

-- | Convert a natural number to an octave.
type family NatToOct (n :: Nat) :: OctaveNum where
    NatToOct 0 = Oct_1
    NatToOct 1 = Oct0
    NatToOct 2 = Oct1
    NatToOct 3 = Oct2
    NatToOct 4 = Oct3
    NatToOct 5 = Oct4
    NatToOct 6 = Oct5
    NatToOct 7 = Oct6
    NatToOct 8 = Oct7
    NatToOct 9 = Oct8
    NatToOct _ = TypeError (Text "Invalid octave.")

-- | Increase the octave by the given number.
type family IncreaseOctave (o :: OctaveNum) (n :: Nat) :: OctaveNum where
    IncreaseOctave o n = NatToOct (OctToNat o + n)

-- | Decrease the octave by the given number.
type family DecreaseOctave (o :: OctaveNum) (n :: Nat) :: OctaveNum where
    DecreaseOctave o n = NatToOct (OctToNat o - n)

-- | Increment an octave.
type family OctSucc (o :: OctaveNum) :: OctaveNum where
    OctSucc o = IncreaseOctave o 1

-- | Decrement an octave.
type family OctPred (o :: OctaveNum) :: OctaveNum where
    OctPred o = DecreaseOctave o 1

-- | Convert a pitch class to a natural number.
type family ClassToNat (pc :: PitchClass) :: Nat where
    ClassToNat C = 0
    ClassToNat D = 1
    ClassToNat E = 2
    ClassToNat F = 3
    ClassToNat G = 4
    ClassToNat A = 5
    ClassToNat B = 6

-- | Convert a natural number to a pitch class.
-- Numbers are taken modulo 7: e.g. 8 corresponds to the pitch 8 mod 7 = 1 = D
type family NatToClass (n :: Nat) :: PitchClass where
    NatToClass 0 = C
    NatToClass 1 = D
    NatToClass 2 = E
    NatToClass 3 = F
    NatToClass 4 = G
    NatToClass 5 = A
    NatToClass 6 = B
    NatToClass n = NatToClass (n - 7)

-- | Increase the pitch class by a given number.
type family IncreaseClass (pc :: PitchClass) (n :: Nat) :: PitchClass where
    IncreaseClass pc n = NatToClass (ClassToNat pc + n)

-- | Decrease the pitch class by a given number.
type family DecreaseClass (pc :: PitchClass) (n :: Nat) :: PitchClass where
    DecreaseClass pc n = NatToClass (ClassToNat pc - n)

-- | Increment a pitch class.
type family ClassSucc (pc :: PitchClass) :: PitchClass where
    ClassSucc pc = IncreaseClass pc 1

-- | Decrement a pitch class.
type family ClassPred (pc :: PitchClass) :: PitchClass where
    ClassPred pc = DecreaseClass pc 1

-- | Convert an interval size to a natural number.
type family IntSizeToNat (is :: IntervalSize) :: Nat where
    IntSizeToNat Unison  = 0
    IntSizeToNat Second  = 1
    IntSizeToNat Third   = 2
    IntSizeToNat Fourth  = 3
    IntSizeToNat Fifth   = 4
    IntSizeToNat Sixth   = 5
    IntSizeToNat Seventh = 6
    IntSizeToNat Octave  = 7

-- | Convert a natural number to an interval size.
type family NatToIntSize (n :: Nat) :: IntervalSize where
    NatToIntSize 0 = Unison
    NatToIntSize 1 = Second
    NatToIntSize 2 = Third
    NatToIntSize 3 = Fourth
    NatToIntSize 4 = Fifth
    NatToIntSize 5 = Sixth
    NatToIntSize 6 = Seventh
    NatToIntSize 7 = Octave
    NatToIntSize _ = TypeError (Text "Invalid interval size.")

-- | Increase the interval size by a given number.
type family IncreaseIntSize (is :: IntervalSize) (n :: Nat) :: IntervalSize where
    IncreaseIntSize is n = NatToIntSize (IntSizeToNat is + n)

-- | Decrease the interval size by a given number.
type family DecreaseIntSize (is :: IntervalSize) (n :: Nat) :: IntervalSize where
    DecreaseIntSize is n = NatToIntSize (IntSizeToNat is - n)

-- | Increment an interval size.
type family IntSizeSucc (is :: IntervalSize) :: IntervalSize where
    IntSizeSucc is = IncreaseIntSize is 1

-- | Decrement an interval size.
type family IntSizePred (is :: IntervalSize) :: IntervalSize where
    IntSizePred is = DecreaseIntSize is 1

-- | Calculate the width of an interval in half-steps.
type family IntervalWidth (i :: IntervalType) :: Nat where
    IntervalWidth (Interval Dim Unison)  = TypeError (Text "Can't diminish unisons.")
    IntervalWidth (Interval Perf Unison) = 0
    IntervalWidth (Interval Aug Unison)  = 1
    IntervalWidth (Interval Dim Fourth)  = 4
    IntervalWidth (Interval Perf Fourth) = 5
    IntervalWidth (Interval Aug Fourth)  = 6
    IntervalWidth (Interval Dim Fifth)   = 6
    IntervalWidth (Interval Perf Fifth)  = 7
    IntervalWidth (Interval Aug Fifth)   = 8
    IntervalWidth (Interval Dim Octave)  = 11
    IntervalWidth (Interval Perf Octave) = 12
    IntervalWidth (Interval Aug Octave)  = 13
    IntervalWidth (Interval Maj Second)  = 2
    IntervalWidth (Interval Maj Third)   = 4
    IntervalWidth (Interval Maj Sixth)   = 9
    IntervalWidth (Interval Maj Seventh) = 11
    IntervalWidth (Interval Aug is)      = IntervalWidth (Interval Maj is) + 1
    IntervalWidth (Interval Min is)      = IntervalWidth (Interval Maj is) - 1
    IntervalWidth (Interval Dim is)      = IntervalWidth (Interval Maj is) - 2

-- | Move a pitch up by a semitone.
type family HalfStepUp (p :: PitchType) :: PitchType where
    HalfStepUp Silence              = Silence
    HalfStepUp (Pitch B  Flat    o) = Pitch B Natural o
    HalfStepUp (Pitch B  acc     o) = Pitch C acc (OctSucc o)
    HalfStepUp (Pitch E  Flat    o) = Pitch E Sharp o
    HalfStepUp (Pitch E  acc     o) = Pitch F acc o
    HalfStepUp (Pitch pc Flat    o) = Pitch pc Natural o
    HalfStepUp (Pitch pc Natural o) = Pitch pc Sharp o
    HalfStepUp (Pitch pc Sharp   o) = Pitch (ClassSucc pc) Natural o

-- | Move a pitch down by a semitone.
type family HalfStepDown (p :: PitchType) :: PitchType where
    HalfStepDown Silence              = Silence
    HalfStepDown (Pitch C  Sharp   o) = Pitch C Natural o
    HalfStepDown (Pitch C  acc     o) = Pitch B acc (OctPred o)
    HalfStepDown (Pitch F  Sharp   o) = Pitch F Natural o
    HalfStepDown (Pitch F  acc     o) = Pitch E acc o
    HalfStepDown (Pitch pc Flat    o) = Pitch (ClassPred pc) Natural o
    HalfStepDown (Pitch pc Natural o) = Pitch pc Flat o
    HalfStepDown (Pitch pc Sharp   o) = Pitch pc Natural o

-- | Move a pitch up by the specified number of semitones.
type family HalfStepsUpBy (p :: PitchType) (n :: Nat) :: PitchType where
    HalfStepsUpBy p 0 = p
    HalfStepsUpBy p n = HalfStepUp (HalfStepsUpBy p (n - 1))

-- | Move a pitch down by the specified number of semitones.
type family HalfStepsDownBy (p :: PitchType) (n :: Nat) :: PitchType where
    HalfStepsDownBy p 0 = p
    HalfStepsDownBy p n = HalfStepDown (HalfStepsDownBy p (n - 1))

-- | Raise a pitch by an interval.
type family RaiseBy (p :: PitchType) (i :: IntervalType) :: PitchType where
    RaiseBy Silence _           = Silence
    RaiseBy _ Compound          = TypeError (Text "Can't shift by compound interval")
    RaiseBy p (Interval Min is) = HalfStepDown (HalfStepsUpBy p (IntervalWidth (Interval Min is) + 1))
    RaiseBy p (Interval Dim is) = HalfStepDown (HalfStepsUpBy p (IntervalWidth (Interval Dim is) + 1))
    RaiseBy p i                 = HalfStepsUpBy p (IntervalWidth i)

-- | Lower a pitch by an interval.
type family LowerBy (p :: PitchType) (i :: IntervalType) :: PitchType where
    LowerBy Silence _           = Silence
    LowerBy _ Compound          = TypeError (Text "Can't shift by compound interval")
    LowerBy p (Interval Maj is) = HalfStepUp (HalfStepsDownBy p (IntervalWidth (Interval Maj is) + 1))
    LowerBy p (Interval Aug is) = HalfStepUp (HalfStepsDownBy p (IntervalWidth (Interval Aug is) + 1))
    LowerBy p i                 = HalfStepsDownBy p (IntervalWidth i)

-- | Raise multiple pitches by an interval.
type family RaiseAllBy (ps :: Vector PitchType n) (i :: IntervalType) :: Vector PitchType n where
    RaiseAllBy None _ = None
    RaiseAllBy (p :-- ps) i = RaiseBy p i :-- RaiseAllBy ps i

-- | Lower multiple pitches by an interval.
type family LowerAllBy (ps :: Vector PitchType n) (i :: IntervalType) :: Vector PitchType n where
    LowerAllBy None _ = None
    LowerAllBy (p :-- ps) i = LowerBy p i :-- LowerAllBy ps i

-- | Raise a pitch by an octave.
type family RaiseByOct (p :: PitchType) :: PitchType where
    RaiseByOct p = RaiseBy p (Interval Perf Octave)

-- | Lower a pitch by an octave.
type family LowerByOct (p :: PitchType) :: PitchType where
    LowerByOct p = LowerBy p (Interval Perf Octave)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Primitive Oct_1 where prim o = 0 ; pretty o = "_5"
instance Primitive Oct0 where prim o = 12 ; pretty o = "_4"
instance Primitive Oct1 where prim o = 24 ; pretty o = "_3"
instance Primitive Oct2 where prim o = 36 ; pretty o = "__"
instance Primitive Oct3 where prim o = 48 ; pretty o = "_ "
instance Primitive Oct4 where prim o = 60 ; pretty o = "  "
instance Primitive Oct5 where prim o = 72 ; pretty o = "' "
instance Primitive Oct6 where prim o = 84 ; pretty o = "''"
instance Primitive Oct7 where prim o = 96 ; pretty o = "'3"
instance Primitive Oct8 where prim o = 108; pretty o = "'4"

instance Primitive C where prim p = 0 ; pretty p = "C"
instance Primitive D where prim p = 2 ; pretty p = "D"
instance Primitive E where prim p = 4 ; pretty p = "E"
instance Primitive F where prim p = 5 ; pretty p = "F"
instance Primitive G where prim p = 7 ; pretty p = "G"
instance Primitive A where prim p = 9 ; pretty p = "A"
instance Primitive B where prim p = 11; pretty p = "B"

instance Primitive Natural where prim a = 0 ; pretty a = " "
instance Primitive Flat where prim a = -1 ; pretty a = "b"
instance Primitive Sharp where prim a = 1 ; pretty a = "#"

instance (Primitive pc, Primitive acc, Primitive oct) => Primitive (Pitch pc acc oct) where
    prim p = prim (PC @pc) + prim (Acc @acc) + prim (Oct @oct)
    pretty p = pretty (PC @pc) ++ pretty (Acc @acc) ++ pretty (Oct @oct)

instance Primitive Silence where prim s = 60 ; pretty s = "~dsdfs~~~"

instance Primitive p => Primitive (Root (PitchRoot p)) where
    prim r = prim (Pit @p)
    pretty r = pretty (Pit @p)

-- Modes
instance Primitive MajorMode where prim m = 0 ; pretty m = "Major"
instance Primitive MinorMode where prim m = 1 ; pretty m = "minor"

-- Scale degrees
instance Primitive I where prim d = 0 ; pretty d = "I"
instance Primitive II where prim d = 1 ; pretty d = "II"
instance Primitive III where prim d = 2 ; pretty d = "III"
instance Primitive IV where prim d = 3 ; pretty d = "IV"
instance Primitive V where prim d = 4 ; pretty d = "V"
instance Primitive VI where prim d = 5 ; pretty d = "VI"
instance Primitive VII where prim d = 6 ; pretty d = "VII"


instance (Primitive pc, Primitive acc, Primitive mo) => Primitive (Key pc acc mo) where
    prim k = 0 -- to be changed
    pretty k = pretty (PC @pc) ++ pretty (Acc @acc) ++ " " ++ pretty (Mod @mo)


instance (RootToPitch (DegreeRoot k sd) ~ p, Primitive p, Primitive sd) => Primitive (Root (DegreeRoot k sd)) where
    prim r = prim (Pit @p)
    pretty r = pretty (ScaDeg @sd)

instance Primitive p => Primitive (PitchRoot p) where
    prim p = prim (Pit @p)
    pretty p = pretty (Pit @p)

instance KnownNat n => Primitive n where
    prim = fromInteger . natVal
    pretty (natVal -> 1) = "Th"
    pretty (natVal -> 2) = "Si"
    pretty (natVal -> 4) = "Ei"
    pretty (natVal -> 8) = "Qu"
    pretty (natVal -> 16) = "Ha"
    pretty (natVal -> 32) = "Wh"
    pretty (natVal -> n) = ":" ++ show n
