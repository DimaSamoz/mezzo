{-# LANGUAGE StandaloneDeriving, ViewPatterns #-}

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
    , DegreeType (..)
    , KeyType (..)
    , RootType (..)
    , Mod (..)
    , ScaDeg (..)
    , KeyS (..)
    , Deg (..)
    , Root (..)
    , RootToPitch
    , PitchToNat
    , Sharpen
    , Flatten
    , Dot
    , HalfOf
    , FromRoot
    , FromSilence
    , FromTriplet
    -- * Specialised musical vector types
    , Voice
    , Partiture
    -- * Intervals
    , IntervalSize (..)
    , IntervalClass (..)
    , IntervalType (..)
    , MakeInterval
    -- ** Singleton types for interval properties
    , IC (..)
    , IS (..)
    , Intv (..)
    -- * Operations
    , OctPred
    , OctSucc
    , HalfStepsUpBy
    , HalfStepsDownBy
    , RaiseBy
    , LowerBy
    , RaiseAllBy
    , LowerAllBy
    , RaiseAllBy'
    , LowerAllBy'
    , RaiseByOct
    , LowerByOct
    , RaiseAllByOct
    , TransposeUpBy
    , TransposeDownBy
    ) where

import GHC.TypeLits hiding (Mod)
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

data DegreeType = Degree ScaleDegree Accidental OctaveNum

-- | The of a scale, chord or piece.
data KeyType = Key PitchClass Accidental Mode

-- | The root of a chord.
data RootType where
    -- | A pitch constructs a diatonic root.
    PitchRoot :: PitchType -> RootType
    -- | A key and a scale degree constructs a scalar root.
    DegreeRoot :: KeyType -> DegreeType -> RootType

-- | The singleton type for 'Mode'.
data Mod (m :: Mode) = Mod

-- | The singleton type for 'ScaleDegree'
data ScaDeg (sd :: ScaleDegree) = ScaDeg

-- | The singleton type for 'KeyType'.
data KeyS (k :: KeyType) = KeyS

data Deg (d :: DegreeType) = Deg

-- | The singleton type for 'Root'.
data Root (r :: RootType) where
    Root :: Primitive r => Root r

-- | Convert a root to a pitch.
--
-- Note: the default octave for scalar roots is 'Oct2'.
type family RootToPitch (dr :: RootType) :: PitchType where
    RootToPitch (PitchRoot p) = p
    RootToPitch (DegreeRoot (Key pc acc m) (Degree sd dacc oct)) =
                    HalfStepsUpBy (Pitch pc acc oct) (DegreeOffset m sd dacc)

-- | Calculate the semitone offset of a scale degree in a given mode.
type family DegreeOffset (m :: Mode) (d :: ScaleDegree) (a :: Accidental) where
    DegreeOffset MajorMode I   Natural = 0
    DegreeOffset MajorMode II  Natural = 2
    DegreeOffset MajorMode III Natural = 4
    DegreeOffset MajorMode IV  Natural = 5
    DegreeOffset MajorMode V   Natural = 7
    DegreeOffset MajorMode VI  Natural = 9
    DegreeOffset MajorMode VII Natural = 11
    DegreeOffset MinorMode I   Natural = 0
    DegreeOffset MinorMode II  Natural = 2
    DegreeOffset MinorMode III Natural = 3
    DegreeOffset MinorMode IV  Natural = 5
    DegreeOffset MinorMode V   Natural = 7
    DegreeOffset MinorMode VI  Natural = 8
    DegreeOffset MinorMode VII Natural = 10
    DegreeOffset m         sd  Flat    = (DegreeOffset m sd Natural) - 1
    DegreeOffset m         sd  Sharp   = (DegreeOffset m sd Natural) + 1

-- | Sharpen a root.
type family Sharpen (r :: RootType) :: RootType where
    Sharpen r = PitchRoot (HalfStepUp (RootToPitch r))

-- | Flatten a root.
type family Flatten (r :: RootType) :: RootType where
    Flatten r = PitchRoot (HalfStepDown (RootToPitch r))

-- | Halve a type-level natural.
type family HalfOf (n :: Nat) :: Nat where
    HalfOf 0 = 0
    HalfOf 1 = 0
    HalfOf 8 = 4
    HalfOf 32 = 16
    HalfOf n = 1 + (HalfOf (n - 2))

-- | Form a dotted duration.
type family Dot (d :: Duration) :: Duration where
    Dot 1 = TypeError (Text "Can't have dotted thirty-seconds.")
    Dot n = n + HalfOf n

-- | Create a new partiture with one voice of the given pitch.
type family FromRoot (r :: RootType) (d :: Nat) :: Partiture 1 d where
    FromRoot r d = ((RootToPitch r) +*+ d) :-- None

-- | Create a new partiture with one voice of silence.
type family FromSilence (d :: Nat) :: Partiture 1 d where
    FromSilence d = (Silence +*+ d) :-- None

-- | Create a new partiture with a triplet of three notes.
type family FromTriplet (d :: Nat) (r1 :: RootType) (r2 :: RootType) (r3 :: RootType)
            :: Partiture 1 (d + HalfOf d + HalfOf d) where
    FromTriplet d r1 r2 r3 = FromRoot r1 d +|+ FromRoot r2 (HalfOf d) +|+ FromRoot r3 (HalfOf d)

-------------------------------------------------------------------------------
-- Type specialisations
-------------------------------------------------------------------------------

-- | A 'Voice' is made up of a sequence of pitch repetitions.
type Voice l = OptVector PitchType l

-- | A 'Partiture' is made up of a fixed number of voices.
type Partiture n l = Matrix PitchType n l

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

-- | The singleton type for 'IntervalSize'.
data IS (is :: IntervalSize) = IS

-- | The singleton type for 'IntervalClass'.
data IC (ic :: IntervalClass) = IC

-- | The singleton type for 'IntervalType'.
data Intv (i :: IntervalType) = Intv

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
    ---- Base cases from C.
    MakeIntervalOrd (Pitch C Flat o) (Pitch C Natural o) = Interval Aug Unison
    MakeIntervalOrd (Pitch C Natural o) (Pitch C Sharp o) = Interval Aug Unison
    MakeIntervalOrd (Pitch C Natural o) (Pitch D Flat o) = Interval Min Second
    MakeIntervalOrd (Pitch C acc o)     (Pitch D acc o)   = Interval Maj Second
    MakeIntervalOrd (Pitch C acc o)     (Pitch E acc o)   = Interval Maj Third
    MakeIntervalOrd (Pitch C acc o)     (Pitch F acc o)   = Interval Perf Fourth
    MakeIntervalOrd (Pitch C acc o)     (Pitch G acc o)   = Interval Perf Fifth
    MakeIntervalOrd (Pitch C acc o)     (Pitch A acc o)   = Interval Maj Sixth
    MakeIntervalOrd (Pitch C acc o)     (Pitch B acc o)   = Interval Maj Seventh
    ---- Base cases from F.
    MakeIntervalOrd (Pitch F Flat o) (Pitch F Natural o) = Interval Aug Unison
    MakeIntervalOrd (Pitch F Natural o) (Pitch F Sharp o) = Interval Aug Unison
    MakeIntervalOrd (Pitch F Natural o) (Pitch G Flat o) = Interval Min Second
    MakeIntervalOrd (Pitch F acc o)     (Pitch G acc o)   = Interval Maj Second
    MakeIntervalOrd (Pitch F acc o)     (Pitch A acc o)   = Interval Maj Third
    MakeIntervalOrd (Pitch F acc o)     (Pitch B acc o)   = Interval Aug Fourth
    MakeIntervalOrd (Pitch F acc o1)     (Pitch C acc o2)   =
            IntervalOrCompound o1 o2 (Interval Perf Fifth)
    MakeIntervalOrd (Pitch F acc o1)     (Pitch D acc o2)   =
            IntervalOrCompound o1 o2 (Interval Maj Sixth)
    MakeIntervalOrd (Pitch F acc o1)     (Pitch E acc o2)   =
            IntervalOrCompound o1 o2 (Interval Maj Seventh)
    ---- Base cases from A.
    MakeIntervalOrd (Pitch A Flat o) (Pitch A Natural o) = Interval Aug Unison
    MakeIntervalOrd (Pitch A Natural o) (Pitch A Sharp o) = Interval Aug Unison
    MakeIntervalOrd (Pitch A Natural o) (Pitch B Flat o) = Interval Min Second
    MakeIntervalOrd (Pitch A acc o)     (Pitch B acc o)   = Interval Maj Second
    MakeIntervalOrd (Pitch A acc o1)     (Pitch C acc o2)   =
            IntervalOrCompound o1 o2 (Interval Min Third)
    MakeIntervalOrd (Pitch A acc o1)     (Pitch D acc o2)   =
            IntervalOrCompound o1 o2 (Interval Perf Fourth)
    MakeIntervalOrd (Pitch A acc o1)     (Pitch E acc o2)   =
            IntervalOrCompound o1 o2 (Interval Perf Fifth)
    MakeIntervalOrd (Pitch A acc o1)     (Pitch F acc o2)   =
        IntervalOrCompound o1 o2 (Interval Min Sixth)
    MakeIntervalOrd (Pitch A acc o1)     (Pitch G acc o2)   =
        IntervalOrCompound o1 o2 (Interval Min Seventh)
    -- Handling perfect and augmented octaves.
    MakeIntervalOrd (Pitch C acc o1) (Pitch C acc o2) =
            IntervalOrCompound o1 o2 (Interval Perf Octave)
    MakeIntervalOrd (Pitch C Natural o1) (Pitch C Sharp o2) =
            IntervalOrCompound o1 o2 (Interval Aug Octave)
    MakeIntervalOrd (Pitch C Flat o1) (Pitch C Natural o2) =
            IntervalOrCompound o1 o2 (Interval Aug Octave)
    MakeIntervalOrd (Pitch F acc o1) (Pitch F acc o2) =
            IntervalOrCompound o1 o2 (Interval Perf Octave)
    MakeIntervalOrd (Pitch F Natural o1) (Pitch F Sharp o2) =
            IntervalOrCompound o1 o2 (Interval Aug Octave)
    MakeIntervalOrd (Pitch F Flat o1) (Pitch F Natural o2) =
            IntervalOrCompound o1 o2 (Interval Aug Octave)
    MakeIntervalOrd (Pitch A acc o1) (Pitch A acc o2) =
            IntervalOrCompound o1 o2 (Interval Perf Octave)
    MakeIntervalOrd (Pitch A Natural o1) (Pitch A Sharp o2) =
            IntervalOrCompound o1 o2 (Interval Aug Octave)
    MakeIntervalOrd (Pitch A Flat o1) (Pitch A Natural o2) =
            IntervalOrCompound o1 o2 (Interval Aug Octave)
    -- Handling accidental first pitch.
    MakeIntervalOrd (Pitch C Flat o) (Pitch pc2 acc o) =
            Expand (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 acc o))
    MakeIntervalOrd (Pitch C Sharp o) (Pitch pc2 acc o) =
            Shrink (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 acc o))
    MakeIntervalOrd (Pitch F Flat o) (Pitch E Sharp o) = Interval Min Second
    MakeIntervalOrd (Pitch F Flat o) (Pitch E Natural o) = Interval Dim Second
    MakeIntervalOrd (Pitch E Natural o) (Pitch F Flat o) = Interval Dim Second
    MakeIntervalOrd (Pitch F Flat o) (Pitch pc2 acc o) =
            Expand (MakeIntervalOrd (Pitch F Natural o) (Pitch pc2 acc o))
    MakeIntervalOrd (Pitch F Sharp o) (Pitch pc2 acc o) =
            Shrink (MakeIntervalOrd (Pitch F Natural o) (Pitch pc2 acc o))
    MakeIntervalOrd (Pitch A Flat o) (Pitch pc2 acc o) =
            Expand (MakeIntervalOrd (Pitch A Natural o) (Pitch pc2 acc o))
    MakeIntervalOrd (Pitch A Sharp o) (Pitch pc2 acc o) =
            Shrink (MakeIntervalOrd (Pitch A Natural o) (Pitch pc2 acc o))
    -- Handling accidental second pitch.
    MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Sharp o) =
            Expand (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Natural o))
    MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Flat o) =
            Shrink (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Natural o))
    MakeIntervalOrd (Pitch F Natural o) (Pitch pc2 Sharp o) =
            Expand (MakeIntervalOrd (Pitch F Natural o) (Pitch pc2 Natural o))
    MakeIntervalOrd (Pitch F Natural o) (Pitch pc2 Flat o) =
            Shrink (MakeIntervalOrd (Pitch F Natural o) (Pitch pc2 Natural o))
    MakeIntervalOrd (Pitch A Natural o) (Pitch pc2 Sharp o) =
            Expand (MakeIntervalOrd (Pitch A Natural o) (Pitch pc2 Natural o))
    MakeIntervalOrd (Pitch A Natural o) (Pitch pc2 Flat o) =
            Shrink (MakeIntervalOrd (Pitch A Natural o) (Pitch pc2 Natural o))
    -- Handling the general case.
    MakeIntervalOrd (Pitch pc1 acc1 o) (Pitch pc2 acc2 o) =
            MakeIntervalOrd (HalfStepDown (Pitch pc1 acc1 o)) (HalfStepDown (Pitch pc2 acc2 o))
    MakeIntervalOrd (Pitch pc1 acc1 o1) (Pitch pc2 acc2 o2) =
            If  (NextOct o1 o2)
                (MakeIntervalOrd (HalfStepDown (Pitch pc1 acc1 o1)) (HalfStepDown (Pitch pc2 acc2 o2)))
                Compound
    -- Handling erroneous construction (shouldn't happen).
    MakeIntervalOrd _ _ = TypeError (Text "Invalid interval.")

-- | Shrink an interval.
type family Shrink (i :: IntervalType) :: IntervalType where
    Shrink (Interval Perf Unison) = TypeError (Text "Can't diminish unisons.1")
    Shrink (Interval Perf is)     = Interval Dim is
    Shrink (Interval Min  is)     = Interval Dim is
    Shrink (Interval Maj  is)     = Interval Min is
    Shrink (Interval Aug  Unison) = Interval Perf Unison
    Shrink (Interval Aug  Fourth) = Interval Perf Fourth
    Shrink (Interval Aug  Fifth)  = Interval Perf Fifth
    Shrink (Interval Aug  Octave) = Interval Perf Octave
    Shrink (Interval Aug  is)     = Interval Maj is
    Shrink (Interval Dim  Unison) = TypeError (Text "Can't diminish unisons.2")
    Shrink (Interval Dim  Second) = TypeError (Text "Can't diminish unisons.3")
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
    Expand (Interval Dim  Unison)  = TypeError (Text "Can't diminish unisons.4")
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
type family (p1 :: PitchType) <<=? (p2 :: PitchType) :: Bool where
    p <<=? p = True
    (Pitch pc1 acc oct) <<=? (Pitch pc2 acc oct) = ClassToNat pc1 <=? ClassToNat pc2
    (Pitch pc acc oct) <<=? (Pitch pc Sharp oct) = True
    (Pitch pc Sharp oct) <<=? (Pitch pc acc oct) = False
    (Pitch pc Flat oct) <<=? (Pitch pc acc oct) = True
    (Pitch pc acc oct) <<=? (Pitch pc Flat oct) = False
    (Pitch E Sharp oct) <<=? (Pitch F Flat oct) = False
    (Pitch F Flat oct) <<=? (Pitch E Sharp oct) = True
    (Pitch B Sharp oct) <<=? (Pitch C Flat oct') =
            If (NextOct oct oct') False ((Pitch B Natural oct) <<=? (Pitch C Flat oct'))
    (Pitch C Flat oct) <<=? (Pitch B Sharp oct') =
            If (NextOct oct' oct) True ((Pitch C Natural oct) <<=? (Pitch B Sharp oct'))
    (Pitch pc1 acc1 oct) <<=? (Pitch pc2 acc2 oct) = ClassToNat pc1 <=? ClassToNat pc2
    (Pitch pc1 acc1 oct1) <<=? (Pitch pc2 acc2 oct2) = OctToNat oct1 <=? OctToNat oct2
    p1 <<=? p2 = PitchToNat p1 <=? PitchToNat p2

-- | Greater than for pitches.
type family (p1 :: PitchType) <<? (p2 :: PitchType) where
    p <<? p = False
    p1 <<? p2 = (p1 <<=? p2)

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

-- | Returns True if the successor of o1 is o2.
type family NextOct (o1 :: OctaveNum) (o2 :: OctaveNum) :: Bool where
    NextOct Oct_1 Oct0 = True
    NextOct Oct0 Oct1 = True
    NextOct Oct1 Oct2 = True
    NextOct Oct2 Oct3 = True
    NextOct Oct3 Oct4 = True
    NextOct Oct4 Oct5 = True
    NextOct Oct5 Oct6 = True
    NextOct Oct6 Oct7 = True
    NextOct Oct7 Oct8 = True
    NextOct _    _    = False

-- | Returns i if o2 is after o2, otherwise returns Compound.
type family IntervalOrCompound (o1 :: OctaveNum) (o2 :: OctaveNum) (i :: IntervalType)
            :: IntervalType where
    IntervalOrCompound o1 o2 int = If (NextOct o1 o2) int Compound

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
    HalfStepUp (Pitch E  Flat    o) = Pitch E Natural o
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

-- | Raise all pitches in a voice by an interval.
type family RaiseAllBy (ps :: Voice l) (i :: IntervalType) :: Voice l where
    RaiseAllBy End _ = End
    RaiseAllBy (p :* d :- ps) i = RaiseBy p i :* d :- RaiseAllBy ps i

-- | Raise multiple pitches by an interval.
type family RaiseAllBy' (ps :: Vector PitchType n) (i :: IntervalType) :: Vector PitchType n where
    RaiseAllBy' None _ = None
    RaiseAllBy' (p :-- ps) i = RaiseBy p i :-- RaiseAllBy' ps i

-- | Lower all pitches in a voice by an interval.
type family LowerAllBy (ps :: Voice l) (i :: IntervalType) :: Voice l where
    LowerAllBy End _ = End
    LowerAllBy (p :* d :- ps) i = LowerBy p i :* d :- LowerAllBy ps i

-- | Lower multiple pitches by an interval.
type family LowerAllBy' (ps :: Vector PitchType n) (i :: IntervalType) :: Vector PitchType n where
    LowerAllBy' None _ = None
    LowerAllBy' (p :-- ps) i = LowerBy p i :-- LowerAllBy' ps i

-- | Raise a pitch by an octave.
type family RaiseByOct (p :: PitchType) :: PitchType where
    RaiseByOct p = RaiseBy p (Interval Perf Octave)

-- | Lower a pitch by an octave.
type family LowerByOct (p :: PitchType) :: PitchType where
    LowerByOct p = LowerBy p (Interval Perf Octave)

type family RaiseAllByOct (ps :: Voice l) :: Voice l where
    RaiseAllByOct v = RaiseAllBy v (Interval Perf Octave)

-- | Transpose a partiture up by the given interval.
type family TransposeUpBy (p :: Partiture n l) (i :: IntervalType) :: Partiture n l where
    TransposeUpBy _ Compound = TypeError (Text "Can't transpose by compound interval.")
    TransposeUpBy None i = None
    TransposeUpBy (v :-- vs) i = RaiseAllBy v i :-- TransposeUpBy vs i

-- | Transpose a partiture down by the given interval.
type family TransposeDownBy (p :: Partiture n l) (i :: IntervalType) :: Partiture n l where
    TransposeDownBy _ Compound = TypeError (Text "Can't transpose by compound interval.")
    TransposeDownBy None i = None
    TransposeDownBy (v :-- vs) i = LowerAllBy v i :-- TransposeDownBy vs i

-------------------------------------------------------------------------------
-- Primitive instances
-------------------------------------------------------------------------------

instance Primitive Oct_1 where type Rep Oct_1 = Int ; prim o = 0 ; pretty o = "_5"
instance Primitive Oct0 where type Rep Oct0 = Int ; prim o = 12 ; pretty o = "_4"
instance Primitive Oct1 where type Rep Oct1 = Int ; prim o = 24 ; pretty o = "_3"
instance Primitive Oct2 where type Rep Oct2 = Int ; prim o = 36 ; pretty o = "__"
instance Primitive Oct3 where type Rep Oct3 = Int ; prim o = 48 ; pretty o = "_ "
instance Primitive Oct4 where type Rep Oct4 = Int ; prim o = 60 ; pretty o = "  "
instance Primitive Oct5 where type Rep Oct5 = Int ; prim o = 72 ; pretty o = "' "
instance Primitive Oct6 where type Rep Oct6 = Int ; prim o = 84 ; pretty o = "''"
instance Primitive Oct7 where type Rep Oct7 = Int ; prim o = 96 ; pretty o = "'3"
instance Primitive Oct8 where type Rep Oct8 = Int ; prim o = 108; pretty o = "'4"

instance Primitive C where type Rep C = Int ; prim p = 0 ; pretty p = "C"
instance Primitive D where type Rep D = Int ;  prim p = 2 ; pretty p = "D"
instance Primitive E where type Rep E = Int ;  prim p = 4 ; pretty p = "E"
instance Primitive F where type Rep F = Int ;  prim p = 5 ; pretty p = "F"
instance Primitive G where type Rep G = Int ;  prim p = 7 ; pretty p = "G"
instance Primitive A where type Rep A = Int ;  prim p = 9 ; pretty p = "A"
instance Primitive B where type Rep B = Int ;  prim p = 11; pretty p = "B"

instance Primitive Natural where type Rep Natural = Int ; prim a = 0 ; pretty a = " "
instance Primitive Flat where type Rep Flat = Int ; prim a = -1 ; pretty a = "b"
instance Primitive Sharp where type Rep Sharp = Int ; prim a = 1 ; pretty a = "#"

-- "Equality constraints are literally magic."
--                                  - Michael Gale, 2017
instance (IntRep pc, IntRep acc, IntRep oct)
        => Primitive (Pitch pc acc oct) where
    type Rep (Pitch pc acc oct) = Int
    prim p = prim (PC @pc) + prim (Acc @acc) + prim (Oct @oct)
    pretty p = pretty (PC @pc) ++ pretty (Acc @acc) ++ pretty (Oct @oct)

instance (IntRep sd, IntRep acc, IntRep oct) => Primitive (Degree sd acc oct) where
    type Rep (Degree sd acc oct) = Int
    prim _ = prim (ScaDeg @sd) + prim (Acc @acc) + prim (Oct @oct)
    pretty _ = pretty (ScaDeg @sd) ++ pretty (Acc @acc) ++ pretty (Oct @oct)

instance Primitive Silence where type Rep Silence = Int ; prim s = 60 ; pretty s = "~~~~"

instance IntRep p => Primitive (Root (PitchRoot p)) where
    type Rep (Root (PitchRoot p)) = Int
    prim r = prim (Pit @p)
    pretty r = pretty (Pit @p)

-- Modes
instance Primitive MajorMode where type Rep MajorMode = Bool ; prim m = True ; pretty m = "Major"
instance Primitive MinorMode where type Rep MinorMode = Bool ; prim m = False ; pretty m = "minor"

-- Scale degrees
instance Primitive I    where type Rep I    = Int ; prim d = 0 ; pretty d = "I"
instance Primitive II   where type Rep II   = Int ; prim d = 1 ; pretty d = "II"
instance Primitive III  where type Rep III  = Int ; prim d = 2 ; pretty d = "III"
instance Primitive IV   where type Rep IV   = Int ; prim d = 3 ; pretty d = "IV"
instance Primitive V    where type Rep V    = Int ; prim d = 4 ; pretty d = "V"
instance Primitive VI   where type Rep VI   = Int ; prim d = 5 ; pretty d = "VI"
instance Primitive VII  where type Rep VII  = Int ; prim d = 6 ; pretty d = "VII"


instance (IntRep pc, IntRep acc, BoolRep mo) => Primitive (Key pc acc mo) where
    type Rep (Key pc acc mo) = [Int]
    prim k = (+ (prim (PC @pc) + prim (Acc @acc))) <$> baseScale
        where baseScale = if (prim (Mod @ mo))
                            then [0, 2, 4, 5, 7, 9, 11]
                            else [0, 2, 3, 5, 7, 8, 10]
    pretty k = pretty (PC @pc) ++ pretty (Acc @acc) ++ " " ++ pretty (Mod @mo)

instance (IntRep p, RootToPitch (DegreeRoot k deg) ~ p, Primitive deg, Primitive k)
        => Primitive (DegreeRoot k deg) where
    type Rep (DegreeRoot k deg) = Int
    prim r = prim (Pit @p)
    pretty r = pretty (Deg @deg)

instance IntRep p => Primitive (PitchRoot p) where
    type Rep (PitchRoot p) = Int
    prim p = prim (Pit @p)
    pretty p = pretty (Pit @p)

instance KnownNat n => Primitive n where
    type Rep n = Int
    prim = fromInteger . natVal
    pretty (natVal -> 1) = "Th"
    pretty (natVal -> 2) = "Si"
    pretty (natVal -> 3) = "Si."
    pretty (natVal -> 4) = "Ei"
    pretty (natVal -> 6) = "Ei."
    pretty (natVal -> 8) = "Qu"
    pretty (natVal -> 12) = "Qu."
    pretty (natVal -> 16) = "Ha"
    pretty (natVal -> 24) = "Ha."
    pretty (natVal -> 32) = "Wh"
    pretty (natVal -> 48) = "Wh."
    pretty (natVal -> n) = ":" ++ show n

-- Intervals

---- Interval classes

instance Primitive Maj where
    type Rep Maj = Int -> Int
    prim _ = id
    pretty _ = "Maj"

instance Primitive Min where
    type Rep Min = Int -> Int
    prim _ = pred
    pretty _ = "Min"

instance Primitive Perf where
    type Rep Perf = Int -> Int
    prim _ = id
    pretty _ = "Perf"

instance Primitive Aug where
    type Rep Aug = Int -> Int
    prim _ = (+ 1)
    pretty _ = "Aug"

instance Primitive Dim where
    type Rep Dim = Int -> Int
    prim _ 2 = 0
    prim _ 4 = 2
    prim _ 5 = 4
    prim _ 7 = 6
    prim _ 9 = 7
    prim _ 11 = 9
    prim _ 12 = 11
    pretty _ = "Dim"

---- Interval sizes

instance Primitive Unison where
    type Rep Unison = Int
    prim _ = 0
    pretty _ = "1"

instance Primitive Second where
    type Rep Second = Int
    prim _ = 2
    pretty _ = "2"

instance Primitive Third where
    type Rep Third = Int
    prim _ = 4
    pretty _ = "3"

instance Primitive Fourth where
    type Rep Fourth = Int
    prim _ = 5
    pretty _ = "4"

instance Primitive Fifth where
    type Rep Fifth = Int
    prim _ = 7
    pretty _ = "5"

instance Primitive Sixth where
    type Rep Sixth = Int
    prim _ = 9
    pretty _ = "6"

instance Primitive Seventh where
    type Rep Seventh = Int
    prim _ = 11
    pretty _ = "7"

instance Primitive Octave where
    type Rep Octave = Int
    prim _ = 12
    pretty _ = "8"

instance (FunRep Int Int ic, IntRep is) => Primitive (Interval ic is) where
    type Rep (Interval ic is) = Int
    prim _ = prim (IC @ic) (prim (IS @ is))
    pretty _ = pretty (IC @ic) ++ " " ++ pretty (IS @is)
