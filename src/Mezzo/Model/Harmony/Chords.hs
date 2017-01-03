{-# LANGUAGE  TypeInType, UndecidableInstances, GADTs, TypeOperators, TypeFamilies #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Harmony.Chords
-- Description :  Models of chords
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and type functions modelling harmonic chords.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Harmony.Chords
    (
    -- * Harmonic types
      KeyType (..)
    , Mode (..)
    , ScaleDegree (..)
    , RootType (..)
    , Root (..)
    -- * Chords
    , TriadType (..)
    , SeventhType (..)
    , Inversion (..)
    , ChordType (..)
    , ChordToPartiture
    ) where

import GHC.TypeLits
import Data.Kind

import Mezzo.Model.Types
import Mezzo.Model.Prim

-------------------------------------------------------------------------------
-- Harmonic types
-------------------------------------------------------------------------------

-- | The of a scale, chord or piece.
data KeyType = Key PitchClass Accidental Mode

-- | The mode of a key: major or minor.
data Mode = MajorMode | MinorMode

-- | The seven scale degrees.
data ScaleDegree = I | II | III | IV | V | VI | VII

-- | The type of a root.
data RootType = Diatonic -- ^ A root given by a specific musical pitch.
              | Scalar   -- ^ A root given by a scale degree in a specific key.

-- | The root of a chord.
data Root (rt :: RootType) where
    -- | A pitch constructs a diatonic root.
    PitchRoot :: PitchType -> Root Diatonic
    -- | A key and a scale degree constructs a scalar root.
    DegreeRoot :: KeyType -> ScaleDegree -> Root Scalar

-- | Convert a root to a pitch.
--
-- Note: the default octave for scalar roots is 'Oct2'.
type family RootToPitch (dr :: Root rt) :: PitchType where
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

-------------------------------------------------------------------------------
-- Chords
-------------------------------------------------------------------------------

-- | The type of a triad.
data TriadType = MajTriad | MinTriad | AugTriad | DimTriad

-- | The type of a seventh chord.
data SeventhType = MajSeventh | MajMinSeventh | MinSeventh | HalfDimSeventh | DimSeventh

-- | The inversion of a chord.
data Inversion = NoInv | FirstInv | SecondInv | ThirdInv

-- | A chord type, indexed by the number of notes.
data ChordType :: Nat -> Type where
    Triad        :: Root t -> TriadType   -> Inversion -> ChordType 3
    SeventhChord :: Root t -> SeventhType -> Inversion -> ChordType 4

-- | Convert a triad type to a list of intervals between the individual pitches.
type family TriadTypeToIntervals (t :: TriadType) :: Vector IntervalType 3 where
    TriadTypeToIntervals MajTriad =
                Interval Perf Unison :-- Interval Maj Third :-- Interval Perf Fifth :-- None
    TriadTypeToIntervals MinTriad =
                Interval Perf Unison :-- Interval Min Third :-- Interval Perf Fifth :-- None
    TriadTypeToIntervals AugTriad =
                Interval Perf Unison :-- Interval Maj Third :-- Interval Aug Fifth  :-- None
    TriadTypeToIntervals DimTriad =
                Interval Perf Unison :-- Interval Min Third :-- Interval Dim Fifth  :-- None

-- | Convert a seventh chord type to a list of intervals between the individual pitches.
type family SeventhTypeToIntervals (s :: SeventhType) :: Vector IntervalType 4 where
    SeventhTypeToIntervals MajSeventh     = TriadTypeToIntervals MajTriad :-| Interval Maj Seventh
    SeventhTypeToIntervals MajMinSeventh  = TriadTypeToIntervals MajTriad :-| Interval Min Seventh
    SeventhTypeToIntervals MinSeventh     = TriadTypeToIntervals MinTriad :-| Interval Min Seventh
    SeventhTypeToIntervals HalfDimSeventh = TriadTypeToIntervals DimTriad :-| Interval Min Seventh
    SeventhTypeToIntervals DimSeventh     = TriadTypeToIntervals DimTriad :-| Interval Dim Seventh

-- | Apply an inversion to a list of pitches.
type family Invert (i :: Inversion) (ps :: Vector PitchType n) :: Vector PitchType n where
    Invert NoInv     ps         = ps
    -- Need awkward workarounds because of #12564.
    Invert FirstInv  (p :-- ps) = ps :-| RaiseByOct p
    Invert SecondInv (p :-- ps) = Invert FirstInv (p :-- Tail' ps) :-| RaiseByOct (Head' ps)
    Invert ThirdInv  (p :-- ps) = Invert SecondInv (p :-- (Head' (Tail' ps)) :-- (Tail' (Tail' (ps)))) :-| RaiseByOct (Head' ps)

-- | Build a list of pitches with the given intervals starting from a root.
type family BuildOnRoot (r :: Root t) (is :: Vector IntervalType n) :: Vector PitchType n where
    BuildOnRoot (PitchRoot Silence) _    = TypeError (Text "Can't build a chord on a rest.")
    BuildOnRoot r None       = None
    BuildOnRoot r (i :-- is) = RaiseBy (RootToPitch r) i :-- BuildOnRoot r is

-- | Convert a chord to a list of constituent pitches.
type family ChordToPitchList (c :: ChordType n) :: Vector PitchType n  where
    ChordToPitchList (Triad        r t i) = Invert i (BuildOnRoot r (TriadTypeToIntervals t))
    ChordToPitchList (SeventhChord r t i) = Invert i (BuildOnRoot r (SeventhTypeToIntervals t))

-- | Convert a chord to a partiture with the given length (one voice for each pitch).
type family ChordToPartiture (c :: ChordType n) (l :: Nat) :: Partiture n l where
    ChordToPartiture c l = VectorToColMatrix (ChordToPitchList c) l
