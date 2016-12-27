{-# LANGUAGE  TypeInType, MultiParamTypeClasses, FlexibleInstances,
    UndecidableInstances, GADTs, TypeOperators, TypeFamilies #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Harmony
-- Description :  Mezzo harmonic model
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and type functions modelling classical harmony.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Harmony
    (
    -- * Consonant and dissonant intervals
      PerfConsonantInterval
    , ImperfConsonantInterval
    , DissonantInterval
    -- * Harmonic motion
    , DirectMotion
    , ContraryMotion
    , ObliqueMotion
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
-- Consonant and dissonant intervals
-------------------------------------------------------------------------------

-- | Classifies perfect consonant intervals:
--
--  * Perfect unisons
--  * Perfect fifths
--  * Perfect octaves
class PerfConsonantInterval (i :: IntervalType)
instance PerfConsonantInterval (Interval Perf Unison)
instance PerfConsonantInterval (Interval Perf Fifth)
instance PerfConsonantInterval (Interval Perf Octave)

-- | Classifies imperfect consonant intervals:
--
--  * Major and minor thirds
--  * Major and minor sixths
class ImperfConsonantInterval (i :: IntervalType)
instance ImperfConsonantInterval (Interval Maj Third)
instance ImperfConsonantInterval (Interval Min Third)
instance ImperfConsonantInterval (Interval Maj Sixth)
instance ImperfConsonantInterval (Interval Min Sixth)

-- | Classifies dissonant intervals:
--
--  * Perfect fourth (by common practice convention)
--  * Augmented and diminished intervals
--  * Second and seventh intervals
class DissonantInterval (i :: IntervalType)
instance DissonantInterval (Interval Perf Fourth)
instance DissonantInterval (Interval Aug is)
instance DissonantInterval (Interval Dim is)
instance DissonantInterval (Interval ic Second)
instance DissonantInterval (Interval ic Seventh)

-------------------------------------------------------------------------------
-- Harmonic motion
-------------------------------------------------------------------------------

-- | Ensures that direct motion is permitted between the two intervals.
class DirectMotion (i1 :: IntervalType) (i2 :: IntervalType)
instance {-# OVERLAPPING #-} TypeError (Text "Direct motion into a perfect unison is forbidden.")
                                => DirectMotion i1 (Interval Perf Unison)
instance {-# OVERLAPPING #-} TypeError (Text "Direct motion into a perfect fifth is forbidden.")
                                => DirectMotion i1 (Interval Perf Fifth)
instance {-# OVERLAPPING #-} TypeError (Text "Direct motion into a perfect octave is forbidden.")
                                => DirectMotion i1 (Interval Perf Octave)
instance {-# OVERLAPPABLE #-}      DirectMotion i1 i2

-- | Ensures that contrary motion is permitted between the two intervals.
class ContraryMotion (i1 :: IntervalType) (i2 :: IntervalType)
instance ContraryMotion i1 i2

-- | Ensures that oblique motion is permitted between the two intervals.
class ObliqueMotion (i1 :: IntervalType) (i2 :: IntervalType)
instance ObliqueMotion i1 i2

-------------------------------------------------------------------------------
-- Chords
-------------------------------------------------------------------------------

-- | The root of a chord.
type Root = PitchType

-- | The type of a triad.
data TriadType = MajTriad | MinTriad | AugTriad | DimTriad

-- | The type of a seventh chord.
data SeventhType = MajSeventh | MajMinSeventh | MinSeventh | HalfDimSeventh | DimSeventh

-- | The inversion of a chord.
data Inversion = NoInv | FirstInv | SecondInv | ThirdInv

-- | A chord type, indexed by the number of notes.
data ChordType :: Nat -> Type where
    Triad        :: Root -> TriadType   -> Inversion -> ChordType 3
    SeventhChord :: Root -> SeventhType -> Inversion -> ChordType 4

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
type family BuildOnRoot (p :: Root) (is :: Vector IntervalType n) :: Vector PitchType n where
    BuildOnRoot Silence _    = TypeError (Text "Can't build a chord on a rest.")
    BuildOnRoot p None       = None
    BuildOnRoot p (i :-- is) = RaiseBy p i :-- BuildOnRoot p is

-- | Convert a chord to a list of constituent pitches.
type family ChordToPitchList (c :: ChordType n) :: Vector PitchType n  where
    ChordToPitchList (Triad        r t i) = Invert i (BuildOnRoot r (TriadTypeToIntervals t))
    ChordToPitchList (SeventhChord r t i) = Invert i (BuildOnRoot r (SeventhTypeToIntervals t))

-- | Convert a chord to a partiture with the given length (one voice for each pitch).
type family ChordToPartiture (c :: ChordType n) (l :: Nat) :: Partiture n l where
    ChordToPartiture c l = VectorToColMatrix (ChordToPitchList c) l
