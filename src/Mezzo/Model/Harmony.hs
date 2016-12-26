{-# LANGUAGE  TypeInType, MultiParamTypeClasses, FlexibleInstances,
    UndecidableInstances, GADTs, TypeOperators, TypeFamilies #-}

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
    , Chord (..)
    , ChordToPitchList
    ) where

import GHC.TypeLits

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

-- | A chord type.
data Chord where
    Triad        :: Root -> TriadType   -> Inversion -> Chord
    SeventhChord :: Root -> SeventhType -> Inversion -> Chord

-- | Convert a triad type to a list of intervals between the individual pitches.
type family TriadTypeToIntervals (t :: TriadType) :: [IntervalType] where
    TriadTypeToIntervals MajTriad = '[Interval Maj Third, Interval Min Third]
    TriadTypeToIntervals MinTriad = '[Interval Min Third, Interval Maj Third]
    TriadTypeToIntervals AugTriad = '[Interval Maj Third, Interval Maj Third]
    TriadTypeToIntervals DimTriad = '[Interval Min Third, Interval Min Third]

-- | Convert a seventh chord type to a list of intervals between the individual pitches.
type family SeventhTypeToIntervals (s :: SeventhType) :: [IntervalType] where
    SeventhTypeToIntervals MajSeventh     = TriadTypeToIntervals MajTriad <.> Interval Maj Third
    SeventhTypeToIntervals MajMinSeventh  = TriadTypeToIntervals MajTriad <.> Interval Min Third
    SeventhTypeToIntervals MinSeventh     = TriadTypeToIntervals MinTriad <.> Interval Min Third
    SeventhTypeToIntervals HalfDimSeventh = TriadTypeToIntervals DimTriad <.> Interval Maj Third
    SeventhTypeToIntervals DimSeventh     = TriadTypeToIntervals DimTriad <.> Interval Min Third

-- | Apply an inversion to a list of pitches.
type family Invert (i :: Inversion) (ps :: [PitchType]) :: [PitchType] where
    Invert NoInv     ps                  = ps
    Invert FirstInv  (p1 : ps)           = ps                              <.> RaiseByOct p1
    Invert SecondInv (p1 : p2 : ps)      = Invert FirstInv  (p1 : ps)      <.> RaiseByOct p2
    Invert ThirdInv  (p1 : p2 : p3 : ps) = Invert SecondInv (p1 : p2 : ps) <.> RaiseByOct p3

-- | Build a list of pitches with the given intervals starting from a root.
type family BuildOnRoot (p :: Root) (is :: [IntervalType]) :: [PitchType] where
    BuildOnRoot Silence _  = TypeError (Text "Can't build a chord on a rest.")
    BuildOnRoot p '[]      = '[p]
    BuildOnRoot p (i : is) = p : BuildOnRoot (RaiseBy p i) is

-- | Convert a chord to a list of constituent pitches.
type family ChordToPitchList (c :: Chord) :: [PitchType] where
    ChordToPitchList (Triad        r t i) = Invert i (BuildOnRoot r (TriadTypeToIntervals t))
    ChordToPitchList (SeventhChord r t i) = Invert i (BuildOnRoot r (SeventhTypeToIntervals t))
