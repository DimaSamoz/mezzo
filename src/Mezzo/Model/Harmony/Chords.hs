{-# LANGUAGE  TypeInType, UndecidableInstances, GADTs, TypeOperators,
    TypeApplications, TypeFamilies, ScopedTypeVariables #-}
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
    -- * Chords
      TriadType (..)
    , SeventhType (..)
    , Inversion (..)
    , TriType (..)
    , SevType (..)
    , Inv (..)
    , InvertChord
    , ChordType (..)
    , Cho (..)
    , FromChord
    , ChordsToPartiture
    ) where

import GHC.TypeLits
import Data.Kind (Type)

import Mezzo.Model.Types
import Mezzo.Model.Prim
import Mezzo.Model.Reify

-------------------------------------------------------------------------------
-- Chords
-------------------------------------------------------------------------------

-- | The type of a triad.
data TriadType = MajTriad | MinTriad | AugTriad | DimTriad

-- | The type of a seventh chord.
data SeventhType = MajSeventh | MajMinSeventh | MinSeventh | HalfDimSeventh | DimSeventh | Doubled TriadType

-- | The inversion of a chord.
data Inversion = Inv0 | Inv1 | Inv2 | Inv3

-- | The singleton type for 'TriadType'.
data TriType (t :: TriadType) = TriType

-- | The singleton type for 'SeventhType'.
data SevType (t :: SeventhType) = SevType

-- | The singleton type for 'Inversion'.
data Inv (t :: Inversion) = Inv

-- | A chord type, indexed by the number of notes.
data ChordType :: Nat -> Type where
    Triad :: RootType -> TriadType   -> Inversion -> ChordType 3
    SeventhChord :: RootType -> SeventhType -> Inversion -> ChordType 4

data Cho (c :: ChordType n) = Cho

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
    SeventhTypeToIntervals (Doubled tt)   = TriadTypeToIntervals tt       :-| Interval Perf Octave

-- | Apply an inversion to a list of pitches.
type family Invert (i :: Inversion) (n :: Nat) (ps :: Vector PitchType n) :: Vector PitchType n where
    Invert Inv0 n ps         = ps
    -- Need awkward workarounds because of #12564.
    Invert Inv1 n (p :-- ps) = ps :-| RaiseByOct p
    Invert Inv2 n (p :-- ps) = Invert Inv1 (n - 1) (p :-- Tail' ps) :-| RaiseByOct (Head' ps)
    Invert Inv3 n (p :-- ps) = Invert Inv2 (n - 1) (p :-- (Head' (Tail' ps)) :-- (Tail' (Tail' (ps)))) :-| RaiseByOct (Head' ps)

-- | Invert a doubled triad chord.
type family InvertDoubled (i :: Inversion) (ps :: Vector PitchType 4) :: Vector PitchType 4 where
    InvertDoubled Inv3 ps = RaiseAllBy ps (Interval Perf Octave)
    InvertDoubled i ps = Invert i 3 (Init' ps) :-| (RaiseByOct (Head' (Invert i 3 (Init' ps))))

type family InvSucc (i :: Inversion) :: Inversion where
    InvSucc Inv0 = Inv1
    InvSucc Inv1 = Inv2
    InvSucc Inv2 = Inv3
    InvSucc Inv3 = Inv0

type family InvertChord (c :: ChordType n) :: ChordType n where
    InvertChord (Triad r t Inv2) = Triad r t Inv0
    InvertChord (Triad r t i) = Triad r t (InvSucc i)
    InvertChord (SeventhChord r t i) = SeventhChord r t (InvSucc i)

-- | Build a list of pitches with the given intervals starting from a root.
type family BuildOnRoot (r :: RootType) (is :: Vector IntervalType n) :: Vector PitchType n where
    BuildOnRoot (PitchRoot Silence) _    = TypeError (Text "Can't build a chord on a rest.")
    BuildOnRoot r None       = None
    BuildOnRoot r (i :-- is) = RaiseBy (RootToPitch r) i :-- BuildOnRoot r is

-- | Convert a chord to a list of constituent pitches.
type family ChordToPitchList (c :: ChordType n) :: Vector PitchType n  where
    ChordToPitchList (Triad        r t i) = Invert i 3 (BuildOnRoot r (TriadTypeToIntervals t))
    ChordToPitchList (SeventhChord r (Doubled tt) i)
                                          = InvertDoubled i (BuildOnRoot r (SeventhTypeToIntervals (Doubled tt)))
    ChordToPitchList (SeventhChord r t i) = Invert i 4 (BuildOnRoot r (SeventhTypeToIntervals t))

-- | Convert a chord to a partiture with the given length (one voice for each pitch).
type family FromChord (c :: ChordType n) (l :: Nat) :: Partiture n l where
    FromChord c l = VectorToColMatrix (ChordToPitchList c) l

type family ChordsToPartiture (v :: Vector (ChordType n) l) (d :: Nat) :: Partiture n (l * d) where
    ChordsToPartiture None l = None
    ChordsToPartiture (c :-- cs) d = FromChord c d +|+ ChordsToPartiture cs d

-------------------------------------------------------------------------------
-- Primitive instances
-------------------------------------------------------------------------------

-- Chord types
instance Primitive MajTriad where
    type Rep MajTriad = Int -> [Int]
    prim t = \r -> [r, r + 4, r + 7]
    pretty t = "Maj"

instance Primitive MinTriad where
    type Rep MinTriad = Int -> [Int]
    prim t = \r -> [r, r + 3, r + 7]
    pretty t = "min"

instance Primitive AugTriad where
    type Rep AugTriad = Int -> [Int]
    prim t = \r -> [r, r + 4, r + 8]
    pretty t = "aug"

instance Primitive DimTriad where
    type Rep DimTriad = Int -> [Int]
    prim t = \r -> [r, r + 3, r + 6]
    pretty t = "dim"

instance Primitive MajSeventh where
    type Rep MajSeventh = Int -> [Int]
    prim t = \r -> [r, r + 4, r + 7, r + 11]
    pretty t = "Maj7"

instance Primitive MajMinSeventh where
    type Rep MajMinSeventh = Int -> [Int]
    prim t = \r -> [r, r + 4, r + 7, r + 10]
    pretty t = "7"

instance Primitive MinSeventh where
    type Rep MinSeventh = Int -> [Int]
    prim t = \r -> [r, r + 3, r + 7, r + 10]
    pretty t = "min7"

instance Primitive HalfDimSeventh where
    type Rep HalfDimSeventh = Int -> [Int]
    prim t = \r -> [r, r + 3, r + 6, r + 10]
    pretty t = "hdim7"

instance Primitive DimSeventh where
    type Rep DimSeventh = Int -> [Int]
    prim t = \r -> [r, r + 3, r + 6, r + 9]
    pretty t = "dim7"

instance (Primitive c, Rep c ~ (Int -> [Int])) => Primitive (Doubled c) where
    type Rep (Doubled c) = Int -> [Int]
    prim t = \r -> prim (TriType @c) r ++ [r + 12]
    pretty t = pretty (TriType @c) ++ "D"

-- Inversions
-- No real need for applying inversions since harmonic composition is commutative,
-- but doesn't hurt

-- Places the first element of the list on its end.
invChord :: [Int] -> [Int]
invChord [] = []
invChord (x : xs) = xs ++ [x + 12]

instance Primitive Inv0 where
    type Rep Inv0 = [Int] -> [Int]
    prim i = id
    pretty i = "I0"

instance Primitive Inv1 where
    type Rep Inv1 = [Int] -> [Int]
    prim i = invChord
    pretty i = "I1"

instance Primitive Inv2 where
    type Rep Inv2 = [Int] -> [Int]
    prim i = invChord . invChord
    pretty i = "I2"

instance Primitive Inv3 where
    type Rep Inv3 = [Int] -> [Int]
    prim i = invChord . invChord . invChord
    pretty i = "I3"

instance ((Rep r) ~ Int, (Rep t) ~ (Int -> [Int]), (Rep i) ~ ([Int] -> [Int])
         , Primitive r, Primitive t, Primitive i)
        => Primitive (Triad r t i) where
    type Rep (Triad r t i) = [Int]
    prim c = prim (Inv @i) . prim (TriType @t) $ prim (Root @r)
    pretty c = pc ++ " " ++ pretty (TriType @t) ++ " " ++ pretty (Inv @i)
        where pc = takeWhile (\c -> c /= ' ' && c /= '\'' && c /= '_') $ pretty (Root @r)

instance ((Rep r) ~ Int, (Rep t) ~ (Int -> [Int]), (Rep i) ~ ([Int] -> [Int])
         , Primitive r, Primitive t, Primitive i)
        => Primitive (SeventhChord r t i) where
    type Rep (SeventhChord r t i) = [Int]
    prim c = prim (Inv @i) . prim (SevType @t) $ prim (Root @r)
    pretty c = pc ++ " " ++ pretty (SevType @t) ++ " " ++ pretty (Inv @i)
        where pc = takeWhile (\c -> c /= ' ' && c /= '\'' && c /= '_') $ pretty (Root @r)
