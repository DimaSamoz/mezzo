
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
      DyadType (..)
    , TriadType (..)
    , TetradType (..)
    , Inversion (..)
    , DyaType (..)
    , TriType (..)
    , TetType (..)
    , Inv (..)
    , InvertChord
    , ChordType (..)
    , Cho (..)
    , FromChord
    ) where

import GHC.TypeLits
import Data.Kind (Type)

import Mezzo.Model.Types
import Mezzo.Model.Prim
import Mezzo.Model.Reify

-------------------------------------------------------------------------------
-- Chords
-------------------------------------------------------------------------------

-- | The type of a dyad.
data DyadType = MinThird | MajThird | PerfFourth | PerfFifth | PerfOct

-- | The type of a triad.
data TriadType = MajTriad | MinTriad | AugTriad | DimTriad | DoubledD DyadType

-- | The type of a tetrad.
data TetradType = MajSeventh | MajMinSeventh | MinSeventh | HalfDimSeventh | DimSeventh | DoubledT TriadType

-- | The inversion of a chord.
data Inversion = Inv0 | Inv1 | Inv2 | Inv3

-- | A chord type, indexed by the number of notes.
data ChordType :: Nat -> Type where
    -- | A dyad, consisting of two pitches.
    Dyad   :: RootType -> DyadType -> Inversion -> ChordType 2
    -- | A triad, consisting of three pitches.
    Triad  :: RootType -> TriadType -> Inversion -> ChordType 3
    -- | A tetrad, consisting of four pitches.
    Tetrad :: RootType -> TetradType -> Inversion -> ChordType 4

-- | The singleton type for 'DyadType'
data DyaType (t :: DyadType) = DyaType

-- | The singleton type for 'TriadType'.
data TriType (t :: TriadType) = TriType

-- | The singleton type for 'TetradType'.
data TetType (t :: TetradType) = TetType

-- | The singleton type for 'Inversion'.
data Inv (t :: Inversion) = Inv

-- | The singleton type for 'ChordType'.
data Cho (c :: ChordType n) = Cho

-- | Convert a dyad type to a list of intervals between the individual pitches.
type family DyadTypeToIntervals (t :: DyadType) :: Vector IntervalType 2 where
    DyadTypeToIntervals MinThird =
                Interval Perf Unison :-- Interval Min Third   :-- None
    DyadTypeToIntervals MajThird =
                Interval Perf Unison :-- Interval Maj Third   :-- None
    DyadTypeToIntervals PerfFourth =
                Interval Perf Unison :-- Interval Perf Fourth :-- None
    DyadTypeToIntervals PerfFifth =
                Interval Perf Unison :-- Interval Perf Fifth  :-- None
    DyadTypeToIntervals PerfOct =
                Interval Perf Unison :-- Interval Perf Octave :-- None

-- | Convert a triad type to a list of intervals between the individual pitches.
type family TriadTypeToIntervals (t :: TriadType) :: Vector IntervalType 3 where
    TriadTypeToIntervals MajTriad      = DyadTypeToIntervals MajThird :-| Interval Perf Fifth
    TriadTypeToIntervals MinTriad      = DyadTypeToIntervals MinThird :-| Interval Perf Fifth
    TriadTypeToIntervals AugTriad      = DyadTypeToIntervals MajThird :-| Interval Aug Fifth
    TriadTypeToIntervals DimTriad      = DyadTypeToIntervals MinThird :-| Interval Dim Fifth
    TriadTypeToIntervals (DoubledD dt) = DyadTypeToIntervals dt       :-| Interval Perf Octave


-- | Convert a seventh chord type to a list of intervals between the individual pitches.
type family TetradTypeToIntervals (s :: TetradType) :: Vector IntervalType 4 where
    TetradTypeToIntervals MajSeventh     = TriadTypeToIntervals MajTriad :-| Interval Maj Seventh
    TetradTypeToIntervals MajMinSeventh  = TriadTypeToIntervals MajTriad :-| Interval Min Seventh
    TetradTypeToIntervals MinSeventh     = TriadTypeToIntervals MinTriad :-| Interval Min Seventh
    TetradTypeToIntervals HalfDimSeventh = TriadTypeToIntervals DimTriad :-| Interval Min Seventh
    TetradTypeToIntervals DimSeventh     = TriadTypeToIntervals DimTriad :-| Interval Dim Seventh
    TetradTypeToIntervals (DoubledT tt)  = TriadTypeToIntervals tt       :-| Interval Perf Octave

-- | Apply an inversion to a list of pitches.
type family Invert (i :: Inversion) (n :: Nat) (ps :: Vector PitchType n) :: Vector PitchType n where
    Invert Inv0 n ps         = ps
    -- Need awkward workarounds because of #12564.
    Invert Inv1 n (p :-- ps) = ps :-| RaiseByOct p
    Invert Inv2 n (p :-- ps) = Invert Inv1 (n - 1) (p :-- Tail' ps) :-| RaiseByOct (Head' ps)
    Invert Inv3 n (p :-- ps) = Invert Inv2 (n - 1) (p :-- (Head' ps) :-- (Tail' (Tail' (ps)))) :-| RaiseByOct (Head' (Tail' ps))

-- | Invert a doubled triad chord.
type family InvertDoubledD (i :: Inversion) (ps :: Vector PitchType 3) :: Vector PitchType 3 where
    InvertDoubledD Inv0 ps = ps
    InvertDoubledD Inv1 ps = Invert Inv1 2 (Init' ps) :-| (RaiseByOct (Head' (Tail' ps)))
    InvertDoubledD Inv2 ps = RaiseAllBy' ps (Interval Perf Octave)

-- | Invert a doubled triad chord.
type family InvertDoubledT (i :: Inversion) (ps :: Vector PitchType 4) :: Vector PitchType 4 where
    InvertDoubledT Inv0 ps = ps
    InvertDoubledT Inv1 ps = Invert Inv1 3 (Init' ps) :-| (RaiseByOct (Head' (Tail' ps)))
    InvertDoubledT Inv2 ps = Invert Inv2 3 (Init' ps) :-| (RaiseByOct (Head' (Tail' (Tail' ps))))
    InvertDoubledT Inv3 ps = RaiseAllBy' ps (Interval Perf Octave)

-- | Enumerate inversions.
type family InvSucc (i :: Inversion) :: Inversion where
    InvSucc Inv0 = Inv1
    InvSucc Inv1 = Inv2
    InvSucc Inv2 = Inv3
    InvSucc Inv3 = Inv0

-- | Invert a chord once.
type family InvertChord (c :: ChordType n) :: ChordType n where
    InvertChord (Dyad r t Inv0) = Dyad r t Inv1
    InvertChord (Dyad r t Inv1) = Dyad r t Inv0
    InvertChord (Triad r t Inv2) = Triad r t Inv0
    InvertChord (Triad r t i) = Triad r t (InvSucc i)
    InvertChord (Tetrad r t i) = Tetrad r t (InvSucc i)

-- | Build a list of pitches with the given intervals starting from a root.
type family BuildOnRoot (r :: RootType) (is :: Vector IntervalType n) :: Vector PitchType n where
    BuildOnRoot (PitchRoot Silence) _          = TypeError (Text "Can't build a chord on a rest.")
    BuildOnRoot r                   None       = None
    BuildOnRoot r                   (i :-- is) = RaiseBy (RootToPitch r) i :-- BuildOnRoot r is

-- | Convert a chord to a list of constituent pitches.
type family ChordToPitchList (c :: ChordType n) :: Vector PitchType n  where
    ChordToPitchList (Dyad r t i)
                    = Invert i 2 (BuildOnRoot r (DyadTypeToIntervals t))
    ChordToPitchList (Triad r (DoubledD dt) i)
                    = InvertDoubledD i (BuildOnRoot r (TriadTypeToIntervals (DoubledD dt)))
    ChordToPitchList (Triad r t i)
                    = Invert i 3 (BuildOnRoot r (TriadTypeToIntervals t))
    ChordToPitchList (Tetrad r (DoubledT tt) i)
                    = InvertDoubledT i (BuildOnRoot r (TetradTypeToIntervals (DoubledT tt)))
    ChordToPitchList (Tetrad r t i)
                    = Invert i 4 (BuildOnRoot r (TetradTypeToIntervals t))

-- | Convert a chord to a partiture with the given length (one voice for each pitch).
type family FromChord (c :: ChordType n) (l :: Nat) :: Partiture n l where
    FromChord (c :: ChordType n) l = VectorToColMatrix n (ChordToPitchList c) l

-------------------------------------------------------------------------------
-- Primitive instances
-------------------------------------------------------------------------------

-- Chord types



instance Primitive MajThird where
    type Rep MajThird = Int -> [Int]
    prim t = \r -> [r, r + 4]
    pretty t = "M3"

instance Primitive MinThird where
    type Rep MinThird = Int -> [Int]
    prim t = \r -> [r, r + 3]
    pretty t = "m3"

instance Primitive PerfFourth where
    type Rep PerfFourth = Int -> [Int]
    prim t = \r -> [r, r + 5]
    pretty t = "P4"

instance Primitive PerfFifth where
    type Rep PerfFifth = Int -> [Int]
    prim t = \r -> [r, r + 7]
    pretty t = "P5"

instance Primitive PerfOct where
    type Rep PerfOct = Int -> [Int]
    prim t = \r -> [r, r + 12]
    pretty t = "P8"


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

instance FunRep Int [Int] c => Primitive (DoubledD c) where
    type Rep (DoubledD c) = Int -> [Int]
    prim t = \r -> prim (DyaType @c) r ++ [r + 12]
    pretty t = pretty (DyaType @c) ++ "D"


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

instance FunRep Int [Int] c => Primitive (DoubledT c) where
    type Rep (DoubledT c) = Int -> [Int]
    prim t = \r -> prim (TriType @c) r ++ [r + 12]
    pretty t = pretty (TriType @c) ++ "D"

-- Inversions
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

instance (IntRep r, FunRep Int [Int] t, FunRep [Int] [Int] i)
        => Primitive (Dyad r t i) where
    type Rep (Dyad r t i) = [Int]
    prim c = prim (Inv @i) . prim (DyaType @t) $ prim (Root @r)
    pretty c = pc ++ " " ++ pretty (DyaType @t) ++ " " ++ pretty (Inv @i)
        where pc = takeWhile (\c -> c /= ' ' && c /= '\'' && c /= '_') $ pretty (Root @r)


instance (IntRep r, FunRep Int [Int] dt, FunRep [Int] [Int] i)
        => Primitive (Triad r (DoubledD dt) i) where
    type Rep (Triad r (DoubledD dt) i) = [Int]
    prim c = inverted ++ [head inverted + 12]
        where rootPos = prim (DyaType @dt) $ prim (Root @r)
              inverted = prim (Inv @i) rootPos
    pretty c = pc ++ " " ++ pretty (DyaType @dt) ++ "D " ++ pretty (Inv @i)
        where pc = takeWhile (\c -> c /= ' ' && c /= '\'' && c /= '_') $ pretty (Root @r)

instance {-# OVERLAPPABLE #-} (IntRep r, FunRep Int [Int] t, FunRep [Int] [Int] i)
        => Primitive (Triad r t i) where
    type Rep (Triad r t i) = [Int]
    prim c = prim (Inv @i) . prim (TriType @t) $ prim (Root @r)
    pretty c = pc ++ " " ++ pretty (TriType @t) ++ " " ++ pretty (Inv @i)
        where pc = takeWhile (\c -> c /= ' ' && c /= '\'' && c /= '_') $ pretty (Root @r)


instance (IntRep r, FunRep Int [Int] tt, FunRep [Int] [Int] i)
        => Primitive (Tetrad r (DoubledT tt) i) where
    type Rep (Tetrad r (DoubledT tt) i) = [Int]
    prim c = inverted ++ [head inverted + 12]
        where rootPos = prim (TriType @tt) $ prim (Root @r)
              inverted = prim (Inv @i) rootPos
    pretty c = pc ++ " " ++ pretty (TriType @tt) ++ "D " ++ pretty (Inv @i)
        where pc = takeWhile (\c -> c /= ' ' && c /= '\'' && c /= '_') $ pretty (Root @r)

instance {-# OVERLAPPABLE #-} (IntRep r, FunRep Int [Int] t, FunRep [Int] [Int] i)
        => Primitive (Tetrad r t i) where
    type Rep (Tetrad r t i) = [Int]
    prim c = prim (Inv @i) . prim (TetType @t) $ prim (Root @r)
    pretty c = pc ++ " " ++ pretty (TetType @t) ++ " " ++ pretty (Inv @i)
        where pc = takeWhile (\c -> c /= ' ' && c /= '\'' && c /= '_') $ pretty (Root @r)
