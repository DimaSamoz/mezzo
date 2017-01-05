{-# LANGUAGE  TypeInType, MultiParamTypeClasses, FlexibleInstances,
    UndecidableInstances, GADTs, TypeOperators, TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Harmony.Functional
-- Description :  Models of functional harmony
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and type functions modelling principles of functional harmony.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Harmony.Functional
    (
      Quality (..)
    , Degree (..)
    , Piece (..)
    , Phrase (..)
    , Cadence (..)
    , Tonic (..)
    , Dominant (..)
    , Subdominant (..)
    )
    where

import GHC.TypeLits
import Data.Kind

import Mezzo.Model.Types hiding (IntervalClass (..))
import Mezzo.Model.Prim
import Mezzo.Model.Harmony.Chords

-- | The quality of a scale degree chord.
data Quality = MajQ | MinQ | DomQ | DimQ

-- | A scale degree chord in given key, on the given scale, with the given quality.
data Degree (d :: ScaleDegree) (q :: Quality) (k :: KeyType) where
    DegChord :: Degree d q k

-- | Ensure that the degree and quality match the mode.
class DiatonicDegree (d :: ScaleDegree) (q :: Quality) (k :: KeyType)
instance MajDegQuality d q => DiatonicDegree d q (Key pc acc MajorMode)
instance MinDegQuality d q => DiatonicDegree d q (Key pc acc MinorMode)

-- | Ensure that the degree and quality are valid in major mode.
class MajDegQuality (d :: ScaleDegree) (q :: Quality)
instance {-# OVERLAPPING #-} MajDegQuality I MajQ
instance {-# OVERLAPPING #-} MajDegQuality II MinQ
instance {-# OVERLAPPING #-} MajDegQuality II DomQ  -- Secondary dominant
instance {-# OVERLAPPING #-} MajDegQuality III MinQ
instance {-# OVERLAPPING #-} MajDegQuality IV MajQ
instance {-# OVERLAPPING #-} MajDegQuality V MajQ
instance {-# OVERLAPPING #-} MajDegQuality V DomQ
instance {-# OVERLAPPING #-} MajDegQuality VI MinQ
instance {-# OVERLAPPING #-} MajDegQuality VII DimQ
instance {-# OVERLAPPABLE #-} TypeError (Text "Can't have a "
                                    :<>: ShowQual q
                                    :<>: ShowDeg d
                                    :<>: Text " degree chord in major mode.")
                                => MajDegQuality d q

-- | Ensure that the degree and quality are valid in minor mode.
class MinDegQuality (d :: ScaleDegree) (q :: Quality)
instance {-# OVERLAPPING #-} MinDegQuality I MinQ
instance {-# OVERLAPPING #-} MinDegQuality II DimQ
instance {-# OVERLAPPING #-} MinDegQuality II DomQ  -- Secondary dominant
instance {-# OVERLAPPING #-} MinDegQuality III MajQ
instance {-# OVERLAPPING #-} MinDegQuality IV MinQ
instance {-# OVERLAPPING #-} MinDegQuality V MajQ
instance {-# OVERLAPPING #-} MinDegQuality V DomQ
instance {-# OVERLAPPING #-} MinDegQuality VI MajQ
instance {-# OVERLAPPING #-} MinDegQuality VII MajQ
instance {-# OVERLAPPABLE #-} TypeError (Text "Can't have a "
                                    :<>: ShowType q :<>: Text " "
                                    :<>: ShowType d
                                    :<>: Text " degree chord in minor mode.")
                                => MinDegQuality d q

-- | A functionally described piece of music, built from multiple phrases.
data Piece (k :: KeyType) (l :: Nat) where
    Cad :: Cadence k l -> Piece k l
    (:~) :: Phrase k l -> Piece k (n - l) -> Piece k n

-- | A phrase matching a specific functional progression.
data Phrase (k :: KeyType) (l :: Nat) where
    -- | A tonic-dominant-tonic progression.
    PhraseIVI :: Tonic k l1 -> Dominant k (l2 - l1) -> Tonic k (l - l2) -> Phrase k l
    -- | A dominant-tonic progression.
    PhraseVI  :: Dominant k l1 -> Tonic k (l - l1) -> Phrase k l

-- | A cadence in a specific key with a specific length.
data Cadence (k :: KeyType) (l :: Nat) where
    -- | Authentic cadence with major fifth chord.
    AuthCad    :: Degree V MajQ k -> Degree I q k -> Cadence k 2
    -- | Authentic cadence with dominant seventh fifth chord.
    AuthCad7   :: Degree V DomQ k -> Degree I q k  -> Cadence k 2
    -- | Authentic cadence with diminished seventh chord.
    AuthCadVii :: Degree VII DimQ k -> Degree I q k -> Cadence k 2
    -- | Half cadence ending with a major fifth chord.
    HalfCad    :: Degree d q k -> Degree V MajQ k -> Cadence k 2
    -- | Deceptive cadence from a dominant fifth to a sixth.
    DeceptCad  :: Degree V DomQ k -> Degree VI q k -> Cadence k 2

-- | A tonic chord.
data Tonic (k :: KeyType) (l :: Nat) where
    -- | A major tonic chord.
    TonMaj :: Degree I MajQ k -> Tonic k 1
    -- | A minor tonic chord.
    TonMin :: Degree I MinQ k -> Tonic k 1

-- | A dominant chord progression.
data Dominant (k :: KeyType) (l :: Nat) where
    -- | Major fifth dominant.
    DomVM   :: Degree V MajQ  k  -> Dominant k 1
    -- | Seventh chord fifth degree dominant.
    DomV7   :: Degree V DomQ k  -> Dominant k 1
    -- | Diminished seventh degree dominant.
    DomVii0 :: Degree VII DimQ k -> Dominant k 1
    -- | Subdominant followed by dominant.
    DomSD   :: Subdominant k l1 -> Dominant k (l - l1) -> Dominant k l
    -- | Secondary dominant followed by dominant.
    DomSecD :: Degree II DomQ k -> Degree V DomQ k -> Dominant k 2

-- | A subdominant chord progression.
data Subdominant (k :: KeyType) (l :: Nat) where
    -- | Minor second subdominant.
    SubIIm     :: Degree II MinQ k -> Subdominant k 1
    -- | Major fourth subdominant.
    SubIVM     :: Degree IV MajQ k -> Subdominant k 1
    -- | Minor third followed by major fourth subdominant
    SubIIImIVM :: Degree III MinQ k -> Degree IV MajQ k -> Subdominant k 2
    -- | Minor fourth dominant.
    SubIVm     :: Degree IV MinQ k -> Subdominant k 1

-- | Convert a scale degree to a chord.
type family DegToChord (d :: Degree d q k)  :: ChordType 4 where
    DegToChord (DegChord :: Degree d q k) = SeventhChord (DegreeRoot k d) (QualToType q) NoInv

-- | Convert a quality to a seventh chord type.
type family QualToType (q :: Quality) :: SeventhType where
    QualToType MajQ = Doubled MajTriad
    QualToType MinQ = Doubled MinTriad
    QualToType DomQ = MajMinSeventh
    QualToType DimQ = DimSeventh

-- | Convert a cadence to chords.
type family CadToChords (c :: Cadence k l) :: Vector (ChordType 4) l where
    CadToChords (AuthCad  d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None
    CadToChords (AuthCad7 d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None
    CadToChords (AuthCadVii d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None
    CadToChords (HalfCad d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None
    CadToChords (DeceptCad d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None

-- | Convert a tonic to chords.
type family TonToChords (t :: Tonic k l) :: Vector (ChordType 4) l where
    TonToChords (TonMaj d) = DegToChord d :-- None
    TonToChords (TonMin d) = DegToChord d :-- None

-- | Convert a dominant to chords.
type family DomToChords (t :: Dominant k l) :: Vector (ChordType 4) l where
    DomToChords (DomVM d) = DegToChord d :-- None
    DomToChords (DomV7 d) = DegToChord d :-- None
    DomToChords (DomVii0 d) = DegToChord d :-- None
    DomToChords (DomSD s d) = SubdomToChords s ++. DomToChords d
    DomToChords (DomSecD d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None

-- | Convert a subdominant to chords.
type family SubdomToChords (t :: Subdominant k l) :: Vector (ChordType 4) l where
    SubdomToChords (SubIIm d) = DegToChord d :-- None
    SubdomToChords (SubIVM d) = DegToChord d :-- None
    SubdomToChords (SubIIImIVM d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None
    SubdomToChords (SubIVm d) = DegToChord d :-- None

-- | Convert a phrase to chords.
type family PhraseToChords (p :: Phrase k l) :: Vector (ChordType 4) l where
    PhraseToChords (PhraseIVI t1 d t2) = TonToChords t1 ++. DomToChords d ++. TonToChords t2
    PhraseToChords (PhraseVI d t) = DomToChords d ++. TonToChords t

-- | Convert a piece to chords.
type family PieceToChords (p :: Piece k l) :: Vector (ChordType 4) l where
    PieceToChords (Cad c) = CadToChords c
    PieceToChords (p :~ ps) = PhraseToChords p ++. PieceToChords ps

-- | Convert a quality to text.
type family ShowQual (q :: Quality) :: ErrorMessage where
    ShowQual MajQ = Text "major "
    ShowQual MinQ = Text "minor "
    ShowQual DomQ = Text "dominant "
    ShowQual DimQ = Text "diminished "

-- | Convert a degree to text.
type family ShowDeg (d :: ScaleDegree) :: ErrorMessage where
    ShowDeg I = Text "1st"
    ShowDeg II = Text "2nd"
    ShowDeg III = Text "3rd"
    ShowDeg IV = Text "4th"
    ShowDeg V = Text "5th"
    ShowDeg VI = Text "6th"
    ShowDeg VII = Text "7th"
