{-# LANGUAGE  TypeInType, MultiParamTypeClasses, FlexibleInstances, TypeApplications, ScopedTypeVariables,
    UndecidableInstances, GADTs, TypeOperators, TypeFamilies, FlexibleContexts, ViewPatterns #-}
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
    , ProgType (..)
    , Phrase (..)
    , Cadence (..)
    , Tonic (..)
    , Dominant (..)
    , Subdominant (..)
    , TimeSignature
    , TimeSig (..)
    , ChordsToPartiture
    , ProgTypeToChords
    , FromProg
    , Prog (..)
    , Ton (..)
    , Dom (..)
    , Sub (..)
    , Cade (..)
    , Phr (..)
    )
    where

import GHC.TypeLits
import Data.Kind (Type)

import Mezzo.Model.Reify
import Mezzo.Model.Types hiding (IntervalClass (..))
import Mezzo.Model.Prim
import Mezzo.Model.Harmony.Chords

-- | The quality of a scale degree chord.
data Quality = MajQ | MinQ | DomQ | DimQ

-- | A scale degree chord in given key, on the given scale, with the given quality.
data Degree (d :: ScaleDegree) (q :: Quality) (k :: KeyType) (i :: Inversion) where
    DegChord :: Degree d q k i

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
data ProgType (k :: KeyType) (l :: Nat) where
    Cad :: Cadence k l -> ProgType k l
    (:=) :: Phrase k l -> ProgType k (n - l) -> ProgType k n

data Prog (p :: ProgType k l) = Prog

-- | A phrase matching a specific functional progression.
data Phrase (k :: KeyType) (l :: Nat) where
    -- | A tonic-dominant-tonic progression.
    PhraseIVI :: Tonic k (l2 - l1) -> Dominant k l1 -> Tonic k (l - l2) -> Phrase k l
    -- | A dominant-tonic progression.
    PhraseVI  :: Dominant k l1 -> Tonic k (l - l1) -> Phrase k l

-- | A cadence in a specific key with a specific length.
data Cadence (k :: KeyType) (l :: Nat) where
    -- | Authentic cadence with major fifth chord.
    AuthCad    :: Degree V MajQ k Inv1 -> Degree I q k Inv0 -> Cadence k 2
    -- | Authentic cadence with dominant seventh fifth chord.
    AuthCad7   :: Degree V DomQ k Inv2 -> Degree I q k Inv0 -> Cadence k 2
    -- | Authentic cadence with diminished seventh chord.
    AuthCadVii :: Degree VII DimQ k Inv0 -> Degree I q k Inv0 -> Cadence k 2
    -- | Authentic cadence with a cadential 6-4 chord
    AuthCad64  :: Degree I MajQ k Inv2 -> Degree V DomQ k Inv3 -> Degree I MajQ k Inv1 -> Cadence k 3
    -- | Half cadence ending with a major fifth chord.
    HalfCad    :: Degree d q k i -> Degree V MajQ k Inv0 -> Cadence k 2
    -- | Deceptive cadence from a dominant fifth to a sixth.
    DeceptCad  :: Degree V DomQ k Inv0 -> Degree VI q k Inv2 -> Cadence k 2

-- | A tonic chord.
data Tonic (k :: KeyType) (l :: Nat) where
    -- | A major tonic chord.
    TonMaj :: Degree I MajQ k Inv0 -> Tonic k 1 -- Temporarily (?) allow only no inversion
    -- | A minor tonic chord.
    TonMin :: Degree I MinQ k Inv0 -> Tonic k 1

class NotInv2 (i :: Inversion)
instance NotInv2 Inv0
instance TypeError (Text "Can't have a tonic in second inversion.") => NotInv2 Inv2
instance NotInv2 Inv1
instance NotInv2 Inv3

-- | A dominant chord progression.
data Dominant (k :: KeyType) (l :: Nat) where
    -- | Major fifth dominant.
    DomVM   :: Degree V MajQ k i -> Dominant k 1
    -- | Seventh chord fifth degree dominant.
    DomV7   :: Degree V DomQ k i -> Dominant k 1
    -- | Diminished seventh degree dominant.
    DomVii0 :: Degree VII DimQ k i -> Dominant k 1
    -- | Subdominant followed by dominant.
    DomSD   :: Subdominant k l1 -> Dominant k (l - l1) -> Dominant k l
    -- | Secondary dominant followed by dominant.
    DomSecD :: Degree II DomQ k Inv0 -> Degree V DomQ k Inv2 -> Dominant k 2

-- | A subdominant chord progression.
data Subdominant (k :: KeyType) (l :: Nat) where
    -- | Minor second subdominant.
    SubIIm     :: Degree II MinQ k i -> Subdominant k 1
    -- | Major fourth subdominant.
    SubIVM     :: Degree IV MajQ k i -> Subdominant k 1
    -- | Minor third followed by major fourth subdominant
    SubIIImIVM :: Degree III MinQ k i1 -> Degree IV MajQ k i2 -> Subdominant k 2
    -- | Minor fourth dominant.
    SubIVm     :: Degree IV MinQ k i -> Subdominant k 1

-- | Convert a scale degree to a chord.
type family DegToChord (d :: Degree d q k i) :: ChordType 4 where
    DegToChord (DegChord :: Degree d q k i) = SeventhChord (DegreeRoot k d) (QualToType q) i

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
    CadToChords (AuthCad64 d1 d2 d3) = DegToChord d1 :-- DegToChord d2 :-- DegToChord d3 :-- None
    CadToChords (HalfCad d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None
    CadToChords (DeceptCad d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None

-- | Convert a tonic to chords.
type family TonToChords (t :: Tonic k l) :: Vector (ChordType 4) l where
    TonToChords (TonMaj d) = DegToChord d :-- None
    TonToChords (TonMin d) = DegToChord d :-- None

-- | Convert a dominant to chords.
type family DomToChords (l :: Nat) (t :: Dominant k l) :: Vector (ChordType 4) l where
    DomToChords 1 (DomVM d) = DegToChord d :-- None
    DomToChords 1 (DomV7 d) = DegToChord d :-- None
    DomToChords 1 (DomVii0 d) = DegToChord d :-- None
    DomToChords l (DomSD (s :: Subdominant k l1) d) =
                    SubdomToChords s ++. DomToChords (l - l1) d
    DomToChords 2 (DomSecD d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None

-- | Convert a subdominant to chords.
type family SubdomToChords (t :: Subdominant k l) :: Vector (ChordType 4) l where
    SubdomToChords (SubIIm d) = DegToChord d :-- None
    SubdomToChords (SubIVM d) = DegToChord d :-- None
    SubdomToChords (SubIIImIVM d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None
    SubdomToChords (SubIVm d) = DegToChord d :-- None

-- | Convert a phrase to chords.
type family PhraseToChords (l :: Nat) (p :: Phrase k l) :: Vector (ChordType 4) l where
    PhraseToChords l (PhraseIVI t1 (d :: Dominant k dl) t2) = TonToChords t1 ++. DomToChords dl d ++. TonToChords t2
    PhraseToChords l (PhraseVI (d :: Dominant k dl) t) = DomToChords dl d ++. TonToChords t

-- | Convert a piece to chords.
type family ProgTypeToChords (l :: Nat) (p :: ProgType k l) :: Vector (ChordType 4) l where
    ProgTypeToChords l (Cad (c :: Cadence k l)) = CadToChords c
    ProgTypeToChords l ((p :: Phrase k l1) := ps) = PhraseToChords l1 p ++. ProgTypeToChords (l - l1) ps

-- | The number of beats in a bar.
type TimeSignature = Nat

-- | Singleton for 'TimeSignature'.
data TimeSig (t :: TimeSignature) = TimeSig

-- | Convert a vector of chords ("chord progression") into a 'Partiture'.
type family ChordsToPartiture (v :: Vector (ChordType n) l) (t :: TimeSignature) :: Partiture n (l * t * 8) where
    ChordsToPartiture None l = None
    ChordsToPartiture (c :-- cs) l = FromChord c (l * 8) +|+ ChordsToPartiture cs l

-- | Convert a progression with a time signature into a partiture.
type family FromProg (p :: ProgType k l) (t :: TimeSignature) :: Partiture 4 (l * t * 8) where
    FromProg (p :: ProgType k l) t = ChordsToPartiture (ProgTypeToChords l p) t

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

-- Singletons

data Ton (t :: Tonic k d) = Ton
data Dom (d :: Dominant k d) = Dom
data Sub (s :: Subdominant k d) = Sub

data Cade (c :: Cadence k d) = Cade
data Phr (p :: Phrase k d) = Phr

-- Tonic

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (TonMaj d) where
    type Rep (TonMaj d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Ton Maj"

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (TonMin d) where
    type Rep (TonMin d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Ton Min"

-- Dominant

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (DomVM d) where
    type Rep (DomVM d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Dom Maj"

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (DomV7 d) where
    type Rep (DomV7 d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Dom Maj7"

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (DomVii0 d) where
    type Rep (DomVii0 d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Dom VII0"

instance (IntLListRep sd, IntLListRep d) => Primitive (DomSD (sd :: Subdominant k sdur) (d :: Dominant k (l - sdur)) :: Dominant k l) where
    type Rep (DomSD sd d) = [[Int]]
    prim _ = prim (Sub @k @sdur @sd) ++ prim (Dom @k @(l - sdur) @d)
    pretty _ = pretty (Sub @k @sdur @sd) ++ " | " ++ pretty (Dom @k @(l - sdur) @d)

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (DomSecD d) where
    type Rep (DomSecD d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Dom SecD"

-- Subdominant

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (SubIIm d) where
    type Rep (SubIIm d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Sub ii"

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (SubIVM d) where
    type Rep (SubIVM d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Sub IV"

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2) => Primitive (SubIIImIVM d1 d2) where
    type Rep (SubIIImIVM d1 d2) = [[Int]]
    prim _ = [prim (Cho @4 @ch1), prim (Cho @4 @ch2)]
    pretty _ = "Sub iii IV"

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (SubIVm d) where
    type Rep (SubIVm d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Sub iv"

-- Cadences

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2) => Primitive (AuthCad d1 d2) where
    type Rep (AuthCad d1 d2) = [[Int]]
    prim _ = [prim (Cho @4 @ch1), prim (Cho @4 @ch2)]
    pretty _ = "AuthCad"

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2) => Primitive (AuthCad7 d1 d2) where
    type Rep (AuthCad7 d1 d2) = [[Int]]
    prim _ = [prim (Cho @4 @ch1), prim (Cho @4 @ch2)]
    pretty _ = "AuthCad V7"

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2) => Primitive (AuthCadVii d1 d2) where
    type Rep (AuthCadVii d1 d2) = [[Int]]
    prim _ = [prim (Cho @4 @ch1), prim (Cho @4 @ch2)]
    pretty _ = "AuthCad vii"

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2, ch3 ~ DegToChord d3, IntListRep ch3) => Primitive (AuthCad64 d1 d2 d3) where
    type Rep (AuthCad64 d1 d2 d3) = [[Int]]
    prim _ = [prim (Cho @4 @ch1), prim (Cho @4 @ch2), prim (Cho @4 @ch3)]
    pretty _ = "AuthCad 6-4"

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2) => Primitive (HalfCad d1 d2) where
    type Rep (HalfCad d1 d2) = [[Int]]
    prim _ = [prim (Cho @4 @ch1), prim (Cho @4 @ch2)]
    pretty _ = "HalfCad"

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2) => Primitive (DeceptCad d1 d2) where
    type Rep (DeceptCad d1 d2) = [[Int]]
    prim _ = [prim (Cho @4 @ch1), prim (Cho @4 @ch2)]
    pretty _ = "DeceptCad"

-- Phrases

instance (IntLListRep t1, IntLListRep d, IntLListRep t2) => Primitive (PhraseIVI (t1 :: Tonic k (l2 - l1)) (d :: Dominant k l1) (t2 :: Tonic k (l - l2)) :: Phrase k l) where
    type Rep (PhraseIVI t1 d t2) = [[Int]]
    prim _ = prim (Ton @k @(l2 - l1) @t1) ++ prim (Dom @k @l1 @d) ++ prim (Ton @k @(l - l2) @t2)
    pretty _ = pretty (Ton @k @(l2 - l1) @t1) ++ " | " ++ pretty (Dom @k @l1 @d) ++ " | " ++ pretty (Ton @k @(l - l2) @t2)

instance (IntLListRep d, IntLListRep t) => Primitive (PhraseVI (d :: Dominant k l1) (t :: Tonic k (l - l1)) :: Phrase k l) where
    type Rep (PhraseVI d t) = [[Int]]
    prim _ = prim (Dom @k @l1 @d) ++ prim (Ton @k @(l - l1) @t)
    pretty _ = pretty (Dom @k @l1 @d) ++ " | " ++ pretty (Ton @k @(l - l1) @t)

-- Progressions

instance (IntLListRep c) => Primitive (Cad c :: ProgType k l) where
    type Rep (Cad c) = [[Int]]
    prim _ = prim (Cade @k @l @c)
    pretty _ = pretty (Cade @k @l @c)

instance (IntLListRep ph, IntLListRep pr) => Primitive ((ph :: Phrase k l) := (pr :: ProgType k (n - l)) :: ProgType k n) where
    type Rep (ph := pr) = [[Int]]
    prim _ = prim (Phr @k @l @ph) ++ prim (Prog @k @(n - l) @pr)
    pretty _ = pretty (Phr @k @l @ph) ++ " || " ++ pretty (Prog @k @(n - l) @pr)
