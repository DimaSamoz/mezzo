{-# LANGUAGE ViewPatterns, NoStarIsType #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

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
    -- * Types and operations
      Quality (..)
    , DegreeC (..)
    , TimeSignature
    , TimeSig (..)
    , KeyToQual
    , KeyToOtherQual
    , IsMajor
    , IsMinor
    -- * Functional harmony
    -- ** Types and constructors
    , ProgType (..)
    , Phrase (..)
    , Cadence (..)
    , Tonic (..)
    , Dominant (..)
    , Subdominant (..)
    , ChordsToPartiture
    , ProgTypeToChords
    , FromProg
    -- ** Singletons
    , Prog (..)
    , Ton (..)
    , Dom (..)
    , Sub (..)
    , Cad (..)
    , Phr (..)
    )
    where

import Mezzo.Model.Reify
import Mezzo.Model.Types hiding (IntervalClass (..))
import Mezzo.Model.Prim
import Mezzo.Model.Harmony.Chords

import GHC.TypeLits
import Data.Kind

infix 5 :=

-------------------------------------------------------------------------------
-- Types and operations
-------------------------------------------------------------------------------

-- | The quality of a scale degree chord.
data Quality = MajQ | MinQ | DomQ | DimQ

-- | A scale degree chord in given key, on the given scale, with the given quality and octave.
data DegreeC (d :: ScaleDegree) (q :: Quality) (k :: KeyType) (i :: Inversion) (o :: OctaveNum) where
    DegChord :: DegreeC d q k i o

-- | The number of beats in a bar.
type TimeSignature = Nat

-- | Singleton for 'TimeSignature'.
data TimeSig (t :: TimeSignature) = TimeSig

-- | Convert the key mode to the corresponding chord quality (i.e., major mode into a major chord).
type family KeyToQual (k :: KeyType) where
    KeyToQual (Key _ _ MajorMode) = MajQ
    KeyToQual (Key _ _ MinorMode) = MinQ

-- | Convert the key mode to the opposite chord quality (i.e., major mode into a minor chord).
type family KeyToOtherQual (k :: KeyType) where
    KeyToOtherQual (Key _ _ MajorMode) = MinQ
    KeyToOtherQual (Key _ _ MinorMode) = MajQ

-- | Convert a quality to a seventh chord type.
type family QualToType (q :: Quality) :: TetradType where
    QualToType MajQ = DoubledT MajTriad
    QualToType MinQ = DoubledT MinTriad
    QualToType DomQ = MajMinSeventh
    QualToType DimQ = DimSeventh

-- | Enforce that the key is in major mode.
class IsMajor (k :: KeyType) (s :: Symbol)
instance IsMajor (Key pc acc MajorMode) s
instance TypeError (Text "Can't have a " :<>: Text s :<>: Text " in minor mode.")
    => IsMajor (Key pc acc MinorMode) s

-- | Enforce that the key is in minor mode.
class IsMinor (k :: KeyType) (s :: Symbol)
instance IsMinor (Key pc acc MinorMode) s
instance TypeError (Text "Can't have a " :<>: Text s :<>: Text " in minor mode.")
    => IsMinor (Key pc acc MajorMode) s

-------------------------------------------------------------------------------
-- Functional harmony
-------------------------------------------------------------------------------

-- | A functionally described piece of music, built from multiple phrases.
data ProgType (k :: KeyType) (l :: Nat) where
    -- | A cadential phrase, ending the progression.
    CadPhrase :: Cadence k l -> ProgType k l
    -- | Add a new phrase to the beginning of the progression.
    (:=) :: Phrase k l -> ProgType k (n - l) -> ProgType k n

-- | A phrase matching a specific functional progression.
data Phrase (k :: KeyType) (l :: Nat) where
    -- | A tonic-dominant-tonic phrase.
    PhraseIVI :: Tonic k (l2 - l1) -> Dominant k l1 -> Tonic k (l - l2) -> Phrase k l
    -- | A dominant-tonic phrase.
    PhraseVI  :: Dominant k l1 -> Tonic k (l - l1) -> Phrase k l
    -- | A tonic phrase.
    PhraseI  :: Tonic k l -> Phrase k l

-- | A cadence in a specific key with a specific length.
data Cadence (k :: KeyType) (l :: Nat) where
    -- | Authentic cadence with major fifth chord.
    AuthCad    :: DegreeC V MajQ k Inv1 (OctPred o) -> DegreeC I q k Inv0 o -> Cadence k 2
    -- | Authentic cadence with dominant seventh fifth chord.
    AuthCad7   :: DegreeC V DomQ k Inv2 (OctPred o) -> DegreeC I q k Inv0 o -> Cadence k 2
    -- | Authentic cadence with diminished seventh chord.
    AuthCadVii :: DegreeC VII DimQ k Inv1 (OctPred o) -> DegreeC I q k Inv0 o -> Cadence k 2
    -- | Authentic cadence with a cadential 6-4 chord
    AuthCad64  :: DegreeC I q k Inv2 o -> DegreeC V DomQ k Inv3 (OctPred o) -> DegreeC I q k Inv1 o -> Cadence k 3
    -- | Deceptive cadence from a dominant fifth to a sixth.
    DeceptCad  :: DegreeC V DomQ k Inv2 o -> DegreeC VI q k Inv1 o -> Cadence k 2
    -- | Full cadence from subdominant to dominant to tonic.
    FullCad    :: Subdominant k l1 -> Cadence k (l - l1) -> Cadence k l
    -- | No cadence.
    NoCad      :: Cadence k 0

-- | A tonic chord.
data Tonic (k :: KeyType) (l :: Nat) where
    -- | A major tonic chord.
    TonT   :: DegreeC I (KeyToQual k) k Inv0 o -> Tonic k 1
    -- | Doubled tonics.
    TonTT  :: Tonic k l1 -> Tonic k (l - l1) -> Tonic k l

-- | A dominant chord progression.
data Dominant (k :: KeyType) (l :: Nat) where
    -- | Major fifth dominant.
    DomVM   :: DegreeC V MajQ k Inv2 o -> Dominant k 1
    -- | Seventh chord fifth degree dominant.
    DomV7   :: DegreeC V DomQ k Inv2 o -> Dominant k 1
    -- | Diminished seventh degree dominant.
    DomVii0 :: DegreeC VII DimQ k i o -> Dominant k 1
    -- | Secondary dominant followed by dominant.
    DomSecD :: DegreeC II DomQ k Inv0 o -> DegreeC V DomQ k Inv2 (OctPred o) -> Dominant k 2
    -- | Subdominant followed by dominant.
    DomSD   :: Subdominant k l1 -> Dominant k (l - l1) -> Dominant k l
    -- | Doubled dominants.
    DomDD   :: Dominant k l1 -> Dominant k (l - l1) -> Dominant k l

-- | A subdominant chord progression.
data Subdominant (k :: KeyType) (l :: Nat) where
    -- | Major fourth subdominant.
    SubIV      :: DegreeC IV (KeyToQual k) k i o -> Subdominant k 1
    -- | Minor second subdominant.
    SubIIm     :: DegreeC II MinQ k i o -> Subdominant k 1
    -- | Minor third followed by major fourth subdominant.
    SubIIImIVM :: DegreeC III MinQ k i1 o -> DegreeC IV MajQ k i2 (OctPred o) -> Subdominant k 2
    -- | Doubled subdominants.
    SubSS      :: Subdominant k l1 -> Subdominant k (l - l1) -> Subdominant k l

-- | Convert a degree chord to a tetrad.
type DegToChord (dc :: DegreeC d q k i o) = Tetrad (DegreeRoot k (Degree d Natural o)) (QualToType q) i

-- | Convert a cadence to chords.
type family CadToChords (l :: Nat) (c :: Cadence k l) :: Vector (ChordType 4) l where
    CadToChords 2 (AuthCad d1 d2)      = DegToChord d1 :-- DegToChord d2 :-- None
    CadToChords 2 (AuthCad7 d1 d2)     = DegToChord d1 :-- DegToChord d2 :-- None
    CadToChords 2 (AuthCadVii d1 d2)   = DegToChord d1 :-- DegToChord d2 :-- None
    CadToChords 2 (DeceptCad d1 d2)    = DegToChord d1 :-- DegToChord d2 :-- None
    CadToChords 3 (AuthCad64 d1 d2 d3) = DegToChord d1 :-- DegToChord d2 :-- DegToChord d3 :-- None
    CadToChords l (FullCad (s :: Subdominant k l1) c) = SubdomToChords l1 s ++. CadToChords (l - l1) c
    CadToChords 0 NoCad                = None

-- | Convert a tonic to chords.
type family TonToChords (l :: Nat) (t :: Tonic k l) :: Vector (ChordType 4) l where
    TonToChords 1 (TonT d) = DegToChord d :-- None
    TonToChords l (TonTT (t1 :: Tonic k l1) t2) = TonToChords l1 t1 ++. TonToChords (l - l1) t2

-- | Convert a dominant to chords.
type family DomToChords (l :: Nat) (t :: Dominant k l) :: Vector (ChordType 4) l where
    DomToChords 1 (DomVM d)       = DegToChord d  :-- None
    DomToChords 1 (DomV7 d)       = DegToChord d  :-- None
    DomToChords 1 (DomVii0 d)     = DegToChord d  :-- None
    DomToChords 2 (DomSecD d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None
    DomToChords l (DomSD (s :: Subdominant k l1) d) =
        SubdomToChords l1 s ++. DomToChords (l - l1) d
    DomToChords l (DomDD (d1 :: Dominant k l1) d2)  =
        DomToChords l1 d1 ++. DomToChords (l - l1) d2

-- | Convert a subdominant to chords.
type family SubdomToChords (l :: Nat) (t :: Subdominant k l) :: Vector (ChordType 4) l where
    SubdomToChords 1 (SubIIm d)         = DegToChord d :-- None
    SubdomToChords 1 (SubIV d)          = DegToChord d :-- None
    SubdomToChords 2 (SubIIImIVM d1 d2) = DegToChord d1 :-- DegToChord d2 :-- None
    SubdomToChords l (SubSS (s1 :: Subdominant k l1) s2) =
        SubdomToChords l1 s1 ++. SubdomToChords (l - l1) s2

-- | Convert a phrase to chords.
type family PhraseToChords (l :: Nat) (p :: Phrase k l) :: Vector (ChordType 4) l where
    PhraseToChords l (PhraseIVI (t1 :: Tonic k (l2 - dl)) (d :: Dominant k dl)
                                (t2 :: Tonic k (l - l2))) =
        TonToChords (l2 - dl) t1 ++. DomToChords dl d ++. TonToChords (l - l2) t2
    PhraseToChords l (PhraseVI (d :: Dominant k dl) t) =
        DomToChords dl d ++. TonToChords (l - dl) t
    PhraseToChords l (PhraseI t) =
        TonToChords l t

-- | Convert a piece to chords.
type family ProgTypeToChords (l :: Nat) (p :: ProgType k l) :: Vector (ChordType 4) l where
    ProgTypeToChords l (CadPhrase (c :: Cadence k l)) = CadToChords l c
    ProgTypeToChords l ((p :: Phrase k l1) := ps) =
        PhraseToChords l1 p ++. ProgTypeToChords (l - l1) ps

-- | Convert a vector of chords ("chord progression") into a 'Partiture'.
type family ChordsToPartiture (v :: Vector (ChordType n) (l :: Nat)) (t :: Nat) :: Partiture n (l * t * 8) where
    ChordsToPartiture None _ = (End :-- End :-- End :-- End :-- None)
    ChordsToPartiture (c :-- cs) l = FromChord c (l * 8) +|+ ChordsToPartiture cs l

-- | Convert a progression with a time signature into a 'Partiture'.
type family FromProg (p :: ProgType k l) (t :: TimeSignature) :: Partiture 4 (l * t * 8) where
    FromProg (p :: ProgType k l) t = ChordsToPartiture (ProgTypeToChords l p) t


-- Singletons

-- | The singleton type for 'Tonic'.
data Ton (t :: Tonic k d) = Ton

-- | The singleton type for 'Tonic'.
data Dom (d :: Dominant k d') = Dom

-- | The singleton type for 'Tonic'.
data Sub (s :: Subdominant k d) = Sub

-- | The singleton type for 'Tonic'.
data Cad (c :: Cadence k d) = Cad

-- | The singleton type for 'Tonic'.
data Phr (p :: Phrase k d) = Phr

-- | The singleton type for 'Tonic'.
data Prog (p :: ProgType k l) = Prog

-------------------------------------------------------------------------------
-- Primitive instances
-------------------------------------------------------------------------------

-- Tonic

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (TonT d) where
    type Rep (TonT d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Ton"

instance (IntLListRep t1, IntLListRep t2) => Primitive (TonTT (t1 :: Tonic k dur1) (t2 :: Tonic k (l - dur1)) :: Tonic k l) where
    type Rep (TonTT t1 t2) = [[Int]]
    prim _ = prim (Ton @k @dur1 @t1) ++ prim (Ton @k @(l - dur1) @t2)
    pretty _ = pretty (Ton @k @dur1 @t1) ++ " | " ++ pretty (Ton @k @(l - dur1) @t2)

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

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2) => Primitive (DomSecD d1 d2) where
    type Rep (DomSecD d1 d2) = [[Int]]
    prim _ = [prim (Cho @4 @ch1)] ++ [prim (Cho @4 @ch2)]
    pretty _ = "Dom SecD"

instance (IntLListRep sd, IntLListRep d) => Primitive (DomSD (sd :: Subdominant k sdur) (d :: Dominant k (l - sdur)) :: Dominant k l) where
    type Rep (DomSD sd d) = [[Int]]
    prim _ = prim (Sub @k @sdur @sd) ++ prim (Dom @k @(l - sdur) @d)
    pretty _ = pretty (Sub @k @sdur @sd) ++ " | " ++ pretty (Dom @k @(l - sdur) @d)

instance (IntLListRep d1, IntLListRep d2) => Primitive (DomDD (d1 :: Dominant k dur1) (d2 :: Dominant k (l - dur1)) :: Dominant k l) where
    type Rep (DomDD d1 d2) = [[Int]]
    prim _ = prim (Dom @k @dur1 @d1) ++ prim (Dom @k @(l - dur1) @d2)
    pretty _ = pretty (Dom @k @dur1 @d1) ++ " | " ++ pretty (Dom @k @(l - dur1) @d2)

-- Subdominant

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (SubIIm d) where
    type Rep (SubIIm d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Sub ii"

instance (ch ~ DegToChord d, IntListRep ch) => Primitive (SubIV d) where
    type Rep (SubIV d) = [[Int]]
    prim _ = [prim (Cho @4 @ch)]
    pretty _ = "Sub IV"

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2) => Primitive (SubIIImIVM d1 d2) where
    type Rep (SubIIImIVM d1 d2) = [[Int]]
    prim _ = [prim (Cho @4 @ch1), prim (Cho @4 @ch2)]
    pretty _ = "Sub iii IV"

instance (IntLListRep s1, IntLListRep s2) => Primitive (SubSS (s1 :: Subdominant k dur1) (s2 :: Subdominant k (l - dur1)) :: Subdominant k l) where
    type Rep (SubSS s1 s2) = [[Int]]
    prim _ = prim (Sub @k @dur1 @s1) ++ prim (Sub @k @(l - dur1) @s2)
    pretty _ = pretty (Sub @k @dur1 @s1) ++ " | " ++ pretty (Sub @k @(l - dur1) @s2)


-- Cadences

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2) => Primitive (AuthCad d1 d2) where
    type Rep (AuthCad d1 d2) = [[Int]]
    prim _ = [prim (Cho @4 @ch1), prim (Cho @4 @ch2)]
    pretty _ = "AuthCad V"

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

instance (ch1 ~ DegToChord d1, IntListRep ch1, ch2 ~ DegToChord d2, IntListRep ch2) => Primitive (DeceptCad d1 d2) where
    type Rep (DeceptCad d1 d2) = [[Int]]
    prim _ = [prim (Cho @4 @ch1), prim (Cho @4 @ch2)]
    pretty _ = "DeceptCad"

instance (IntLListRep sd, IntLListRep c) => Primitive (FullCad (sd :: Subdominant k sdur) (c :: Cadence k (l - sdur)) :: Cadence k l) where
    type Rep (FullCad sd c) = [[Int]]
    prim _ = prim (Sub @k @sdur @sd) ++ prim (Cad @k @(l - sdur) @c)
    pretty _ = pretty (Sub @k @sdur @sd) ++ " | " ++ pretty (Cad @k @(l - sdur) @c)

instance Primitive NoCad where
    type Rep NoCad = [[Int]]
    prim _ = []
    pretty _ = "NoCad"

-- Phrases

instance (IntLListRep t1, IntLListRep d, IntLListRep t2) => Primitive (PhraseIVI (t1 :: Tonic k (l2 - l1)) (d :: Dominant k l1) (t2 :: Tonic k (l - l2)) :: Phrase k l) where
    type Rep (PhraseIVI t1 d t2) = [[Int]]
    prim _ = prim (Ton @k @(l2 - l1) @t1) ++ prim (Dom @k @l1 @d) ++ prim (Ton @k @(l - l2) @t2)
    pretty _ = pretty (Ton @k @(l2 - l1) @t1) ++ " | " ++ pretty (Dom @k @l1 @d) ++ " | " ++ pretty (Ton @k @(l - l2) @t2)

instance (IntLListRep d, IntLListRep t) => Primitive (PhraseVI (d :: Dominant k l1) (t :: Tonic k (l - l1)) :: Phrase k l) where
    type Rep (PhraseVI d t) = [[Int]]
    prim _ = prim (Dom @k @l1 @d) ++ prim (Ton @k @(l - l1) @t)
    pretty _ = pretty (Dom @k @l1 @d) ++ " | " ++ pretty (Ton @k @(l - l1) @t)

instance (IntLListRep t) => Primitive (PhraseI (t :: Tonic k l) :: Phrase k l) where
    type Rep (PhraseI t) = [[Int]]
    prim _ = prim (Ton @k @l @t)
    pretty _ = pretty (Ton @k @l @t)

-- Progressions

instance (IntLListRep c) => Primitive (CadPhrase c :: ProgType k l) where
    type Rep (CadPhrase c) = [[Int]]
    prim _ = prim (Cad @k @l @c)
    pretty _ = pretty (Cad @k @l @c)

instance (IntLListRep ph, IntLListRep pr) => Primitive ((ph :: Phrase k l) := (pr :: ProgType k (n - l)) :: ProgType k n) where
    type Rep (ph := pr) = [[Int]]
    prim _ = prim (Phr @k @l @ph) ++ prim (Prog @k @(n - l) @pr)
    pretty _ = pretty (Phr @k @l @ph) ++ " || " ++ pretty (Prog @k @(n - l) @pr)
