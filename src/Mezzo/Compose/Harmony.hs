{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Harmony
-- Description :  Harmony composition units
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Literals for chords and progressions.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Harmony where

import Mezzo.Model
import Mezzo.Compose.Types
import Mezzo.Compose.Builder
import Mezzo.Compose.Templates
import Mezzo.Compose.Basic

import GHC.TypeLits

infixr 5 :+

-- * Functional harmony terms

-- | Type of harmonic terms in some key.
type InKey k v = KeyS k -> v

toProg :: InKey k (PhraseList p) -> InKey k (Prog p)
toProg _ = const Prog

-- ** Progressions

-- | Create a new musical progression from the given time signature and progression schema.
prog :: ValidProg r t p => InKey k (PhraseList p) -> Music (Sig :: Signature t k r) (FromProg p t)
prog ik = Progression ((toProg ik) KeyS)

-- ** Phrases

-- | List of phrases in a progression parameterised by the key, ending with a cadence.
data PhraseList (p :: ProgType k l) where
    -- | A cadential phrase, ending the progression.
    Cdza :: Cad c -> KeyS k -> PhraseList (CadPhrase c)
    -- | Add a new phrase to the beginning of the progression.
    (:+) :: InKey k (Phr p) -> InKey k (PhraseList ps) -> KeyS k -> PhraseList (p := ps)

-- | Create a new cadential phrase.
cadence :: InKey k (Cad c) -> InKey k (PhraseList (CadPhrase c))
cadence c = \k -> Cdza (c k) k

-- | Dominant-tonic phrase.
ph_I :: InKey k (Ton (t :: Tonic k l)) -> InKey k (Phr (PhraseI t :: Phrase k l))
ph_I _ = const Phr

-- | Dominant-tonic phrase.
ph_VI :: InKey k (Dom (d :: Dominant k l1)) -> InKey k (Ton (t :: Tonic k (l - l1))) -> InKey k (Phr (PhraseVI d t :: Phrase k l))
ph_VI _ _ = const Phr

-- | Tonic-dominant-tonic phrase.
ph_IVI :: InKey k (Ton (t1 :: Tonic k (l2 - l1))) -> InKey k (Dom (d :: Dominant k l1)) -> InKey k (Ton (t2 :: Tonic k (l - l2))) -> InKey k (Phr (PhraseIVI t1 d t2 :: Phrase k l))
ph_IVI _ _ _ = const Phr

-- ** Cadences

-- | Authentic V-I dominant cadence.
auth_V_I :: InKey k (Cad (AuthCad (DegChord :: DegreeC V MajQ k Inv1 Oct2) (DegChord :: DegreeC I (KeyToQual k) k Inv0 Oct3)))
auth_V_I = const Cad

-- | Authentic V7-I dominant seventh cadence.
auth_V7_I :: InKey k (Cad (AuthCad7 (DegChord :: DegreeC V DomQ k Inv2 Oct2) (DegChord :: DegreeC I (KeyToQual k) k Inv0 Oct3)))
auth_V7_I = const Cad

-- | Authentic vii-I leading tone cadence.
auth_vii_I :: InKey k (Cad (AuthCadVii (DegChord :: DegreeC VII DimQ k Inv1 Oct2) (DegChord :: DegreeC I (KeyToQual k) k Inv0 Oct3)))
auth_vii_I = const Cad

-- | Authentic cadential 6-4 cadence.
auth_64_V7_I :: InKey k (Cad (AuthCad64 (DegChord :: DegreeC I (KeyToQual k) k Inv2 Oct3) (DegChord :: DegreeC V DomQ k Inv3 Oct2) (DegChord :: DegreeC I (KeyToQual k) k Inv1 Oct3)))
auth_64_V7_I = const Cad

-- | Deceptive V-iv cadence.
decept_V_iv :: InKey k (Cad (DeceptCad (DegChord :: DegreeC V DomQ k Inv2 Oct2) (DegChord :: DegreeC VI (KeyToOtherQual k) k Inv1 Oct2)))
decept_V_iv = const Cad

-- | Full cadence, starting with a subdominant.
full :: InKey k (Sub s) -> InKey k (Cad c) -> InKey k (Cad (FullCad s c))
full _ _ = const Cad

-- | End progression without an explicit cadence.
end :: InKey k (PhraseList (CadPhrase NoCad))
end = cadence $ const Cad

-- ** Tonic chords

-- | Tonic chord.
ton :: InKey k (Ton (TonT (DegChord :: DegreeC I (KeyToQual k) k Inv0 Oct3)))
ton = const Ton

-- | Doubled tonics.
ton_T_T :: InKey k (Ton ton1) -> InKey k (Ton ton2) -> InKey k (Ton (TonTT ton1 ton2))
ton_T_T _ _ = const Ton


-- ** Dominants

-- | Dominant (V) chord.
dom_V :: InKey k (Dom (DomVM (DegChord :: DegreeC V MajQ k Inv2 Oct2)))
dom_V = const Dom

-- | Dominant seventh (V7) chord.
dom_V7 :: InKey k (Dom (DomV7 (DegChord :: DegreeC V DomQ k Inv2 Oct2)))
dom_V7 = const Dom

-- | Dominant leading tone (vii) chord.
dom_vii0 :: InKey k (Dom (DomVii0 (DegChord :: DegreeC VII DimQ k Inv1 Oct2)))
dom_vii0 = const Dom

-- | Secondary dominant - dominant (V/V-V7) chord.
dom_II_V7 :: InKey k (Dom (DomSecD (DegChord :: DegreeC II DomQ k Inv0 Oct3) (DegChord :: DegreeC V DomQ k Inv2 Oct2)))
dom_II_V7 = const Dom

-- | Subdominant followed by a dominant.
dom_S_D :: InKey k (Sub subdom) -> InKey k (Dom dom) -> InKey k (Dom (DomSD subdom dom))
dom_S_D _ _ = const Dom

-- | Dominant followed by another dominant.
dom_D_D :: InKey k (Dom dom1) -> InKey k (Dom dom2) -> InKey k (Dom (DomDD dom1 dom2))
dom_D_D _ _ = const Dom

-- ** Subdominants

-- | Subdominant fourth (IV) chord.
subdom_IV :: InKey k (Sub (SubIV (DegChord :: DegreeC IV (KeyToQual k) k Inv2 Oct2)))
subdom_IV = const Sub

-- | Subdominant minor second (ii) chord.
subdom_ii :: IsMajor k "ii subdominant"
    => InKey k (Sub (SubIIm (DegChord :: DegreeC II MinQ k Inv0 Oct3)))
subdom_ii = const Sub

-- | Subdominant third-fourth (iii-IV) progression.
subdom_iii_IV :: IsMajor k "iii-IV subdominant"
    => InKey k (Sub (SubIIImIVM (DegChord :: DegreeC III MinQ k Inv0 Oct3) (DegChord :: DegreeC IV MajQ k Inv3 Oct2)))
subdom_iii_IV = const Sub

-- | Doubled subdominants.
subdom_S_S :: InKey k (Sub sub1) -> InKey k (Sub sub2) -> InKey k (Sub (SubSS sub1 sub2))
subdom_S_S _ _ = const Sub

-- * Time signatures

-- | Duple meter (2/4).
duple :: TimeSig 2
duple = TimeSig

-- | Triple meter (3/4).
triple :: TimeSig 3
triple = TimeSig

-- | Quadruple meter (4/4).
quadruple :: TimeSig 4
quadruple = TimeSig

-- * Key literals
mkKeyLits
