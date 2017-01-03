{-# LANGUAGE  TypeInType, MultiParamTypeClasses, FlexibleInstances,
    UndecidableInstances, GADTs, TypeOperators, TypeFamilies #-}

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
    ) where

import GHC.TypeLits
import Data.Kind

import Mezzo.Model.Types hiding (IntervalClass (..))
import Mezzo.Model.Prim
import Mezzo.Model.Harmony.Chords

data Quality = Maj | Min | Dom7 | Dim

data Degree (d :: ScaleDegree) (q :: Quality) (k :: KeyType) where
    DegChord :: DiatonicDegree d q k => Degree d q k

class DiatonicDegree (d :: ScaleDegree) (q :: Quality) (k :: KeyType)
instance MajDegQuality d q => DiatonicDegree d q (Key pc acc MajorMode)
instance MinDegQuality d q => DiatonicDegree d q (Key pc acc MinorMode)

class MajDegQuality (d :: ScaleDegree) (q :: Quality)
instance {-# OVERLAPPING #-} MajDegQuality I Maj
instance {-# OVERLAPPING #-} MajDegQuality II Min
instance {-# OVERLAPPING #-} MajDegQuality III Min
instance {-# OVERLAPPING #-} MajDegQuality IV Maj
instance {-# OVERLAPPING #-} MajDegQuality V Maj
instance {-# OVERLAPPING #-} MajDegQuality V Dom7
instance {-# OVERLAPPING #-} MajDegQuality VI Min
instance {-# OVERLAPPING #-} MajDegQuality VII Dim
instance {-# OVERLAPPABLE #-} TypeError (Text "Can't have a "
                                    :<>: ShowType q
                                    :<>: ShowType d
                                    :<>: Text " degree chord in major mode.")
                                => MajDegQuality d q

class MinDegQuality (d :: ScaleDegree) (q :: Quality)
instance {-# OVERLAPPING #-} MinDegQuality I Min
instance {-# OVERLAPPING #-} MinDegQuality II Dim
instance {-# OVERLAPPING #-} MinDegQuality III Maj
instance {-# OVERLAPPING #-} MinDegQuality IV Min
instance {-# OVERLAPPING #-} MinDegQuality V Maj
instance {-# OVERLAPPING #-} MinDegQuality V Dom7
instance {-# OVERLAPPING #-} MinDegQuality VI Maj
instance {-# OVERLAPPING #-} MinDegQuality VII Maj
instance {-# OVERLAPPABLE #-} TypeError (Text "Can't have a "
                                    :<>: ShowType q
                                    :<>: ShowType d
                                    :<>: Text " degree chord in minor mode.")
                                => MinDegQuality d q


data Piece (k :: KeyType) (l :: Nat) where
    Cad :: Cadence k l -> Piece k l
    (:~) :: Phrase k l -> Piece k (n - l) -> Piece k n

data Phrase (k :: KeyType) (l :: Nat) where
    PhraseIVI :: Tonic k l1 -> Dominant k l2 -> Tonic k l3 -> Phrase k (l1 + l2 + l3)
    PhraseVI ::                Dominant k l1 -> Tonic k l2 -> Phrase k (l1 + l2)

type family QualToMode (q :: Quality) :: Mode where
    QualToMode Maj = MajorMode
    QualToMode Min = MinorMode
    QualToMode _   = TypeError (Text "Ambiguous mode.")

data Cadence (k :: KeyType) (l :: Nat) where
    AuthCad    :: Degree V Maj k   -> Degree I q k   -> Cadence k 2
    AuthCad7   :: Degree V Dom7 k  -> Degree I q k   -> Cadence k 2
    AuthCadVii :: Degree VII Dim k -> Degree I q k   -> Cadence k 2
    HalfCad    :: Degree d q k     -> Degree V Maj k -> Cadence k 2
    DeceptCad  :: Degree V Dom7 k  -> Degree VI q k  -> Cadence k 2

data Tonic (k :: KeyType) (l :: Nat) where
    TonMaj :: Degree I Maj k -> Tonic k 1
    TonMin :: Degree I Min k -> Tonic k 1

data Dominant (k :: KeyType) (l :: Nat) where
    DomVM   ::                     Degree V Maj  k  -> Dominant k 1
    DomV7   ::                     Degree V Dom7 k  -> Dominant k 1
    DomVii0 ::                     Degree VII Dim k -> Dominant k 1
    DomSD   :: SubDominant k l1 -> Dominant k l2    -> Dominant k (l1 + l2)
    DomSecD :: Degree II Dom7 k -> Degree V Dom7 k  -> Dominant k 2

data SubDominant (k :: KeyType) (l :: Nat) where
    SubIIm     ::                     Degree II Min k -> SubDominant k 1
    SubIVM     ::                     Degree IV Maj k -> SubDominant k 1
    SubIIImIVM :: Degree III Min k -> Degree IV Maj k -> SubDominant k 2
    SubIVm     ::                     Degree IV Min k -> SubDominant k 1

