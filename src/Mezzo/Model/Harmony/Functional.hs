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





data Degree (sd :: ScaleDegree) (q :: Quality) where
    DegChord :: ScaleChord d a q -> Degree d q

data Piece (m :: Mode) = Piece [Phrase m]

data Phrase (m :: Mode) where
    PhraseIVI :: Tonic m -> Dominant m -> Tonic m -> Phrase m
    PhraseVI ::             Dominant m -> Tonic m -> Phrase m

data Tonic (m :: Mode) where
    TonMaj :: Degree I Maj -> Tonic MajorMode
    TonMin :: Degree I Min -> Tonic MinorMode

data Dominant (m :: Mode) where
    DomV7   ::                   Degree V Dom7  -> Dominant m
    DomVM   ::                   Degree V Maj   -> Dominant m
    DomVII0 ::                   Degree VII Dim -> Dominant m
    DomSD   :: SubDominant m  -> Dominant m     -> Dominant m
    DomSecD :: Degree II Dom7 -> Degree V Dom7  -> Dominant m

data SubDominant (m :: Mode) where
    SubIIm     ::                   Degree II Min -> SubDominant MajorMode
    SubIVM     ::                   Degree IV Maj -> SubDominant MajorMode
    SubIIImIVM :: Degree III Min -> Degree IV Maj -> SubDominant MajorMode
    SubIVm     ::                   Degree IV Min -> SubDominant MinorMode
