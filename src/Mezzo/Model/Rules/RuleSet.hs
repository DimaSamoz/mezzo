
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Rules.RuleSet
-- Description :  MIDI exporting
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Encapsulation of various musical rule sets that Mezzo can use.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Rules.RuleSet where

import Mezzo.Model.Types
import Mezzo.Model.Harmony
import Mezzo.Model.Reify
import Mezzo.Model.Prim

import qualified Mezzo.Model.Rules.ClassicalRules as CR

import Data.Kind
import GHC.TypeLits

-- | The types of rule sets implemented.
data RuleSetType =
      Classical     -- ^ Classical rules.
    | Free          -- ^ No composition rules.

-- ** Default constraints

-- | Default note constraints.
type DefNoteConstraints r d = (IntRep r, Primitive d)

-- | Default resst constraints.
type DefRestConstraints d = (Primitive d)

-- | Default chord constraints.
type DefChordConstraints (c :: ChordType n) d = (IntListRep c, Primitive n, Primitive d)

-- | Default progression constraints.
type DefProgConstraints s p = (IntLListRep p, IntRep s, KnownNat s)

-- | Class of rule sets for a given rule type.
class RuleSet (t :: RuleSetType) where
    type MelConstraints   t (m1 :: Partiture n l1) (m2 :: Partiture n l2) :: Constraint
    type HarmConstraints  t (m1 :: Partiture n1 l) (m2 :: Partiture n2 l) :: Constraint
    type NoteConstraints  t (r :: RootType)        (d :: Duration)        :: Constraint
    type RestConstraints  t                        (d :: Duration)        :: Constraint
    type ChordConstraints t (c :: ChordType n)     (d :: Duration)        :: Constraint
    type ProgConstraints  t (s :: TimeSignature)   (p :: ProgType k l)    :: Constraint
    type HomConstraints   t (m1 :: Partiture n1 l) (m2 :: Partiture n2 l) :: Constraint

    -- Defaults
    type NoteConstraints t r d = DefNoteConstraints r d
    type RestConstraints t d = DefRestConstraints d
    type ChordConstraints t c d = DefChordConstraints c d
    type ProgConstraints t s p = DefProgConstraints s p

-- | Classical rules.
instance RuleSet Classical where
    type MelConstraints Classical m1 m2 = CR.ValidMelConcat m1 m2
    type HarmConstraints Classical m1 m2 = CR.ValidHarmConcat (CR.Align m1 m2)
    type HomConstraints Classical m1 m2 = Valid

-- | No rules.
instance RuleSet Free where
    type MelConstraints Free m1 m2 = Valid
    type HarmConstraints Free m1 m2 = Valid
    type HomConstraints Free m1 m2 = Valid
