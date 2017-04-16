
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Music
-- Description :  Mezzo music algebra
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Algebraic description of music with type-level constraints.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Music
    (
    -- * Music
      Music (..)
    , Signature (..)
    -- * Constraints
    , ValidChord
    , ValidProg
    , ValidHom
    , ValidMel
    , ValidHarm
    ) where

import Data.Kind
import GHC.TypeLits
import Text.PrettyPrint.Boxes

import Mezzo.Model.Prim
import Mezzo.Model.Harmony.Motion
import Mezzo.Model.Harmony.Chords
import Mezzo.Model.Harmony.Functional
import Mezzo.Model.Types
import Mezzo.Model.Reify
import Mezzo.Model.Rules.RuleSet

infixl 3 :|:
infixl 4 :-:

-------------------------------------------------------------------------------
-- The 'Music' datatype
-------------------------------------------------------------------------------

-- | A piece of music consisting of parallel and sequential composition of notes
-- and rests, subject to constraints.
--
-- Currently enforced constraints are:
--
--  * Height (number of voices) of two sequentially composed pieces must be equal.
--  * Width (number of temporal units) of two parallelly composed pieces must be equal.
--  * Sequentially composed voices cannot have any augmented, diminished or seventh leaps.
--  * Parallelly composed pieces cannot have any minor second or major seventh harmonic intervals.
--  * Music must not contain parallel or concealed unisons, fifths or octaves.
--
data Music :: forall n l t k. Signature t k -> Partiture n l -> Type where
    -- | Sequential or melodic composition of music.
    (:|:) :: ValidMel m1 m2  => Music s m1 -> Music s m2 -> Music s (m1 +|+ m2)
    -- | Parallel or harmonic composition of music.
    (:-:) :: ValidHarm m1 m2 => Music s m1 -> Music s m2 -> Music s (m1 +-+ m2)
    -- | A note specified by a pitch and a duration.
    Note :: ValidNote r d => Root r -> Dur d -> Music s (FromRoot r d)
    -- | A rest specified by a duration.
    Rest :: ValidRest d => Dur d -> Music s (FromSilence d)
    -- | A chord specified by a chord type and a duration.
    Chord :: ValidChord c d => Cho c -> Dur d -> Music s (FromChord c d)
    -- | A progression specified by a time signature, and its progression schema.
    Progression :: ValidProg t p => Prog p -> Music (Sig :: Signature t k) (FromProg p t)
    -- | A homophonic composition with a melody line and an accompaniment.
    Homophony :: ValidHom m a => Music s m -> Music s a -> Music s (m +-+ a)

-------------------------------------------------------------------------------
-- Musical constraints
-- Specifications of the rules that valid musical terms have to follow.
-------------------------------------------------------------------------------

-- | Select the active rule set.
type ActRuleSet = Classical

-- | Ensures that two pieces of music can be composed sequentially.
type ValidMel m1 m2 = MelConstraints ActRuleSet m1 m2

-- | Ensures that two pieces of music can be composed in parallel.
type ValidHarm m1 m2 = HarmConstraints ActRuleSet m1 m2

-- | Ensures that the note is valid.
type ValidNote r d = NoteConstraints ActRuleSet r d

-- | Ensures that the rest is valid.
type ValidRest d = RestConstraints ActRuleSet d

-- | Ensures that the chord is valid.
type ValidChord c d = ChordConstraints ActRuleSet c d

-- | Ensures that a progression is valid.
type ValidProg s p = ProgConstraints ActRuleSet s p

-- | Ensures that a homophonic composition is valid.
type ValidHom m a = HomConstraints ActRuleSet m a

-------------------------------------------------------------------------------
-- Pretty-printing
-------------------------------------------------------------------------------

instance Show (Music s m) where show = render . ppMusic

-- | Pretty-print a 'Music' value.
ppMusic :: Music s m -> Box
ppMusic (Note r d) = char '|' <+> doc r <+> doc d
ppMusic (Rest d) = char '|' <+> text "~~~~" <+> doc d
ppMusic (m1 :|: m2) = ppMusic m1 <> emptyBox 1 1 <> ppMusic m2
ppMusic (m1 :-: m2) = ppMusic m1 // ppMusic m2
ppMusic (Chord c d) = char '|' <+> doc c <+> doc d
-- ppMusic (Progression ts p) = text "Prog" <+> doc ts <+> doc p

-- | Convert a showable value into a pretty-printed box.
doc :: Show a => a -> Box
doc = text . show
