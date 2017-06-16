
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
    , ValidNote
    , ValidRest
    , ValidChord
    , ValidProg
    , ValidHom
    , ValidMel
    , ValidHarm
    , ValidTripl
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

-- | Properties of a musical piece: the time signature, the key signature and rule set.
data Signature (t :: TimeSignature) (k :: KeyType) (ruleset :: Type) = Sig

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
data Music :: forall n l t k r. Signature t k r -> Partiture n l -> Type where
    -- | Sequential or melodic composition of music.
    (:|:) :: ValidMel s m1 m2  => Music s m1 -> Music s m2 -> Music s (m1 +|+ m2)
    -- | Parallel or harmonic composition of music.
    (:-:) :: ValidHarm s m1 m2 => Music s m1 -> Music s m2 -> Music s (m1 +-+ m2)
    -- | A note specified by a pitch and a duration.
    Note :: ValidNote s r d => Root r -> Dur d -> Music s (FromRoot r d)
    -- | A rest specified by a duration.
    Rest :: ValidRest s d => Dur d -> Music s (FromSilence d)
    -- | A chord specified by a chord type and a duration.
    Chord :: ValidChord s c d => Cho c -> Dur d -> Music s (FromChord c d)
    -- | A progression specified by a time signature, and its progression schema.
    Progression :: ValidProg r t p => Prog p -> Music (Sig :: Signature t k r) (FromProg p t)
    -- | A homophonic composition with a melody line and an accompaniment.
    Homophony :: ValidHom s m a => Music s m -> Music s a -> Music s (m +-+ a)
    -- | A triplet with a nominal duration and three pitches.
    Triplet :: ValidTripl s d r1 r2 r3 => Dur d -> Root r1 -> Root r2 -> Root r3 -> Music s (FromTriplet d r1 r2 r3)

-------------------------------------------------------------------------------
-- Musical constraints
-- Specifications of the rules that valid musical terms have to follow.
-------------------------------------------------------------------------------

-- | Select the active rule set.
type ActRuleSet = Classical

-- | Ensures that two pieces of music can be composed sequentially.
type ValidMel (s :: Signature t k r) m1 m2 =
    MelConstraints r m1 m2

-- | Ensures that two pieces of music can be composed in parallel.
type ValidHarm (s :: Signature t k r) m1 m2 =
    HarmConstraints r m1 m2

-- | Ensures that the note is valid.
type ValidNote (s :: Signature t k r) ro d =
    (NoteConstraints r ro d, IntRep ro, Primitive d)

-- | Ensures that the rest is valid.
type ValidRest (s :: Signature t k r) d =
    (RestConstraints r d, Primitive d)

-- | Ensures that the chord is valid.
type ValidChord (s :: Signature t k r) (c :: ChordType n) d =
    (ChordConstraints r c d, IntListRep c, Primitive n, Primitive d)

-- | Ensures that a progression is valid.
type ValidProg r t p =
    (ProgConstraints r t p, IntLListRep p, IntRep t, KnownNat t)

-- | Ensures that a homophonic composition is valid.
type ValidHom (s :: Signature t k r) m a =
    HomConstraints r m a

-- | Ensures that a triplet is valid.
type ValidTripl (s :: Signature t k r) d r1 r2 r3 =
    ( TriplConstraints r d r1 r2 r3, IntRep r1, IntRep r2, IntRep r3, Primitive d
    , Primitive (HalfOf d), NoteConstraints r r1 d, NoteConstraints r r2 (HalfOf d)
    , NoteConstraints r r3 (HalfOf d))

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
ppMusic (Progression p) = text "Prog:" <+> doc p

-- | Convert a showable value into a pretty-printed box.
doc :: Show a => a -> Box
doc = text . show
