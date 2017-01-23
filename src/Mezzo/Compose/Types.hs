{-# LANGUAGE TypeInType, RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Types
-- Description :  Common types
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used by the composition library.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Types
    (
    -- * Duration type synonyms
      Whole
    , Half
    , Quarter
    , Eighth
    , Sixteenth
    , ThirtySecond
    -- * Combinatorial types
    -- , RootS
    -- , DurC
    -- , ChordM
    -- , ChordM'
    )
    where

import Mezzo.Model
import Control.Monad.Cont

-------------------------------------------------------------------------------
-- Duration type synonyms
-------------------------------------------------------------------------------

-- | Whole note duration.
type Whole = Dur 32

-- | Half note duration.
type Half = Dur 16

-- | Quarter note duration.
type Quarter = Dur 8

-- | Eighth note duration.
type Eighth = Dur 4

-- | Sixteenth note duration.
type Sixteenth = Dur 2

-- | Thirty-second note duration.
type ThirtySecond = Dur 1

-------------------------------------------------------------------------------
-- Combinatorial types
-- Types employed in the combinatorial input of notes.
-------------------------------------------------------------------------------

-- -- The type of root specifiers.
-- type RootS r = forall m. (Root r -> m) -> m
--
-- type RootM r1 r2 = Root r1 -> RootS r2
--
-- -- | The type of chord modifiers with default inversion.
-- type ChordM c r t d = Root r -> DurC r d -> Music (FromChord (c r t Inv0) d)
--
-- -- | The type of combinatorial chord type literals with custom inversion.
-- type ChordM' c r t i d = Root r -> Inv i -> DurC r d -> Music (FromChord (c r t i) d)
--
-- -- | The type of duration continuations.
-- type DurC r d = Root r -> Music (FromRoot r d)
--
-- type ChordDurC c r t i d = DurC r d -> Root r -> Music (FromChord (c r t i) d)

type Spec t = forall m. (t -> m) -> m

type Trans s s' = s -> Spec s'

type Fin a r = a -> r

-- type Conv t t' = Spec t -> Spec t'

type RootS r = Spec (Root r)

type ChorS c = Spec (Cho c)

type RootT r r' = Trans (Root r) (Root r')

type ChorT c c' = Trans (Cho c) (Cho c')

type TriadT r t i = Trans (Root r) (Cho (Triad r t i))

type NoteF r d = Fin (Root r) (Music (FromRoot r d))

type ChorF c d = Fin (Cho c) (Music (FromChord c d))

c :: RootS (PitchRoot (Pitch C Natural Oct3))
c = spec Root

sharp :: RootT r (Sharpen r)
sharp r = spec Root

maj :: TriadT r MajTriad Inv0
maj r = spec Cho

qn :: NoteF r 8
qn p = Note p Dur

qc :: ChorF c 8
qc c = Chord c Dur

spec :: t -> Spec t
spec i c = c i

int :: Int -> Spec Int
int = spec

inc :: Trans Int Int
inc i = spec (succ i)

toString :: Trans Int String
toString n = spec (show n)

ex :: Trans String String
ex s = spec (s ++ "!")

smile :: Fin String String
smile s = s ++ " :)"
