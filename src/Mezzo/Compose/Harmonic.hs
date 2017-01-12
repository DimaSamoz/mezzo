{-# LANGUAGE TypeInType, TypeApplications, TemplateHaskell, RankNTypes, ViewPatterns, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Harmonic
-- Description :  Harmonic composition units
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

module Mezzo.Compose.Harmonic where

import Mezzo.Model
import Mezzo.Compose.Types
import Mezzo.Compose.Templates
import Mezzo.Compose.Basic

-- * Atomic literals

-- ** Scale degree literals
scaleDegreeLits

-- ** Mode literals
modeLits

-- ** Triad type literals
triTyLits

-- ** Seventh type literals
sevTyLits

_dbl :: TriType t -> SevType (Doubled t)
_dbl t = SevType

-- ** Inversion literals
invLits

-- ** Constructors

-- | Create a new key from a pitch class, accidental and mode.
key :: PC p -> Acc a -> Mod m -> KeyS (Key p a m)
key p a m = KeyS

-- | Create a new root from a pitch.
rootP :: Pit p -> Root (PitchRoot p)
rootP p = Root

-- | Create a new root from a key and a scale degree.
rootS :: KeyS k -> ScaDeg d -> Root (DegreeRoot k d)
rootS k d = Root

-- | Create a triad from a root, a triad type and an inversion.
triad :: Root r -> TriType t -> Inv i -> Cho (Triad r t i)
triad r t i = Cho

-- | Create a seventh chord from a root, a triad type and an inversion.
seventh :: Root r -> SevType t -> Inv i -> Cho (SeventhChord r t i)
seventh r t i = Cho

-- * Continuation literals

-- | Get the actual duration of a duration continuation
getDur :: Root r -> DurC r d -> Dur d
getDur p (($ p) -> Note _ dur) = dur
getDur p (($ p) -> Rest dur) = dur

-- ** Triads
mkTriTyCombs

-- ** Seventh chords
mkSevTyCombs

-- ** Doubled triads
mkDoubledTyCombs
