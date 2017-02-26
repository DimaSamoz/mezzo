{-# LANGUAGE TypeInType, TypeApplications, TemplateHaskell, RankNTypes,
    GADTs, ImplicitParams, FlexibleContexts #-}

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
import Mezzo.Compose.Builder
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

-- | Create a triad from a root, a triad type and an inversion.
triad :: Root r -> TriType t -> Inv i -> Cho (Triad r t i)
triad r t i = Cho

-- | Create a seventh chord from a root, a triad type and an inversion.
seventh :: Root r -> SevType t -> Inv i -> Cho (SeventhChord r t i)
seventh r t i = Cho

-- * Chord builders

-- ** Triad converters
mkTriConvs

-- ** Seventh chord converters
mkSevConvs

-- ** Doubled triad converters
mkDoubledConvs

-- ** Inversion mutators
inv :: ChorM c (InvertChord c)
inv = constConv Cho
