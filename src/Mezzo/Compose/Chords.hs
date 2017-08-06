{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Chords
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

module Mezzo.Compose.Chords where

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

-- ** Dyad type literals
dyaTyLits

-- ** Triad type literals
triTyLits

-- ** Tetrad type literals
tetTyLits

_dbl :: TriType t -> TetType (DoubledT t)
_dbl t = TetType

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
seventh :: Root r -> TetType t -> Inv i -> Cho (Tetrad r t i)
seventh r t i = Cho

-- * Chord builders

-- ** Dyad converters
mkDyaConvs

-- ** Triad converters
mkTriConvs

-- ** Doubled dyad converters
mkDoubledDConvs

-- ** Tetrad converters
mkTetConvs

-- ** Doubled triad converters
mkDoubledTConvs

-- ** Inversion mutators
inv :: ChorM c (InvertChord c)
inv = constConv Cho
