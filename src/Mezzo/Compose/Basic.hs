{-# LANGUAGE TypeInType, TypeApplications, TemplateHaskell, RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Basic
-- Description :  Basic composition units
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Literals for pitches, notes, durations, etc.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Basic where

import Mezzo.Model
import Mezzo.Compose.Types
import Mezzo.Compose.Templates

-- * Atomic literals

-- ** Pitch class literals
pitchClassLits

-- ** Accidental literals
accidentalLits

-- ** Octave literals
octaveLits

-- ** Duration literals
_wh :: Whole
_wh = Dur @32

_ha :: Half
_ha = Dur @16

_qu :: Quarter
_qu = Dur @8

_ei :: Eighth
_ei = Dur @4

_si :: Dur 2
_si = Dur @2

_th :: Dur 1
_th = Dur @1

-- * Pitches

-- ** Constructor

-- | Create a new pitch with the given class, accidental and octave.
pitch :: PC pc -> Acc acc -> Oct oct -> Pit (Pitch pc acc oct)
pitch pc acc oct = Pit

-- | Value representing silence, the "pitch" of rests.
silence :: Pit Silence
silence = Pit

-- ** Concrete literals
mkPitchLits

-- ** Combinatorial literals (admitting continuations)
mkPitchCombs

r :: (Pit Silence -> m) -> m
r = \dur -> dur silence

-- * Notes

-- ** Constructors
note :: Pit p -> Dur d -> Music (FromPitch p d)
note p d = Note p d

rest :: Dur d -> Music (FromPitch Silence d)
rest d = Rest d

-- ** Duration continuations

wh :: DurC p 32
wh = \p -> Note p _wh

ha :: DurC p 16
ha = \p -> Note p _ha

qu :: DurC p 8
qu = \p -> Note p _qu

ei :: DurC p 4
ei = \p -> Note p _ei

si :: DurC p 2
si = \p -> Note p _si

th :: DurC p 1
th = \p -> Note p _th
