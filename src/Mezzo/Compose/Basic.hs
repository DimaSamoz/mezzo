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
import Mezzo.Compose.Templates

-- * Atomic literals

-- ** Pitch class literals
pitchClassLits

-- ** Accidental literals
accidentalLits

-- ** Octave literals
octaveLits

-- ** Duration literals
_wh :: Dur 32
_wh = Dur @32

_ha :: Dur 16
_ha = Dur @16

_qu :: Dur 8
_qu = Dur @8

_ei :: Dur 4
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

-- ** Concrete literals
mkPitchLits

-- ** Combinatorial literals (admitting continuations)
mkPitchCombs

-- * Notes

-- ** Constructor
note :: Pit p -> Dur d -> Music (FromPitch p d)
note p d = Note p d

-- ** Duration continuations

type DurC d = forall p. Pit p -> Music (FromPitch p d)

wh :: DurC 32
wh = \p -> Note p _wh

ha :: DurC 16
ha = \p -> Note p _ha

qu :: DurC 8
qu = \p -> Note p _qu

ei :: DurC 4
ei = \p -> Note p _ei

si :: DurC 2
si = \p -> Note p _si

th :: DurC 1
th = \p -> Note p _th
