{-# LANGUAGE TypeInType, TypeApplications, TemplateHaskell #-}

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
-- Literals for pitches, notes, durations, chords, etc.
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

-- ** Pitches

-- *** Constructor

-- | Create a new pitch with the given class, accidental and octave.
pitch :: PC pc -> Acc acc -> Oct oct -> Pit (Pitch pc acc oct)
pitch pc acc oct = Pit

-- *** Literals
mkPitchLits

